#' Process a layer of type \code{group_count}
#'
#' @param e A layer environment
#'
#' @return A
#' @export
#'
#' @examples
process_count_layer <- function(e) {
  evalq({

    if(!exists("count_fmt")) count_fmt <- f_str("ax (xxx.x%)", n, pct)
    if(!exists("include_total_row")) include_total_row <- TRUE

    # Construct the counts for each target grouping
    summary_stat <- built_target %>%
      # Filter out based on where
      filter(!!where) %>%
      # Group by varaibles including target variables and count them
      group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
      tally() %>%
      ungroup() %>%
      # Group by all column variables
      group_by(!!treat_var, !!!cols) %>%
      add_tally(name = "Total", wt = n) %>%
      ungroup() %>%
      # complete all combiniations of factors to include combiniations that don't exist.
      # add 0 for combintions that don't exist
      complete(!!treat_var, !!!by, !!!target_var, !!!cols, fill = list(n = 0, Total = 0))

    # If there is no values in summary_stat, which can happen depending on where. Return nothing
    if(nrow(summary_stat) == 0) return()

    #
    total_stat <- NULL
    if(include_total_row) {
      # create a data.frame to create total counts
      total_stat <- built_target %>%
        # filter out based on where
        filter(!!where) %>%
        # Group by all column variables
        group_by(!!treat_var, !!!cols) %>%
        tally() %>%
        ungroup() %>%
        mutate(Total = n) %>%
        # complete based on missing groupings
        complete(!!treat_var, !!!cols, fill = list(n = 0, Total = 0))
    }


    # rbind tables together
      built_table <- summary_stat %>%
        bind_rows(total_stat) %>%
        mutate(n = construct_count_string(n, Total, count_fmt)) %>%
        # Pivot table
        pivot_wider(id_cols = c(match_exact(by), match_exact(target_var)),
                    names_from = c(!!treat_var, match_exact(cols)), values_from = n,
                    names_prefix = "var1_1") %>%
        # Replace String names for by and target variables. target variables are included becasue they are
        # equivilant to by variables in a count layer
        replace_by_string_names(c(by, target_var)) %>%
        apply_row_masks()

  }, envir = e)
}

#' Set Count Layer String Format
#'
#' @param x the layer object to add/modify the count format
#' @param str The f_str object to add
#'
#' @return
#' @export
#'
#' @examples
set_count_fmt <- function(x, str) {
  assert_inherits_class(x, "count_layer")

  assert_has_class(str, "f_str")

  assert_that(all(str$vars %in% c("n", "pct")),
              "f_str in a count_layer can only be n or pct")

  env_bind(x, count_fmt = str)

  x
}

#' Format n counts for display in count_layer
#'
#' @param .n Vector of counts for each cell
#' @param .total  Vector of totals. Should be the same length as .n and be the
#'   denominator that column is based off of.
#' @param count_fmt The f_str object the strings are formatted around.
#'
#' @return A tibble replacing the originial counts
construct_count_string <- function(.n, .total, count_fmt = NULL) {

  # If a layer_width flag is present, edit the formatting string to display the maximum
  # character length
  if(str_detect(count_fmt$format_string, "ax")) {
    # Pull max character length from counts
    max_width <- max(nchar(.n))

    # Replace the flag with however many xs
    count_fmt_str <- str_replace(count_fmt$format_string, "ax",
                             paste(rep("x", max_width), collapse = ""))

    # Make a new f_str and replace the old one
    count_fmt <- f_str(count_fmt_str, n, pct)
  }

  # Make a vector of ncounts
  str1 <- map_chr(.n, num_fmt, 1, fmt = count_fmt)
  # Makea vector of ratios between n and total. Replace na values with 0
  pcts <- replace(.n/.total, is.na(.n/.total), 0)
  # Make a vector of percentages
  str2 <- map_chr(pcts*100, num_fmt, 2, fmt = count_fmt)

  # Put the vector strings together
  sprintf(count_fmt$repl_str, str1, str2)

}


#' Set the include_total_row option for count processing
#'
#' @param x A layer object
#' @param include_total A logical vector
set_include_total_row <- function(x, include_total) {
  assert_inherits_class(x, "count_layer")

  assert_that(is.logical(include_total))

  env_bind(x, include_total_row = include_total)

  x
}
