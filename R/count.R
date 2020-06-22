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

    # Construct the counts for each target grouping
    summary_stat <- built_target %>%
      # Filter out based on where
      filter(!!where) %>%
      # Only pull out the needed variables
      select(!!treat_var, !!!target_var, !!!by, !!!cols) %>%
      # Group by varaibles including target variables
      count(!!treat_var, !!!target_var, !!!by, !!!cols) %>%
      # Group by all column variables
      add_count(!!treat_var, !!!cols, name = "Total", wt = n) %>%
      # complete all combiniations of factors to include combiniations that don't exist.
      # add 0 for combintions that don't exist
      complete(!!treat_var, !!!target_var, !!!by, !!!cols, fill = list(n = 0, Total = 0))

    # create a data.frame to create total counts
    total_stat <- built_target %>%
      # filter out based on where
      filter(!!where) %>%
      # Only pull out the needed variables
      select(!!treat_var, !!!target_var, !!!by, !!!cols) %>%
      # Group by all column variables
      count(!!treat_var, !!!cols) %>%
      mutate(Total = n) %>%
      # complete based on missing groupings
      complete(!!treat_var, !!!cols, fill = list(n = 0, Total = 0))

    # rbind tables together
      built_table <- summary_stat %>%
        bind_rows(total_stat) %>%
        mutate(n = construct_count_string(n, Total, count_fmt)) %>%
        # Pivot table
        pivot_wider(id_cols = c(match_exact(by), match_exact(target_var)), names_from = c(!!treat_var, match_exact(cols)), values_from = n)

  }, envir = e)
}

set_count_fmt <- function(x, str) {
  assert_has_class(x, "count_layer")

  assert_has_class(str, "f_str")

  count_fmt <- str
}

#' Format n counts for display in count_layer
#'
#' @param ns
#'
#' @return A tibble replacing the originial counts
construct_count_string <- function(.n, .total, count_fmt = NULL) {

  # If a layer_width flag is present, edit the formatting string to display the maximum
  # character length
  if(str_detect(count_fmt$format_string, "\\{layer_width\\(x\\)\\}")) {
    # Pull max character length from counts
    max_width <- max(nchar(.n))

    # Replace the flag with however many xs
    count_fmt_str <- str_replace(count_fmt$format_string, "\\{layer_width\\(x\\)\\}",
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

