
#' @export
process_summaries.shift_layer <- function(x, ...) {

  evalq({

    assert_that(all(names(target_var) %in% c("row", "column")),
                all(c("row", "column") %in% names(target_var)),
                msg = "target_vars passed to a shift layer must be named.")


    if(is.null(format_strings)) format_strings <- gather_defaults(environment())[[1]]

    # Subset the local built_target based on where
    # Catch errors
    # Puting this here to make clear it happens up-front in the layer
    tryCatch({
      #save the built target before thw where so it can be processed in the denominator
      built_target_pre_where <- built_target
      built_target <- built_target %>%
        filter(!!where)
    }, error = function(e) {
      abort(paste0("group_shift `where` condition `",
                   as_label(where),
                   "` is invalid. Filter error:\n", e))
    })

  }, envir = x)

  process_shift_denoms(x)

  # Create the table used for denoms
  process_shift_n(x)

  prepare_format_metadata(x)
}

#' @noRd
process_shift_n <- function(x) {

  evalq({

    numeric_data <- built_target %>%
      # Group by variables including target variables and count them
      group_by(!!treat_var, !!!by, !!!unname(target_var), !!!cols) %>%
      tally(name = "n") %>%
      ungroup() %>%
      # complete all combinations of factors to include combinations that don't exist.
      # add 0 for combinations that don't exist
      complete(!!treat_var, !!!by, !!!unname(target_var), !!!cols, fill = list(n = 0)) %>%
      # Change the treat_var and first target_var to characters to resolve any
      # issues if there are total rows and the original column is numeric
      mutate(!!treat_var := as.character(!!treat_var)) %>%
      mutate(!!as_label(target_var$row) := as.character(!!target_var$row)) %>%
      # Rename the row target to summary_var
      rename("summary_var" := !!target_var$row)

    # If there is no values in summary_stat, which can happen depending on where. Return nothing
    if(nrow(numeric_data) == 0) return()

    if("pct" %in% format_strings$vars) process_shift_total(current_env())
  }, envir = x)

}

#' @noRd
process_shift_total <- function(x) {

  evalq({
    if(is.null(denoms_by)) denoms_by <- c(treat_var, by, cols)

    numeric_data <- numeric_data %>%
      group_by(!!!denoms_by) %>%
      do(get_denom_total(., denoms_by, denoms_df))

  }, envir = x)
}

#' @noRd
prepare_format_metadata.shift_layer <- function(x) {

  evalq({


    # Pull max character length from counts. Should be at least 1
    n_width <- max(c(nchar(numeric_data$n), 1L))

    # If a layer_width flag is present, edit the formatting string to display the maximum
    # character length
    if(str_detect(format_strings$format_string, "a")) {
      # Replace the flag with however many xs
      replaced_string <- str_replace(format_strings$format_string, "a",
                                     paste(rep("x", n_width), collapse = ""))

      # Make a new f_str and replace the old one
      format_strings <- f_str(replaced_string, !!!format_strings$vars)
    }
    max_length <- format_strings$size
  }, envir = x)

}

#' @export
#' @keywords internal
process_formatting.shift_layer <- function(x, ...) {

  evalq({
    formatted_data <- numeric_data %>%
      # Mutate value based on if there is a distinct_by
      mutate(n = construct_shift_string(.n=n, .total = total,
                                        shift_fmt=format_strings,
                                        max_layer_length=max_layer_length,
                                        max_n_width=max_n_width)) %>%
      # Pivot table
      pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                  names_from = c( !!treat_var, !!target_var$column, match_exact(cols)),
                  values_from = n,
                  names_prefix = "var1_") %>%
      replace_by_string_names(quos(!!!by, summary_var))
  }, envir = x)

  add_order_columns(x)

  env_get(x, "formatted_data")

}

#' @noRd
#'
#' @param .n counts
#' @param .total The value used in the denominator of the pct
#' @param shift_fmt The f_str object used to display the string
#' @param max_layer_length The maximum character length in the table
#' @param max_n_width The maximum count length in the table.
construct_shift_string <- function(.n, .total, shift_fmt, max_layer_length, max_n_width) {

  vars_ord <- map_chr(shift_fmt$vars, as_name)

  # str_all is a list that contains character vectors for each parameter that might be calculated
  str_all <- vector("list", 5)
  # Append the repl_str to be passed to do.call
  str_all[1] <- shift_fmt$repl_str
  # Iterate over every variable
  for(i in seq_along(vars_ord)) {
    str_all[[i+1]] <-  count_string_switch_help(vars_ord[i], shift_fmt, .n, .total,
                                                .distinct_n, .distinct_total, vars_ord)
  }

  # Put the vector strings together. Only include parts of str_all that aren't null
  string_ <- do.call(sprintf, str_all[!map_lgl(str_all, is.null)])

  string_ <- pad_formatted_data(string_, 0, max_n_width)

  string_
}

#' @noRd
#' @param x The layer object
#'
#' This creates the `denoms_df` object that contains the counts of the combinations
#' of the layer and table parameters
process_shift_denoms <- function(x) {

  evalq({

    if(is.null(denom_where)) denom_where <- where

    denoms_df <- built_target_pre_where %>%
      filter(!!denom_where) %>%
      group_by(!!!unname(target_var), !!treat_var, !!!by, !!!cols) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      complete(!!!unname(target_var), !!treat_var, !!!by, !!!cols) %>%
      # The rows will duplicate for some reason so this removes that
      distinct() %>%
      rename("summary_var" := !!target_var$row)

  }, envir = x)

}
