
#' @export
process_summaries.shift_layer <- function(x, ...) {

  evalq({

    assert_that(all(names(target_var) %in% c("row", "column")),
                all(c("row", "column") %in% names(target_var)),
                msg = "target_vars passed to a shift layer must be named.")

  }, envir = x)

  process_shift_denoms(x)

  # Create the table used for denoms
  process_shift_n(x)

  prepare_shift_format_metadata(x)
}

#' @export
process_shift_n <- function(x) {

  evalq({
    numeric_data <- built_target %>%
      # Filter out based on where
      filter(!!where) %>%
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

#' @export
process_shift_total <- function(x) {

  evalq({
    if(is.null(denom_by)) denom_by <- c(treat_var, cols)

    numeric_data %<>%
      group_by(!!!denom_by) %>%
      do(get_shift_total(., denom_by, denoms_df))

  }, envir = x)
}

#' @export
prepare_shift_format_metadata <- function(x) {

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
process_formatting.shift_layer <- function(x, ...) {

  evalq({
    formatted_data <- numeric_data %>%
      # Mutate value based on if there is a distinct_by
      mutate(n = construct_shift_string(.n=n,
                                        shift_fmt=format_strings,
                                        max_layer_length=max_layer_length,
                                        max_n_width=max_n_width)) %>%
      # Pivot table
      pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                  names_from = c(!!treat_var, match_exact(cols), !!target_var$column),
                  values_from = n,
                  names_prefix = "var1_") %>%
      replace_by_string_names(quos(!!!by, summary_var))
  }, envir = x)

  add_order_columns(x)

  env_get(x, "formatted_data")

}

#' @export
construct_shift_string <- function(.n, shift_fmt, max_layer_length, max_n_width) {

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

  string_ <- pad_formatted_data(string_, max_layer_length, max_n_width)

  string_
}

#' @export
construct_count_string <- function(.n, .total, .distinct_n = NULL, .distinct_total = NULL,
                                   count_fmt = NULL, max_layer_length, max_n_width) {

  ## Added this for processing formatting in nested count layers where this won't be processed yet
  if (is.null(max_layer_length)) max_layer_length <- 0
  if (is.null(max_n_width)) max_n_width <- 0

  vars_ord <- map_chr(count_fmt$vars, as_name)

  # str_all is a list that contains character vectors for each parameter that might be calculated
  str_all <- vector("list", 5)
  # Append the repl_str to be passed to do.call
  str_all[1] <- count_fmt$repl_str
  # Iterate over every variable
  for(i in seq_along(vars_ord)) {
    str_all[[i+1]] <-  count_string_switch_help(vars_ord[i], count_fmt, .n, .total,
                                                .distinct_n, .distinct_total, vars_ord)
  }

  # Put the vector strings together. Only include parts of str_all that aren't null
  string_ <- do.call(sprintf, str_all[!map_lgl(str_all, is.null)])

  string_ <- pad_formatted_data(string_, max_layer_length, max_n_width)

  string_
}

#' @export
process_shift_denoms <- function(x) {

  evalq({

    denoms_df <- built_target %>%
      group_by(!!!target_var, !!treat_var, !!!by, !!!cols) %>%
      summarize(n = n())

  }, envir = x)

}
