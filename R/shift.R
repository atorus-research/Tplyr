

process_summaries.shift_layer <- function(x, ...) {

  evalq({

    assert_that(all(names(target_var) %in% c("row", "column")),
                all(c("row", "column") %in% names(target_var)),
                msg = "target_vars passed to a shift layer must be named.")

    # Overwrite the cols in the table
    cols <<- vars(!!!cols, !!target_var$column)

    y <- process_summaries(group_count(parent = current_env(), target_var = !!target_var$row,
                                  by = vars(!!!by), where = !!where) %>%
                             set_format_strings(f_str("a", n)))

    numeric_data <- env_get(y, "numeric_data")
    format_strings <- env_get(y, "format_strings")
    n_width <- env_get(y, "n_width")
    max_length <- env_get(y, "max_length")

    target_var <- vars(!!target_var$row)
  }, envir = x)
}

process_formatting.shift_layer <- function(x, ...) {

  process_formatting.count_layer(x)
}
