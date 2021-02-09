#' @noRd
process_nested_count_target <- function(x) {

  evalq({

    if(is.null(indentation)) indentation <- "   "



    assert_that(quo_is_symbol(target_var[[2]]),
                msg = "Inner layers must be data driven variables")

    if(quo_is_symbol(target_var[[1]])){
      first_var_length <- length(unique(target[[as_name(target_var[[1]])]]))
      second_var_length <- length(unique(target[[as_name(target_var[[2]])]]))

      assert_that(second_var_length >= first_var_length,
                  msg = "The number of values of your second variable must be greater than the number of levels in your first variable")
    }

    if(is.factor(target[[as_name(target_var[[1]])]])) {
      warning(paste0("Factors are not currently supported in nested count layers",
                     " that have two data driven variables. Factors will be coerced into character vectors"),
              immediate. = TRUE)
    }
    if(is.factor(target[[as_name(target_var[[2]])]]) && quo_is_symbol(target_var[[1]])) {
      warning(paste0("Factors are not currently supported in nested count layers",
                     " that have two data driven variables. Factors will be coerced into character vectors"),
              immediate. = TRUE)
    }

    if (!is.null(denoms_by)) {
      change_denom_ind <- map_chr(denoms_by, as_name) %in% "summary_var"
      second_denoms_by <- denoms_by
      second_denoms_by[change_denom_ind] <- quos(!!target_var[[1]])
    } else {
      denoms_by <- c(treat_var, cols)
      second_denoms_by <- denoms_by
    }

    first_layer <- process_summaries(group_count(current_env(), target_var = !!target_var[[1]],
                                                 by = vars(!!!by), where = !!where))

    second_layer <- process_summaries(group_count(current_env(), target_var = !!target_var[[2]],
                                                  by = vars(!!target_var[[1]], !!!by), where = !!where) %>%
                                        set_count_row_prefix(indentation) %>%
                                        set_denoms_by(!!!second_denoms_by))

    first_layer_final <- first_layer$numeric_data
    # add_column(!!target_var[[1]] := .[["summary_var"]])

    second_layer_final <- second_layer$numeric_data %>%
      group_by(!!target_var[[1]]) %>%
      do(filter_nested_inner_layer(., target, target_var[[1]], target_var[[2]], indentation))

    # Bind the numeric data together
    numeric_data <- bind_rows(first_layer_final, second_layer_final)

    # Save the original by and target_vars incase the layer is rebuilt
    by_saved <- by
    target_var_saved <- target_var
    is_built_nest <- TRUE

    by <- vars(!!target_var[[1]], !!!by)
    target_var <- vars(!!target_var[[2]])


  }, envir = x)

}

#' This function is meant to remove the values of an inner layer that don't
#' appear in the target data
#' @noRd
filter_nested_inner_layer <- function(.group, target, outer_name, inner_name, indentation) {

  # Is outer variable text? If it is don't filter on it
  text_outer <- !quo_is_symbol(outer_name)
  outer_name <- as_name(outer_name)
  inner_name <- as_name(inner_name)

  if(text_outer) {
    lvs <- levels(target[[inner_name]])
    target_inner_values <- target %>%
      select(inner_name) %>%
      unlist() %>%
      c(lvs) %>%
      unique() %>%
      paste0(indentation, .)

  } else {
    current_outer_value <- unique(.group[, outer_name])[[1]]

    target_inner_values <- target %>%
      filter(!!sym(outer_name) == current_outer_value) %>%
      select(inner_name) %>%
      unlist() %>%
      paste0(indentation, .)
  }

  .group %>%
    filter(summary_var %in% target_inner_values)

}

#' This function resets the variables for a nested layer after it was built
#' @noRd
refresh_nest <- function(x) {
  env_bind(x, by = env_get(x, "by_saved"))
  env_bind(x, target_var = env_get(x, "target_var_saved"))
}
