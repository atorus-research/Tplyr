

#' Build treatment groups into tables
#'
#' @param table A tplyr_table object to have its groups built.
#'
#' @return The table invisibly
#' @noRd
treatment_group_build <- function(table) {
  output <- evalq({

    # Make built_target a copy of target
    built_target <- clean_attr(target)
    built_pop_data <- clean_attr(pop_data)

    # Dummies for treatment groups added to target dataset
    built_target <- built_target %>%
      filter(!!table_where) %>%
      mutate(!!treat_var := as.character(!!treat_var))

    built_pop_data <- built_pop_data %>%
      filter(!!pop_where) %>%
      mutate(!!pop_treat_var := as.character(!!pop_treat_var))

    for (grp_i in seq_along(treat_grps)) {
      built_target <- built_target %>%
        filter(!!treat_var %in% treat_grps[[grp_i]]) %>%
        mutate(!!treat_var := names(treat_grps)[grp_i]) %>%
        bind_rows(built_target)
    }
    # Dummies for treatment groups added to population dataset
    for (grp_i in seq_along(treat_grps)) {
      built_pop_data <- built_pop_data %>%
        filter(!!pop_treat_var %in% treat_grps[[grp_i]]) %>%
        mutate(!!pop_treat_var := names(treat_grps)[grp_i]) %>%
        bind_rows(built_pop_data)
    }

    # Convert the pop data treatment variable to a factor
    built_pop_data[[as_label(pop_treat_var)]] <- factor(built_pop_data[[as_label(pop_treat_var)]])

    # Convert the target data treatment variable to a factor and force the levels from
    # the population data target variable in
    built_target[[as_label(treat_var)]] <- factor(built_target[[as_label(treat_var)]],
                                                  levels = levels(built_pop_data[[as_label(pop_treat_var)]]))

    rm(grp_i)
  }, envir=table)

  invisible(table)
}

#' Verify layer is compatible with the layer it is contained in
#'
#' Currently a place holder. Will add more checks in further in development
#'
#' @noRd
verify_layer_compatibility <- function(layer) {
  NextMethod("verify_layer_compatibility")
}

#' @noRd
verify_layer_compatibility.count_layer <- function(layer){

  evalq({

  }, envir = layer)
  return(invisible(layer))
}
