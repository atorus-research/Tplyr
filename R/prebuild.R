

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

    if (!is.factor(target[[as_name(treat_var)]])) {
      built_target <- built_target %>%
        mutate(
          !!treat_var := factor(!!treat_var)
        )
    }

    built_pop_data <- clean_attr(pop_data)

    if (!is.factor(pop_data[[as_name(pop_treat_var)]])) {
      built_pop_data <- built_pop_data %>%
        mutate(
          !!pop_treat_var := factor(!!pop_treat_var)
        )
    }

    # Capture all source factor levels
    fct_levels <- unique(c(
      levels(built_pop_data[[as_name(pop_treat_var)]]),
      levels(built_target[[as_name(treat_var)]]),
      names(treat_grps)
      ))

    # Apply the filter and catch any filter errors, report
    # the issue to the user explicitly
    tryCatch({
      built_target <- built_target %>%
        filter(!!table_where)
    }, error = function(e) {
      abort(paste0("tplyr_table `where` condition `",
                   as_label(table_where),
                   "` is invalid. Filter error:\n", e))
    })

    # Same filter test on population data
    tryCatch({
      built_pop_data <- built_pop_data %>%
        filter(!!pop_where)
    }, error = function(e) {
      abort(paste0("Population data `pop_where` condition `",
                   as_label(pop_where),
                   "` is invalid. Filter error:\n", e,
                   "If the population data and target data subsets should be different, use `set_pop_where`."))
    })

    # Make sure all factors are preserved and where logic didn't take out any factors
    for(i in seq_along(cols)) {
      built_target <- built_target %>%
        mutate(!!cols[[i]] := fct_expand(as.character(!!cols[[i]]),
                                         as.character(unique(target[[as_name(cols[[i]])]])),
                                         levels(target[, as_name(cols[[i]])])))
      built_pop_data <- built_pop_data %>%
        mutate(!!cols[[i]] := fct_expand(as.character(!!cols[[i]]),
                                         as.character(unique(pop_data[[as_name(cols[[i]])]])),
                                         levels(pop_data[, as_name(cols[[i]])])))
    }

    # Levels are lost here
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

    # Make sure factors are preserved
    built_target <- built_target %>%
      mutate(!!treat_var := factor(!!treat_var, levels = fct_levels))

    built_pop_data <- built_pop_data %>%
      mutate(!!pop_treat_var := factor(!!pop_treat_var, levels = fct_levels))

    rm(grp_i, i, fct_levels)
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
