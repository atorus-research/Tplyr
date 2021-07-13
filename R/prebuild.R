

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

    # Make sure factors are preserved
      built_target <- built_target %>%
        mutate(!!treat_var := fct_expand(as.character(!!treat_var),
                                         as.character(unique(target[[as_name(treat_var)]])),
                                         levels(target[[as_name(treat_var)]])))
      # mutate(!!treat_var := fct_expand(.[, as_name(treat_var)], unique(target[,as_name(treat_var)])))

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

      built_pop_data <- built_pop_data %>%
        mutate(!!pop_treat_var := fct_expand(as.character(!!pop_treat_var),
                                             as.character(unique(pop_data[[as_name(pop_treat_var)]])),
                                             levels(pop_data[[as_name(pop_treat_var)]])))

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
    if(!is.factor(built_pop_data[[as_label(pop_treat_var)]])){
      built_pop_data[[as_label(pop_treat_var)]] <- factor(built_pop_data[[as_label(pop_treat_var)]])
    }

    # Convert the target data treatment variable to a factor and force the levels from
    # the population data target variable in
    if(!is.factor(built_target[[as_label(treat_var)]])){
      built_target[[as_label(treat_var)]] <- factor(built_target[[as_label(treat_var)]],
                                                    levels = levels(built_pop_data[[as_label(pop_treat_var)]]))
    }


    rm(grp_i, i)
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
