

#' Build treatment groups into tables
#'
#' @param table A tplyr_table object to have its groups built.
#'
#' @return The table invisibly
#' @noRd
treatment_group_build <- function(table) {
  output <- evalq({
    # Dummies for treatment groups added to target dataset
    built_target <- target %>%
      filter(!!table_where) %>%
      mutate(!!treat_var := as.character(!!treat_var))
    built_pop_data <- pop_data %>%
      filter(!!table_where) %>%
      mutate(!!pop_treat_var := as.character(!!pop_treat_var))
    for (grp_i in seq(along = treat_grps)) {
      built_target <- built_target %>%
        filter(!!treat_var %in% treat_grps[[grp_i]]) %>%
        mutate(!!treat_var := names(treat_grps)[grp_i]) %>%
        bind_rows(built_target)
    }
    # Dummies for treatment groups added to population dataset
    for (grp_i in seq(along = treat_grps)) {
      built_pop_data <- built_pop_data %>%
        filter(!!treat_var %in% treat_grps[[grp_i]]) %>%
        mutate(!!treat_var := names(treat_grps)[grp_i]) %>%
        bind_rows(built_pop_data)
    }
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
