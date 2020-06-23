

treatment_group_build <- function(table) {
  output <- evalq({
    # Dummies for treatment groups added to target dataset
    built_target <- target
    built_pop_data <- pop_data
    for (grp in seq(along = treat_grps)) {
      built_target <- built_target %>%
        filter(!!treat_var %in% treat_grps[[grp]]) %>%
        mutate(!!treat_var := names(treat_grps)[grp]) %>%
        bind_rows(built_target)
    }
    # Dummies for treatment groups added to population dataset
    for (grp in seq(along = treat_grps)) {
      built_pop_data <- built_pop_data %>%
        filter(!!treat_var %in% treat_grps[[grp]]) %>%
        mutate(!!treat_var := names(treat_grps)[grp]) %>%
        bind_rows(built_pop_data)
    }
    rm(grp)
  }, envir=table)
}

verify_layer_compatibility <- function(layer) {
  NextMethod("verify_layer_compatibility")
}

verify_layer_compatibility.count_layer <- function(layer){

  evalq({

  }, envir = layer)
  return(invisible(layer))
}
