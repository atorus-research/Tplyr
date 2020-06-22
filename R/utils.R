### Utility Functions

#' Extract the top function from a nested call and insert desired arguments
#'
#' @param c An R expression
#' @param allowable_calls A character vector of function names allowed to be called within a piping sequence
#' @param ... Parameters to insert into topmost call
#'
#' @return The original call object with
#'
#' @examples
#' #TBD
#' # modify_nested_call(mean(c(1,2,3)) %>% print(), na.rm=TRUE)
modify_nested_call <- function(c, allowable_calls = getNamespaceExports("Tplyr"), ...) {
  # If the call is not from magrittr, then modify the contents and return the call
  if (call_name(c) != "%>%") {
    # Only allow the user to use `tplyr` functions
    if (!is.null(allowable_calls)) {
      assert_that(call_name(c) %in% allowable_calls, msg = "Functions called within `add_layer` must be part of `Tplyr`")
    }
    c <- call_modify(.call=c, ...)

  } else {
    if (!is.null(allowable_calls)) {
      # Only allow the user to use `tplyr` functions
      assert_that(all(sapply(call_args(c), call_name) %in% allowable_calls),
                  msg="Functions called within `add_layer` must be part of `Tplyr`")
    }

    # Recursively extract the left side of the magrittr call to work your way up
    e <- call_standardise(c)
    c <- modify_nested_call(call_args(e)$lhs, allowable_calls = allowable_calls, ...)
    # Modfify the magittr call by inserting the call retrieved from recursive command back in
    c <- call_modify(e, lhs=c)
    c
  }
}

#' Find depth of a layer object
#'
#' This function returns the number of containers "above" a layer object. As
#' layers can be nested layers may contain layers and so on. This uses
#' recursion to find the table environment
#'
#' @param layer A layer object
#' @param i The current index
#'
#' @return the number of containers a layer is in
depth_from_table <- function(layer, i){
  if(class(env_parent(layer))[1] == "tplyr_table") return(i + 1)
  else {
    return(depth_from_table(env_parent(layer), i+1))
  }
}


#' Convert a list of quosures to character strings
#'
#' Intended for use in a tidyselect context. Pivots take arguments as character strings or indices. Tidyselect tools return those
#' indices. This allows you to pass a list of quosures (which Tplyr carries a lot of) without explicitly converting types
#'
#' @param vars List of quosures containing variables
#'
#' @return Character string of labels
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   summarize(mean=mean(Sepal.Length), median = median(Sepal.Length)) %>%
#'   pivot_longer(cols = match_exact(vars(mean, median)))
#'
match_exact <- function(var_list) {
  # Should have been a list of quosures on input
  assert_inherits_class(var_list, "quosures")
  # Return the variable names as a character string in appropriate tidyselect format
  out <- map_chr(var_list, as_label)
  out[out != 'NULL'] # Exclude NULL quosures
}

#' Organize row labels within a layer output
#'
#' @param dat A data.frame/tibble to have row labels renamed
#' @param by The \code{by} object within a layer
#'
#' @return A tibble with renamed variables and row labels re-ordered to the front of the tibble
replace_by_string_names <- function(dat, by) {
  # By must be a list of quosures
  assert_that(is_quosures(by), msg = "`by` must be a list of quosures")

  i <- 0

  # If there were character strings in the by variables then rename them
  # with an index, starting at 1
  for (i in seq_along(by)) {
    dat <- rename(dat, !!paste0('row_label', i) := as_label(by[[i]]))
  }

  # If there was a column named `row_label` the index it
  if ('row_label' %in% names(dat)) {
    dat <- rename(dat, !!paste0('row_label', i + 1) := row_label)
  }

  # Sort the row labels by index
  row_labels <- names(dat)[str_detect(names(dat), 'row_label')]

  # Insert row labels to the front of the tibble
  select(dat, all_of(sort(row_labels)), everything())
}
