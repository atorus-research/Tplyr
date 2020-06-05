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
