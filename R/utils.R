### Utility Functions

#' Extract the top function from a nested call and insert desired arguments
#'
#' @param c An R expression
#' @param ... Parameters to insert into topmost call
#'
#' @return The original call object with
#'
#' @examples
modify_nested_call <- function(c, ...) {

  # If the call is not from magrittr, then modify the contents and return the call
  if (call_name(c) != "%>%") {
    c <- call_modify(.call=c, ...)
    c
  } else {
    # Recursively extract the left side of the magrittr call to work your way up
    e <- call_standardise(c)
    c <- modify_nested_call(call_args(e)$lhs, ...)
    # Modfify the magittr call by inserting the call retrieved from recursive command back in
    c <- call_modify(e, lhs=c)
    c
  }
}
