#' Custom `tplyr` assertion to display if an improper class was passed into an object.
#'
#' @param x Object whose class should be checked
#' @param should_be The class the object is required to be
#' @param arg The function parameter being checked
#'
#' @return
#' @export
#'
#' @examples
is_class <- function(x, should_be) {
  class(x) == should_be
}

#' Title
#'
#' @param call
#' @param env
#'
#' @return
#' @export
#'
#' @examples
on_failure(is_class) <- function(call, env) {
  # Pick off the call object arguments to make them easier to access
  fargs <- call_args(call)
  val <- eval(fargs$x)

  # Look at the traceback information
  trc <- trace_back()
  # Look at the call stack and check how many frames are on the stack
  max_index = max(trc$indices)

  # A 5th frame means the assertion was raised inside a function
  if (max_index >= 5){
    # To get the calling function, look 4 frames back
    # `trace_back` orders the frames from top call first, so have to look backwards from the end
    # of the `calls` binding
    cname <- call_name(trc$calls[[max_index - 4]])
    # Make a print string out of it
    cname <- paste0('` in function `',cname, '`')
  } else {
    # If the assertion was raised outside of a function then just hold a back tick
    cname = '`'
  }

  # Print the error message containing the calling function name if it exists
  paste0('Argument `', fargs$x, cname, ' must be ', fargs$should_be,
         ". Instead a class of ", class(val)," was passed.")
}


