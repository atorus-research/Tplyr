
#' Assert that an argument is of a certain class
#'
#' @param x
#' @param should_be
#'
#' @return
#' @export
#'
#' @examples
assert_has_class <- function(x, should_be) {
  # Get the name of the parameter being checked
  param <- quo_get_expr(enquo(x))

  # Is the argument the class that it shoudl be?
  if (class(x) != should_be){
    # Grab the trace back into an object
    trc <- trace_back()
    # Look at the length of the traceback
    max_length <- max(trc$indices)
    # If it's >1 we're innside a function, so grab the name
    if (max_length > 1){
      # Pull the name out of the call stack
      cname <- call_name(trc$calls[[max_length - 1]])
      # Make a display string
      func_str <- paste0('` in function `', cname, '`')
    } else {
      # Filler
      func_str <- '`'
    }
    # Abort and show error
    abort(paste0('Argument `', param, func_str, ' must be ',
                 should_be, '. Instead a class of "', class(x),
                 '" was passed.'))
  }
}

#' Assert that an argument inherits certain class
#'
#' @param x
#' @param should_have
#'
#' @return
#' @export
#'
#' @examples
assert_inherits_class <- function(x, should_have) {
  # Get the name of the parameter being checked
  param <- quo_get_expr(enquo(x))

  # Is the argument the class that it shoudl be?
  if (!inherits(x, should_have)){

    # Grab the trace back into an object
    trc <- trace_back()
    # Look at the length of the traceback
    max_length <- max(trc$indices)
    # If it's >1 we're innside a function, so grab the name
    if (max_length > 1){
      # Pull the name out of the call stack
      cname <- call_name(trc$calls[[max_length - 1]])
      # Make a display string
      func_str <- paste0('` in function `', cname, '`')
    } else {
      # Filler
      func_str <- '`'
    }
    # Abort and show error
    abort(paste0('Argument `', param, func_str,
                 ' does not inherit "', should_have,
                 '". Classes: ', paste(class(x), collapse=", ")))
  }
}
