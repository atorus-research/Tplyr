### Logging Functions


#' Toggle 'Tplyr' debug messaging
#'
#' \strong{NOTE:} This feature is being questioned and is not currently being utilized.
#'
#' @param status TRUE/FALSE - Turns status messaging on or off
#'
#'
#' @return Nothing
#' @export
#'
#' @examples
#' tplyr_debug(TRUE)
tplyr_debug <- function(status) {
  assert_that(is.logical(status))

  options('tplyr.debug' = status)
}

#' Produce a debug message in Tplyr code
#'
#' @param message The debugging message to be printed
#' @param ... Additional parameters passed to \code{rlang::inform}
#'
#' @return Nothing
#' @noRd
#'
#' @examples
#' tplyr_debug(TRUE)
#' dmessage('This is will print!')
#'
#' tplyr_debug(FALSE)
#' dmessage('This will not...')
dmessage <- function(message, ...) {
  if (getOption('tplyr.debug')) inform(paste("DEBUG:", message), ...)
}
