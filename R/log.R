### Logging Functions


#' Toggle Tplyr debug messaging
#'
#' @param status TRUE/FALSE - Turns status messaging on or off
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

#' Title
#'
#' @param message The debugging message to be printed
#' @param ... Additional parameters passed to \code{rlang::inform}
#'
#' @return Nothing
#' @export
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

# datinfo <- function(.data) {
#
# }
