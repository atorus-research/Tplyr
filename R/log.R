### Logging Functions

tplyr_debug <- function(status) {
  assert_that(is.logical(status))

  options('tplyr_debug' = status)
}

dmessage <- function(message, ...) {
  if (getOption('tplyr_debug')) inform(paste("DEBUG:", message), ...)
}
