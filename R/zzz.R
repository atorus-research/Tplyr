#TODO: Package documentation

# Default options ----
tplyr_default_options <- list(
  tplyr.debug = FALSE
)

# Carry out process on load ----
.onLoad <- function(libname, pkgname) {
  # store existing options
  op <- options()

  # Set any options that haven't been set
  toset <- !(names(tplyr_default_options) %in% names(op))
  if(any(toset)) options(tplyr_default_options[toset])

  invisible()
}
