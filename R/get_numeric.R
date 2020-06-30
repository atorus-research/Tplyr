#' Retrieve the numeric data from a tplyr objects
#'
#' It may be desirable to access the numeric data from a tplyr object directly, and these objects provide
#' a method of accessing that directly.
#'
#' When used on a \code{tplyr_table} object, this method will aggregate the numeric data from all Tplyr layers. The data
#' are combined into a single data frame and returned to the user. If the data has already been processed (i.e. \code{build} has
#' been run), the numeric data is already available and will simply be returned. Otherwise, the numeric portion of the layer will
#' be processed.
#'
#' @param x A tplyr_table or tplyr_layer object
#' @param ... Arguments to pass foward
#'
#' @return Numeric data from the Tplyr layer
#' @export
#'
#' @examples
#' #TBD
get_numeric_data <- function(x, ...) {
  UseMethod("get_numeric_data")
}


#' Get numeric data from a tplyr_table object
#' @export
#' @noRd
get_numeric_data.tplyr_table <- function(x, ...) {

  if (!exists('built_target', envir=x)) {
    treatment_group_build(x)
  }

  num_data <- map(x$layers, get_numeric_data)

  num_data

}


#' Get numeric data from a tplyr_layer object
#' @export
#' @noRd
get_numeric_data.tplyr_layer <- function(x, ...) {

  if (!exists('numeric_data', envir=x)) {
    process_summaries(x)
  }

  env_get(x, 'numeric_data')
}
