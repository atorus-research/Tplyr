#' Retrieve the numeric data from a tplyr objects
#'
#' It may be desirable to access the numeric data from a tplyr object directly, and these objects provide
#' a method of accessing that directly.
#'
#' When used on a \code{tplyr_table} object, this method will aggregate the numeric data from all Tplyr layers. The data will be
#' returned to the user in a list of dataframes. If the data has already been processed (i.e. \code{build} has
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
#' # Load in pipe
#' library(magrittr)
#'
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl)
#'   ) %>%
#'   add_layer(
#'     group_desc(mpg)
#'   ) %>%
#'   get_numeric_data()
#'
get_numeric_data <- function(x, ...) {
  UseMethod("get_numeric_data")
}


#' Get numeric data from a tplyr_table object
#' @export
#' @noRd
get_numeric_data.tplyr_table <- function(x, ...) {

  # If the pre-build wasn't executed then execute it
  if (!'built_target' %in% ls(x)) {
    treatment_group_build(x)
    build_header_n(x)
  }

  # Gather the numeric data from each layer
  num_data <- map(x$layers, get_numeric_data)

  num_data

}


#' Get numeric data from a tplyr_layer object
#' @export
#' @noRd
get_numeric_data.tplyr_layer <- function(x, ...) {

  # If the numeric data doesn't exist in the layer then process it
  if (!'numeric_data' %in% ls(x)) {
    process_summaries(x)
  }

  # Return the object
  env_get(x, 'numeric_data')
}
