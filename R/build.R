### Build/Render Functions

#' Trigger the execution of the \code{tplyr_table}
#'
#' @description
#' The functions used to assemble a \code{tplyr_table} object and each of the layers do not trigger the processing of any data. Rather, a lazy
#' execution style is used to allow you to contruct your table, preview what the output should look like, and then explicitly state when the
#' data processing should happen. \code{build} triggers this event.
#'
#' @details
#' When the \code{build} command is executed, all of the data processing commences. Any preprocessing necessary within the table environment
#' takes place first. Next, each of the layers begings executing. This process is recursive, as when a layer is starts executing, \code{tplyr}
#' looks to see if there are any sublayers within that layer. Once the layers complete executing, within a layer, the provided instruction
#' will join them together, or within the table the outputs will be stacked together.
#'
#' Once this process is complete, any post-processing necessary within the table environment takes place, and the final output can be
#' delivered. Metadata and traceability information are kept within each of the layer environments, which allows an investigation into the
#' source of the resulting datapoints.
#'
#' @param x A \code{tplyr_table} object
#'
#'
#' @return An executed \code{tplyr_table}
#' @export
#'
#' @examples
#' # TBD
#'
#' @seealso tplyr_table, tplyr_layer, add_layer, layer_constructors
build <- function(x) {
  UseMethod("build")
}

#' tplyr_table S3 method
#' @noRd
#' @export
build.tplyr_table <- function(x) {

  treatment_group_build(x)

  output <- map(x$layers, build)

  # Feed the output up
  bind_rows(output)
}

#' count_layer S3 method
#' @noRd
build.count_layer <- function(x) {

  # Build the sub-layers
  sublayer_output <- map(x$layers, build)

  # Feed the output up
  layer_output <- process_count_layer(x)

  # TODO: Some combination process
  output <- layer_output
  output
}

#' desc_layer S3 method
#' @noRd
#' @export
build.desc_layer <- function(x) {

  # Build the sub-layers
  sublayer_output <- map(x$layers, build)

  # Feed the output up
  layer_output <- process_desc_layer(x)

  # TODO: Some combination process
  output <- layer_output
  output
}

#' shift_layer S3 method
#' @noRd
build.shift_layer <- function(x) {

  # Build the sub-layers
  sublayer_output <- map(x$layers, build)

  # Feed the output up
  layer_output <- process_shift_layer(x)

  # TODO: Some combination process
  output <- layer_output
  output
}





