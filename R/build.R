### Build/Render Functions

#' Trigger the execution of the \code{tplyr_table}
#'
#' @description
#' The functions used to assemble a \code{tplyr_table} object and each of the layers do not trigger the processing of any data. Rather, a lazy
#' execution style is used to allow you to contruct your table and then explicitly state when the data processing should happen.
#' \code{build} triggers this event.
#'
#' @details
#' When the \code{build} command is executed, all of the data processing commences. Any preprocessing necessary within the table environment
#' takes place first. Next, each of the layers begins executing. Once the layers complete executing, the output of each layer is stacked
#' into the resulting data frame.
#'
#' Once this process is complete, any post-processing necessary within the table environment takes place, and the final output can be
#' delivered. Metadata and traceability information are kept within each of the layer environments, which allows an investigation into the
#' source of the resulting datapoints. For example, numeric data from any summaries performed is maintained and accessible within
#' a layer using \code{\link{get_numeric_data}}.
#'
#' In future releases, Tplyr will have options to examine consistency across layers and make formatting
#' decisions for consistency (i.e. decimal alignment).
#'
#' @param x A \code{tplyr_table} object
#'
#' @return An executed \code{tplyr_table}
#' @export
#'
#' @examples
#' # Load in Pipe
#' library(magrittr)
#'
#' tplyr_table(iris, Species) %>%
#'   add_layer(
#'     group_desc(Sepal.Length, by = "Sepal Length") %>%
#'       set_format_strings(
#'         "n"        = f_str("xx", n),
#'         "Mean (SD)"= f_str("xx.x", mean),
#'         "SD"       = f_str("xx.xx", sd),
#'         "Median"   = f_str("xx.x", median),
#'         "Q1, Q3"   = f_str("xx, xx", q1, q3),
#'         "Min, Max" = f_str("xx, xx", min, max),
#'         "Missing"  = f_str("xx", missing)
#'       )
#'   ) %>%
#'     add_layer(
#'       group_desc(Sepal.Width, by = "Sepal Width")%>%
#'         set_format_strings(
#'           "n"        = f_str("xx", n),
#'           "Mean (SD)"= f_str("xx.x", mean),
#'           "SD"       = f_str("xx.xx", sd),
#'           "Median"   = f_str("xx.x", median),
#'          "Q1, Q3"   = f_str("xx, xx", q1, q3),
#'           "Min, Max" = f_str("xx, xx", min, max),
#'           "Missing"  = f_str("xx", missing)
#'         )
#'     ) %>%
#'     build()
#'
#' @seealso tplyr_table, tplyr_layer, add_layer, add_layers, layer_constructors
build <- function(x) {
  UseMethod("build")
}

#' tplyr_table S3 method
#' @noRd
#' @export
build.tplyr_table <- function(x) {

  # Table Pre build
  treatment_group_build(x)

  x <- build_header_n(x)

  # Process Layer summaries
  map(x$layers, process_summaries)

  # Get table formatting info
  formatting_meta <- fetch_formatting_info(x)

  # Format layers/table and pivot. process_formatting should return the built table!
  output <- map(x$layers, process_formatting) %>%
    bind_rows() %>%
    select(starts_with('row_label'), starts_with('var'), everything())

  # Rearange columns. Currently just alphabetical
  output
}

#' Process layers to get numeric results of layer
#'
#' This is an internal method, but is exported to support S3 dispatch. Not intended for direct use by a user.
#' @param x a tplyr_layer object
#' @param ... arguments passed to dispatch
#'
#' @return The tplyr_layer object with a 'built_table' binding
#' @export
process_summaries <- function(x, ...) {
  UseMethod("process_summaries")
}

#' Process layers to get formatted and pivoted tables.
#'
#' This is an internal method, but is exported to support S3 dispatch. Not intended for direct use by a user.
#' @param x A tplyr_layer object
#' @param ... arguments passed to dispatch
#'
#' @return The formatted_table object that is binded to the layer
#' @export
process_formatting <- function(x, ...) {
  UseMethod("process_formatting")
}


#' Placeholder function to fetch table formatting data from layers
#'
#' @param x A tplyr_table object
#'
#' @return The data used to format layers. Structure currently TBD
fetch_formatting_info <- function(x) {

  # Get the max length of f_str objects in sub_layers
  max_layer_length <- max(map_int(x$layers, ~ env_get(.x, "max_length")))
  # Get the max length of n counts only, not including f_str formatting
  max_n_width <- max(map_dbl(x$layers, ~ ifelse(inherits(.x, 'count_layer'), .x$n_width, 0L)))

  env_bind(x, max_layer_length = max_layer_length)
  env_bind(x, max_n_width = max_n_width)
}


######################################################################################
######## Saving the below for posterity but I don't think its needed anymore #########
######################################################################################


#' #' count_layer S3 method
#' #' @noRd
#' build.count_layer <- function(x) {
#'
#'   target_var_length <- length(env_get(x, "target_var"))
#'
#'     if(target_var_length == 2) {
#'
#'
#'
#'       # Begin with the layer itself and process the first target vars values one by one
#'       layer_output <- map_dfr(unlist(get_target_levels(x, env_get(x, "target_var")[[1]])),
#'                               bind_nested_count_layer, x = x)
#'
#'       # Build the sub-layers
#'       sublayer_output <- map(x$layers, build)
#'
#'       # Feed the output up
#'       #layer_output <- process_count_layer(x)
#'
#'       # TODO: Some combination process
#'       output <- layer_output
#'       output
#'
#'       # If there are not two, and not one, fail. TODO: move this into compatibility
#'     } else if (target_var_length != 1) {
#'       abort("target_var can only contain one or two target_variables.
#'             Other amounts are not currently implemented.")
#'
#'       # If there is just one no need for logic
#'     } else {
#'
#'       # Build the sub-layers
#'       sublayer_output <- map(x$layers, build)
#'
#'       # Feed the output up
#'       layer_output <- process_count_layer(x)
#'
#'       # TODO: Some combination process
#'       output <- layer_output
#'       output
#'     }
#' }
#'
#' #' desc_layer S3 method
#' #' @noRd
#' #' @export
#' build.desc_layer <- function(x) {
#'
#'   # Build the sub-layers
#'   sublayer_output <- map(x$layers, build)
#'
#'   # Feed the output up
#'   layer_output <- process_desc_layer(x)
#'
#'   # TODO: Some combination process
#'   output <- layer_output
#'   output
#' }
#'
#' #' shift_layer S3 method
#' #' @noRd
#' build.shift_layer <- function(x) {
#'
#'   # Build the sub-layers
#'   sublayer_output <- map(x$layers, build)
#'
#'   # Feed the output up
#'   layer_output <- process_shift_layer(x)
#'
#'   # TODO: Some combination process
#'   output <- layer_output
#'   output
#' }





