### Build/Render Functions

#' Trigger the execution of the \code{tplyr_table}
#'
#' @description The functions used to assemble a \code{tplyr_table} object and
#' each of the layers do not trigger the processing of any data. Rather, a lazy
#' execution style is used to allow you to construct your table and then
#' explicitly state when the data processing should happen. \code{build}
#' triggers this event.
#'
#' @details When the \code{build} command is executed, all of the data
#' processing commences. Any pre-processing necessary within the table
#' environment takes place first. Next, each of the layers begins executing.
#' Once the layers complete executing, the output of each layer is stacked into
#' the resulting data frame.
#'
#' Once this process is complete, any post-processing necessary within the table
#' environment takes place, and the final output can be delivered. Metadata and
#' traceability information are kept within each of the layer environments,
#' which allows an investigation into the source of the resulting datapoints.
#' For example, numeric data from any summaries performed is maintained and
#' accessible within a layer using \code{\link{get_numeric_data}}.
#'
#' The `metadata` option of build will trigger the construction of traceability
#' metadata for the constructed data frame. Essentially, for every "result" that
#' Tplyr produces, Tplyr can also generate the steps necessary to obtain the
#' source data which produced that result from the input. For more information,
#' see vignette("metadata").
#'
#' @param x A \code{tplyr_table} object
#' @param metadata  Trigger to build metadata. Defaults to FALSE
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
#'     group_desc(Sepal.Length, by = "Sepal Length")
#'   ) %>%
#'   add_layer(
#'     group_desc(Sepal.Width, by = "Sepal Width")
#'   ) %>%
#'   build()
#'
#' @seealso tplyr_table, tplyr_layer, add_layer, add_layers, layer_constructors
build <- function(x, metadata=FALSE) {
    UseMethod("build")
}

#' tplyr_table S3 method
#' @noRd
#' @export
build.tplyr_table <- function(x, metadata=FALSE) {

  op <- options()

  tryCatch({
    # Override scipen with Typlr option
    options('scipen' = getOption('tplyr.scipen')) # Override scipen

    # Table Pre build
    treatment_group_build(x)

    x <- build_header_n(x)

    # Process Layer summaries
    map(x$layers, process_summaries)

    # Get table formatting info
    formatting_meta <- fetch_formatting_info(x)

    # Format layers/table and pivot. process_formatting should return the built table!
    output_list <- purrr::map(x$layers, process_formatting)

    output <- output_list %>%
      map2_dfr(seq_along(output_list), add_layer_index) %>%
      ungroup() %>%
      select(starts_with('row_label'), starts_with('var'), "ord_layer_index", everything())

    # Process metadata if triggered
    if (metadata) {
      metadata_list <- purrr::map(x$layers, process_metadata)

      # Prepare metadata like the output
      metadata <- metadata_list %>%
        map2_dfr(seq_along(metadata_list), add_layer_index) %>%
        ungroup() %>%
        mutate(
          row_id = paste0(row_id, '_', ord_layer_index)
        ) %>%
        select(row_id, starts_with('row_label'), starts_with('var'), everything(), -starts_with('ord'))

      # Finish off the row_id with the layer indicator and put row_id up front
      output <- output %>%
        mutate(
          row_id = paste0(row_id, '_', ord_layer_index)
        ) %>%
        select(row_id, everything())

      # Write the metadata to the environment
      env_bind(x, metadata=metadata)
    } else {
      # Drop row_id if metadata isn't built
      output <- output %>%
        select(-row_id)
    }



  }, finally = {
    # Set options back to defaults
    options(op)
  })

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
#' @keywords internal
process_summaries <- function(x, ...) {
  UseMethod("process_summaries")
}

#' Process layers to get formatted and pivoted tables.
#'
#' This is an internal method, but is exported to support S3 dispatch. Not intended for direct use by a user.
#' @param x A tplyr_layer object
#' @param ... arguments passed to dispatch
#'
#' @return The formatted_table object that is bound to the layer
#' @export
#' @keywords internal
process_formatting <- function(x, ...) {
  UseMethod("process_formatting")
}

#' @noRd
prepare_format_metadata <- function(x) {
  UseMethod("prepare_format_metadata")
}

#' Process layers to get metadata tables
#'
#' This is an internal method, but is exported to support S3 dispatch. Not intended for direct use by a user.
#' @param x A tplyr_layer object
#' @param ... arguments passed to dispatch
#'
#' @return The formatted_meta object that is bound to the layer
#' @export
#' @keywords internal
process_metadata <- function(x, ...) {
  UseMethod("process_metadata")
}

#' Fetch table formatting info from layers
#'
#' @param x A tplyr_table object
#'
#' @return The data used to format layers. Structure currently TBD
#' @noRd
fetch_formatting_info <- function(x) {

  # Get the max length of f_str objects in sub_layers
  max_layer_length <- max(map_int(x$layers, ~ env_get(.x, "max_length")), inherit = TRUE)
  # Get the max length of n counts only, not including f_str formatting
  max_n_width <- max(map_dbl(x$layers, ~ ifelse(inherits(.x, 'count_layer'), .x$n_width, 0L)))

  env_bind(x, max_layer_length = max_layer_length)
  env_bind(x, max_n_width = max_n_width)
}
