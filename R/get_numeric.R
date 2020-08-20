#' Retrieve the numeric data from a tplyr objects
#'
#' \code{get_numeric_data} provides access to the un-formatted numeric data for
#' each of the layers within a \code{tplyr_table}, with options to allow you to
#' extract distinct layers and filter as desired.
#'
#' When used on a \code{tplyr_table} object, this method will aggregate the
#' numeric data from all Tplyr layers. The data will be returned to the user in
#' a list of data frames. If the data has already been processed (i.e.
#' \code{build} has been run), the numeric data is already available and will be
#' returned without reprocessing. Otherwise, the numeric portion of the layer
#' will be processed.
#'
#' Using the layer and where parameters, data for a specific layer can be
#' extracted and subset. This is most clear when layers are given text names
#' instead of using a layer index, but a numeric index works as well.
#'
#' @param x A tplyr_table or tplyr_layer object
#' @param layer Layer name or index to select out specifically
#' @param where Subset criteria passed to dplyr::filter
#' @param ... Additional arguments to pass forward
#'
#' @return Numeric data from the Tplyr layer
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' t <- tplyr_table(mtcars, gear) %>%
#'  add_layer(name='drat',
#'            group_desc(drat)
#'  ) %>%
#'  add_layer(name='cyl',
#'            group_count(cyl)
#'  )
#'
#'  # Return a list of the numeric data frames
#'  get_numeric_data(t)
#'
#'  # Get the data from a specific layer
#'  get_numeric_data(t, layer='drat')
#'  get_numeric_data(t, layer=1)
#'
#'  # Choose multiple layers by name or index
#'  get_numeric_data(t, layer=c('cyl', 'drat'))
#'  get_numeric_data(t, layer=c(2, 1))
#'
#'  # Get the data and filter it
#'  get_numeric_data(t, layer='drat', where = gear==3)
#'
get_numeric_data <- function(x, layer=NULL, where=TRUE, ...) {
  UseMethod("get_numeric_data")
}


#' Get numeric data from a tplyr_table object
#' @export
#' @noRd
get_numeric_data.tplyr_table <- function(x, layer=NULL, where=TRUE, ...) {

  where <- enquo(where)

  # If where is provided then a layer must be specified
  assert_that(!(quo_get_expr(where) != TRUE && (is.null(layer) || length(layer)!=1)),
              msg="If `where` is provided, a single `layer` value must be specified")

  # If layer is a numeric value then it must be in range
  if (is.numeric(layer)) {
    assert_that(all(between(layer, 1, length(x$layers))), msg="Provided layer index is out of range")
  } else if (!is.null(layer) && !is.null(names(x$layers))) {
    # The provided name must exist
    missing_layers <- layer %in% names(x$layers)
    assert_that(all(missing_layers), msg=paste0("Layer(s) ", paste0(layer[!missing_layers], collapse=", "), " do(es) not exist"))
  }

  # If the pre-build wasn't executed then execute it
  if (!'built_target' %in% ls(x)) {
    treatment_group_build(x)
    build_header_n(x)
  }

  # If not picking a specific layer, then get all the numeric data
  if (is.null(layer)) {
    return(map(x$layers, get_numeric_data))
  } else if (length(layer) > 1) {
    # If the layer variable was multiple elements then grap all of them
    return(map(x$layers[layer], get_numeric_data))
  } else {
    # Otherwise, pick it out and filter
    get_numeric_data(x$layers[[layer]]) %>%
      filter(!!where)
  }



}


#' Get numeric data from a tplyr_layer object
#' @export
#' @noRd
get_numeric_data.tplyr_layer <- function(x, layer=NULL, where=TRUE, ...) {

  # If the numeric data doesn't exist in the layer then process it
  if (!'numeric_data' %in% ls(x)) {
    process_summaries(x)
  }

  # Return the object
  env_get(x, 'numeric_data')
}



#' Get statistics data
#'
#' Like the layer numeric data, Tplyr also stores the numeric data produced from
#' statistics like risk difference. This helper function gives you access to
#' obtain that data from the environment
#'
#' When used on a \code{tplyr_table} object, this method will aggregate the
#' numeric data from all Tplyr layers and calculate all statistics. The data
#' will be returned to the user in a list of data frames. If the data has
#' already been processed (i.e. \code{build} has been run), the numeric data is
#' already available and the statistic data will simply be returned. Otherwise,
#' the numeric portion of the layer will be processed.
#'
#' Using the layer, where, and statistic parameters, data for a specific layer
#' statistic can be extracted and subset, allowing you to directly access data
#' of interest. This is most clear when layers are given text names instead of
#' using a layer index, but a numeric index works as well. If just a statistic
#' is specified, that statistic will be collected and returned in a list of data
#' frames, allowing you to grab, for example, just the risk difference
#' statistics across all layers.
#'
#' @param x A tplyr_table or tplyr_layer object
#' @param layer Layer name or index to select out specifically
#' @param statistic Statistic name or index to select
#' @param where Subset criteria passed to dplyr::filter
#' @param ... Additional arguments passed to dispatch
#'
#' @return The statistics data of the supplied layer
#' @export
#'
#' @examples
#' library(magrittr)
#'
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(name='drat',
#'             group_desc(drat)
#'   ) %>%
#'   add_layer(name="cyl",
#'             group_count(cyl)
#'   ) %>%
#'   add_layer(name="am",
#'             group_count(am) %>%
#'               add_risk_diff(c('4', '3'))
#'   ) %>%
#'   add_layer(name="carb",
#'             group_count(carb) %>%
#'               add_risk_diff(c('4', '3'))
#'   )
#'
#'  # Returns a list of lists, containing stats data from each layer
#'  get_stats_data(t)
#'
#'  # Returns just the riskdiff statistics from each layer - NULL
#'  # for layers without riskdiff
#'  get_stats_data(t, statistic="riskdiff")
#'
#'  # Return the statistic data for just the "am" layer - a list
#'  get_stats_data(t, layer="am")
#'  get_stats_data(t, layer=3)
#'
#'  # Return the statistic data for just the "am" and "cyl", layer - a
#'  # list of lists
#'  get_stats_data(t, layer=c("am", "cyl"))
#'  get_stats_data(t, layer=c(3, 2))
#'
#'  # Return just the statistic data for "am" and "cyl" - a list
#'  get_stats_data(t, layer=c("am", "cyl"), statistic="riskdiff")
#'  get_stats_data(t, layer=c(3, 2), statistic="riskdiff")
#'
#'
#'  # Return the riskdiff for the "am" layer - a data frame
#'  get_stats_data(t, layer="am", statistic="riskdiff")
#'
#'  # Return and filter the riskdiff for the am layer - a data frame
#'  get_stats_data(t, layer="am", statistic="riskdiff", where = summary_var==1)
#'
get_stats_data <- function(x, layer=NULL, statistic=NULL, where=TRUE, ...) {
  UseMethod("get_stats_data")
}


#' Get numeric data from a tplyr_table object
#' @export
#' @noRd
get_stats_data.tplyr_table <- function(x, layer=NULL, statistic=NULL, where=TRUE, ...) {

  where <- enquo(where)

  # If where is provided then a layer must be specified
  assert_that(!(quo_get_expr(where) != TRUE && ((is.null(layer) || length(layer)!=1) && is.null(statistic))),
              msg="If `where` is provided, `layer_name` and `statistic` must be specified")

  # If layer is a numeric value then it must be in range
  if (is.numeric(layer)) {
    assert_that(all(between(layer, 1, length(x$layers))), msg="Provided layer index is out of range")
  } else if (!is.null(layer) && !is.null(names(x$layers))) {
    # The provided name must exist
    missing_layers <- layer %in% names(x$layers)
    assert_that(all(missing_layers), msg=paste0("Layer(s) ", paste0(layer[!missing_layers], collapse=", "),
                                               " do(es) not exist"))
  }

  # If the pre-build wasn't executed then execute it
  if (!'built_target' %in% ls(x)) {
    treatment_group_build(x)
    build_header_n(x)
  }

  # If not picking a specific layer, then get all the numeric data
  if (is.null(layer)) {
    return(map(x$layers, get_stats_data, statistic=statistic))
  } else if (length(layer) > 1) {
    return(map(x$layers[layer], get_stats_data, statistic=statistic))
  }

  # Otherwise, pick out the stats container
  stats <- get_stats_data(x$layers[[layer]], statistic=statistic)

  if (!is.null(statistic)) {
    stats <- stats %>%
      filter(!!where)
  }

  stats
}


#' Get numeric data from a tplyr_layer object
#' @export
#' @noRd
get_stats_data.tplyr_layer <- function(x, layer=NULL, statistic=NULL, where=TRUE, ...) {

  # If the numeric data doesn't exist in the layer then process it
  if (!'numeric_data' %in% ls(x)) {
    process_summaries(x)
  }

  # Return the object
  stats <- env_get(x, 'stats')

  # Extract the stats_numeric_data object from the environments
  stats <- map(stats, ~ env_get(env=.x, 'stats_numeric_data'))

  if (!is.null(statistic)) {
    stats <- stats[[statistic]]
  }

  stats
}
