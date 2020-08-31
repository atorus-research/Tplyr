#' Gather the default settings for a particular layer
#'
#' Format settings can be applied to a layer, to a table, or set
#' via default options.
#'
#' @param x A tplyr layer
#'
#' @return The default format strings
#' @noRd
gather_defaults <- function(x) {
  UseMethod('gather_defaults')
}

#' Desc layer format string option extraction
#'
#' @param x A desc layer
#'
#' @return The default format strings
#' @noRd
gather_defaults.desc_layer <- function(x) {
  # Get the defaults set within options
  opt_settings <- getOption('tplyr.desc_layer_default_formats')
  # Get the table defaults if they're available
  table_settings <- evalq(desc_layer_formats, envir=x)

  # Return the opt settings if the table settings are null
  # Otherwise return the table settings
  if (is_empty(table_settings)) {
    opt_settings
  } else {
    table_settings
  }
}


#' Count layer format string option extraction
#'
#' @param x A count layer
#'
#' @return The default format strings
#' @noRd
gather_defaults.count_layer <- function(x) {
  # Get the defaults set within options
  opt_settings <- getOption('tplyr.count_layer_default_formats')
  # Get the table defaults if they're available
  table_settings <- evalq(count_layer_formats, envir=x)

  # Append together - table will be preferred over option when indexing
  append(table_settings, opt_settings)
}

#' Shift layer format string option extraction
#'
#' @param x A shift layer
#'
#' @return The default format strings
#' @noRd
gather_defaults.shift_layer <- function(x) {
  # Get the defaults set within options
  opt_settings <- getOption('tplyr.shift_layer_default_formats')
  # Get the table defaults if they're available
  table_settings <- evalq(shift_layer_formats, envir=x)

  # Append together - table will be preferred over option when indexing
  append(table_settings, opt_settings)
}
