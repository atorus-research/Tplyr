### Table Constructor

#' Safety Table
#'
#' @param table Source data used to create the table
#' @param headers Column display headers
#' @param header_n Population count information to dispaly in headers
#' @param layers Layering components used in table display
#'
#' @return A safetyTable environment object
#'
#' @export
#'
#' @examples
safety_table <- function(table, headers, header_n, layers) {
  if(missing(table)){
    return(structure(environment(),
           class = "safetyTable"))
  }
  new_saftey_table(table)
}

#' @noRd
new_saftey_table <- function(table, headers, header_n, layers) {
  validate_safety_table(table, headers, header_n, layers)

  structre(environment(something),
           class = "safetyTable")
}

#' @import rlang
#' @noRd
validate_safety_table <- function(table, headers, header_n, layers) {

  # table should be a data.frame
  if(!inherits(table, "data.frame")) {
    msg <- paste0("'table' argument passed to safety_table must be a data.frame,",
                  "\n",
                  "instead a class of: '",
                  class(table),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "safety_table_constructor_error")
  }

  # headers should be a character vector
  if(!inherits(headers, "character")) {
    msg <- paste0("'headers' argument passed to safety_table must be a data.frame,",
                  "\n",
                  "instead a class of: '",
                  class(headers),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "safety_table_constructor_error")
  }

  # header_n should be a named character vector with the names matching the
  # headers
  if(!inherits(header_n, "character")) {
    msg <- paste0("'header_n' argument passed to safety_table must be a data.frame,",
                  "\n",
                  "instead a class of: '",
                  class(header_n),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "safety_table_constructor_error")
  }
  if(length(header_n) != length(headers)) {
    msg <- paste0("'header_n' argument must be the same length as the 'headers' argument,",
                  "\n",
                  "length(header_n): ",
                  length(header_n),
                  "\n",
                  "length(headers): ",
                  length(headers))
    rlang::abort(message = msg,
                 class = "safety_table_constructor_error")
  }
  if(!all(names(header_n) %in% headers) &
     !all(headers %in% header_n)) {
    msg <- paste0("'header_n' argument must have same names as the 'headers' argument values,",
                  "\n",
                  "names(header_n): ",
                  names(header_n),
                  "\n",
                  "headers: ",
                  headers)
    rlang::abort(message = msg,
                 class = "safety_table_constructor_error")
  }

  # layers should be safety_table_layers
  if(!all(inherits(layers, "safety_table_layers"))) {
    msg <- paste0("'layers' argument passed to safety_table must be a safety_table_layers,",
                  "\n",
                  "instead a class of: '",
                  class(layers),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "safety_table_constructor_error")
  }
}


