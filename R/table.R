### Table Constructor

#' Safety Table
#'
#' @description
#' The function `tplyr_table()` creates a 'tplyr_table' object which is composed
#' of a table and table display options, and logic for creating the table. The
#' layer environments passed to a 'tplyr_table' are used to evaluate the code
#' used to create the table.
#'
#' @section Environment Bindings/Properties:
#' \itemize{
#' \item{table - The main piece of data the table will be created from.}
#' \item{headers - The column headers for the table. These will be repeated
#'   across pages as appropriate.}
#' \item{header_n - The population count information used as the denominator
#'   and display in the column headers.}
#' \item{layers - The logic used to prepare and render the table.}
#' }
#'
#' @param table Source data used to create the table. A 'data.frame' object.
#' @param headers Column display headers
#' @param header_n Population count information to dispaly in headers. A
#'   data.frame with population level information
#'
#' @return A safety_table object which is an environment where the code
#'   creating the table is evaluated.
#'
#' @import rlang
#' @seealso [layer()]
#'
#' @examples
#' df <- data.frame(a = 1:10, b = 11:20)
#' headers <- c("a", "b")
#' header_n <- c(a = "10", b = "10")
#'
#' tplyr_table(df, headers, header_n)
#' @export
tplyr_table <- function(table, headers, header_n) {
  if(missing(table)){
    #return a blank environment if no table information is passed
    return(structure(rlang::env(),
           class = c("tplyr_table", "environment")))
  }
  new_tplyr_table(table, headers, header_n)
}

#' @import rlang
#' @noRd
new_tplyr_table <- function(table, headers, header_n) {
  validate_tplyr_table(table, headers, header_n)

  structure(rlang::env(
    table = table,
    headers = headers,
    header_n = header_n
  ),
           class = append("tplyr_table", "environment"))
}

#' @import rlang
#' @noRd
validate_tplyr_table <- function(table, headers, header_n) {

  # table should be a data.frame
  if(!inherits(table, "data.frame")) {
    msg <- paste0("'table' argument passed to tplyr_table must be a data.frame,",
                  "\n",
                  "instead a class of: '",
                  class(table),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "tplyr_table_constructor_error")
  }

  # headers should be a character vector
  if(!inherits(headers, "character")) {
    msg <- paste0("'headers' argument passed to tplyr_table must be a data.frame,",
                  "\n",
                  "instead a class of: '",
                  class(headers),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "tplyr_table_constructor_error")
  }

  # header_n should be a named character vector with the names matching the
  # headers
  if(!inherits(header_n, "character")) {
    msg <- paste0("'header_n' argument passed to tplyr_table must be a character vector,",
                  "\n",
                  "instead a class of: '",
                  class(header_n),
                  "' was passed.")
    rlang::abort(message = msg,
                 class = "tplyr_table_constructor_error")
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
                 class = "tplyr_table_constructor_error")
  }
  if(!(all(names(header_n) %in% headers) &
     all(headers %in% names(header_n)))) {
    msg <- paste0("'header_n' argument must have same names as the 'headers' argument values,",
                  "\n",
                  "names(header_n): ",
                  names(header_n),
                  "\n",
                  "headers: ",
                  headers)
    rlang::abort(message = msg,
                 class = "tplyr_table_constructor_error")
  }
}


