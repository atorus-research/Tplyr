### Table Properties These shouldn't require a method dispatch

#' Return or set header bindings
#'
#' The column header that will be displayed when the table is rendered.
#'
#' @param table A \code{tplyr_table} object to set or return header information.
#'
#' @return For \code{tplyr_header} the header binding of the \code{tplyr_talbe}
#'   object. For \code{tplyr_header<-} and \code{set_tplyr_header} the modified
#'   object.
#'
#' @importFrom rlang env_get
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' tplyr_header(tab) <- c("Sepal.Length", "Sepal.Width", "Petal.Length",
#'                        "Petal.Width", "Species")
#'
#' @rdname headers
#' @export
tplyr_header <- function(table) {
  rlang::env_get(table, "headers")
}

#' @param x A \code{tplyr_table} object
#' @param value A character vector detailing the column headers
#'
#' @importFrom rlang env_bind
#' @importFrom assertthat assert_that
#'
#' @rdname headers
#' @export
`tplyr_header<-` <- function(x, value) {
  # headers should be a character vector
  assertthat::assert_that(inherits(value, "character"),
                          msg = paste0("'headers' argument passed to tplyr_table must be a character vector,",
                                       "\n",
                                       "instead a class of: '",
                                       class(value),
                                       "' was passed."))


  rlang::env_bind(x, headers = value)

  x
}

#' @param table A \code{tplyr_table} object.
#' @param headers A character vector detailing the column headers
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang env_bind
#'
#' @rdname headers
#' @export
set_tplyr_header <- function(table, headers) {
  # headers should be a character vector
  assertthat::assert_that(inherits(headers, "character"),
                          msg = paste0("'headers' argument passed to tplyr_table must be a character vector,",
                                       "\n",
                                       "instead a class of: '",
                                       class(headers),
                                       "' was passed."))


  rlang::env_bind(table, headers = headers)

  table
}

#' Return or set population data bindings
#'
#' The population data is used when calculating N counts in the column headers,
#' as well as determining denominators for percent calculations in the table
#' body.
#'
#' @param table A \code{tplyr_table} object to set or return population data
#'   information.
#' @param pop_data A \code{data.frame} object containing the
#'
#' @return For \code{tplyr_pop_data} the pop_data binding of the \code{tplyr_table}
#'   object. For \code{tplyr_pop_data<-} nothing is returned, the pop_data binding
#'   is set silently. For \code{set_tplyr_pop_data} the modified object.
#'
#' @importFrom rlang env_get
#' @export
#' @rdname pop_data
tplyr_pop_data <- function(table) {
  env_get(table, "pop_data")
}

#' Return or set header_n binding
#'
#' @param table A \code{tplyr_table} object
#'
#' @importFrom rlang env_get
#'
#' @return For \code{tplyr_header_n} the header_n binding of the
#'   \code{tplyr_table} object. For \code{tplyr_header_n<-} and
#'   \code{set_tplyr_header_n} the modified object.
#'
#' @export
#' @rdname header_n
tplyr_header_n <- function(table) {
  rlang::env_get(table, "header_n")
}

#' @param x A \code{tplyr_table} object
#' @param value A named numeric vector. Names of vector should match headers.
#'
#' @importFrom rlang env_bind
#' @importFrom rlang env_has
#' @importFrom rlang env_get
#' @importFrom assertthat assert_that
#'
#' @export
#' @rdname header_n
`tplyr_header_n<-` <- function(x, value) {
  assertthat::assert_that(is.numeric(value))

  rlang::env_bind(x, header_n = value)

  x
}

#' @param header_n A named numeric vector. Names of vector should match headers.
#'
#' @importFrom rlang env_bind
#' @importFrom rlang env_get
#' @importFrom rlang env_has
#' @importFrom assertthat assert_that
#'
#' @export
#' @rdname header_n
set_tplyr_header_n <- function(table, header_n) {
  assertthat::assert_that(is.character(header_n))

  rlang::env_bind(table, header_n = header_n)

  table
}

#' Return or set pop_data binding
#'
#' @param table A \code{tplyr_table} object.
#'
#' @return For \code{tplyr_pop_data} the pop_data binding of the
#'   \code{tplyr_table} object. For \code{tplyr_pop_data<-} and
#'   \code{set_tplyr_pop_data} the modified object.
#'
#' @importFrom rlang env_get
#'
#' @export
#' @rdname pop_data
tplyr_pop_data <- function(table) {
  rlang::env_get(table, "pop_data")
}

#' @param x A \code{tplyr_table} object.
#' @param value A data.frame with population level information
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang env_bind
#'
#' @export
#' @rdname pop_data
`tplyr_pop_data<-` <- function(x, value) {
  # table should be a data.frame
  assertthat::assert_that(inherits(value, "data.frame"),
                          msg = paste0("'pop_data' argument passed to tplyr_table must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(value),
                                       "' was passed."))
  env_bind(x, pop_data = value)

  x
}

#' @param pop_data A data.frame with population level information
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang env_bind
#'
#' @export
#' @rdname pop_data
set_tplyr_pop_data <- function(table, pop_data) {
  # table should be a data.frame
  assertthat::assert_that(inherits(pop_data, "data.frame"),
                          msg = paste0("'pop_data' argument passed to tplyr_table must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(pop_data),
                                       "' was passed."))
  rlang::env_bind(table, pop_data = pop_data)

  table
}

#' Return or set the treatment variable binding
#'
#' @param table A \code{tplyr_table} object to set or return treatment variable
#'   the table is split by.
#'
#' @return For \code{tplyr_treat_var} the treat_var binding of the \code{tplyr_table}
#'   object. For \code{tplyr_treat_var<-} nothing is returned, the treat_var binding
#'   is set silently. For \code{set_tplyr_treat_var} the modified object.
#'
#' @importFrom rlang env_get
#'
#' @export
#' @rdname treat_var
tplyr_treat_var <- function(table) {
  rlang::env_get(table, "treat_var")
}

#' @param x A \code{tplyr_table} object to set or return treatment variable
#'   the table is split by.
#' @param value A treatment variable. quosure?
#'
#' @importFrom rlang env_bind
#' @importFrom rlang enquo
#'
#' @export
#' @rdname treat_var
`tplyr_treat_var<-` <- function(x, value) {
  value <- rlang::enquo(value)

  rlang::env_bind(x, treat_var = value)

  x
}

#' @param treat_var A treatment variable. quosure?
#'
#' @importFrom rlang env_bind
#' @importFrom rlang enquo
#'
#' @export
#' @rdname treat_var
set_tplyr_treat_var <- function(table, treat_var) {
  treat_var <- rlang::enquo(treat_var)

  assertthat::assert_that(class(rlang::quo_get_expr(treat_var)) == "name",
                          as.character(rlang::quo_get_expr(treat_var)) %in% names(table$target))

  rlang::env_bind(table, treat_var = treat_var)

  table
}

#' Return or set pop_treat_var binding
#'
#' @param table A \code{tplyr_table} object
#'
#' @return For \code{tplyr_pop_treat_var} the pop_treat_var binding of the \code{tplyr_table}
#'   object. For \code{tplyr_pop_treat_var<-} and \code{set_tplyr_pop_treat_var} the modified
#'   object.
#'
#' @importFrom rlang env_get
#'
#' @rdname pop_treat_var
#' @export
tplyr_pop_treat_var <- function(table) {
  rlang::env_get(table, "pop_treat_var")
}

#' @param x A \code{tplyr_table} object
#' @param value A named quosure from the pop_data environment
#'
#' @rdname pop_treat_var
#' @export
`tplyr_pop_treat_var<-` <- function(x, value) {
  pop_treat_var <- rlang::enquo(value)

  assertthat::assert_that(class(rlang::quo_get_expr(pop_treat_var)) == "name",
                          as.character(rlang::quo_get_expr(pop_treat_var)) %in% names(x$pop_data))

  rlang::env_bind(x, pop_treat_var = pop_treat_var)

  x
}

#' @param pop_treat_var A named quosure from the pop_data environment
#'
#' @rdname pop_treat_var
#' @export
set_tplyr_pop_treat_var <- function(table, pop_treat_var) {
  pop_treat_var <- rlang::enquo(pop_treat_var)

  assertthat::assert_that(class(rlang::quo_get_expr(pop_treat_var)) == "name",
                          as.character(rlang::quo_get_expr(pop_treat_var)) %in% names(table$pop_data))

  rlang::env_bind(table, pop_treat_var = pop_treat_var)

  table
}
