### Table Properties These shouldn't require a method dispatch

#' Return or set header bindings
#'
#' The column header that will be displayed when the table is rendered.
#'
#' @param table A \code{tplyr_table} object
#'
#' @return For \code{tplyr_header} the header binding of the \code{tplyr_talbe}
#'   object. For \code{tplyr_header<-} and \code{set_tplyr_header} the modified
#'   object.
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' header(tab) <- c("Sepal.Length", "Sepal.Width", "Petal.Length",
#'                        "Petal.Width", "Species")
#'
#' @rdname headers
#' @export
header <- function(table) {
  rlang::env_get(table, "headers")
}

#' @param x A \code{tplyr_table} object
#' @param value A character vector detailing the column headers
#'
#' @rdname headers
#' @export
`header<-` <- function(x, value) {
  set_header(x, value)
}

#' @param headers A character vector detailing the column headers
#'
#' @rdname headers
#' @export
set_header <- function(table, headers) {
  # headers should be a character vector
  assert_that(is.character(headers),
                          msg = paste0("'headers' argument passed to tplyr_table must be a character vector,",
                                       "\n",
                                       "instead a class of: '",
                                       class(headers),
                                       "' was passed."))


  rlang::env_bind(table, headers = headers)

  table
}

#' Return or set header_n binding
#'
#' When the header or table body relies on population count data, the header_n
#' binding is used for display and calculations.
#'
#' @param table A \code{tplyr_table} object
#'
#' @return For \code{tplyr_header_n} the header_n binding of the
#'   \code{tplyr_table} object. For \code{tplyr_header_n<-} and
#'   \code{set_tplyr_header_n} the modified object.
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' header_n(tab) <- c(setosa = 50, versicolor = 50, virginica = 50)
#'
#' @export
#' @rdname header_n
header_n <- function(table) {
  rlang::env_get(table, "header_n")
}

#' @param x A \code{tplyr_table} object
#' @param value A named numeric vector. Names of vector should match treatement
#'   group names.
#'
#' @export
#' @rdname header_n
`header_n<-` <- function(x, value) {
  set_header_n(x, value)
}

#' @param header_n A named numeric vector. Names of vector should match treatement
#'   group names.
#'
#' @export
#' @rdname header_n
set_header_n <- function(table, header_n) {
  assert_that(is.numeric(header_n),
                          msg = "header_n argument must be numeric")

  assert_that(!is.null(names(header_n)),
              msg = "header_n argument must be named")

  rlang::env_bind(table, header_n = header_n)

  table
}

#' Return or set population data bindings
#'
#' The population data is used when calculating N counts in the column headers,
#' as well as determining denominators for percent calculations in the table
#' body.
#'
#' @param table A \code{tplyr_table} object
#' @param pop_data A \code{data.frame} object containing the populatoin level
#'   information.
#'
#' @return For \code{tplyr_pop_data} the pop_data binding of the \code{tplyr_table}
#'   object. For \code{tplyr_pop_data<-} nothing is returned, the pop_data binding
#'   is set silently. For \code{set_tplyr_pop_data} the modified object.
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' pop_data(tab) <- mtcars
#'
#' @export
#' @rdname pop_data
pop_data <- function(table) {
  rlang::env_get(table, "pop_data")
}

#' @param x A \code{tplyr_table} object
#' @param value A data.frame with population level information
#'
#' @export
#' @rdname pop_data
`pop_data<-` <- function(x, value) {
  set_pop_data(x, value)
}

#' @param pop_data A data.frame with population level information
#'
#' @export
#' @rdname pop_data
set_pop_data <- function(table, pop_data) {
  # table should be a data.frame
  assert_that(inherits(pop_data, "data.frame"),
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
#'   object. For \code{set_tplyr_treat_var} the modified object.
#'
#' @examples
#' tab <- tplyr_table(mtcars, cyl)
#'
#' set_treat_var(tab, gear)
#'
#' @export
#' @rdname treat_var
treat_var <- function(table) {
  rlang::env_get(table, "treat_var")
}

#' @param treat_var A treatment variable. quosure?
#'
#' @export
#' @rdname treat_var
set_treat_var <- function(table, treat_var) {
  treat_var <- rlang::enquo(treat_var)

  assert_that(!quo_is_missing(treat_var),
                          msg = "A treat_var argument must be supplied")

  assert_that(class(rlang::quo_get_expr(treat_var)) == "name",
                          as_label(rlang::quo_get_expr(treat_var)) %in% names(table$target),
                          msg = "treat_var column not found in target dataset")

  rlang::env_bind(table, treat_var = treat_var)

  table
}

#' Return or set pop_treat_var binding
#'
#' @param table A \code{tplyr_table} object
#'
#' @return For \code{tplyr_pop_treat_var} the pop_treat_var binding of the \code{tplyr_table}
#'   object. For \code{set_tplyr_pop_treat_var} the modified object.
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' pop_data(tab) <- mtcars
#' set_pop_treat_var(tab, mpg)
#'
#' @rdname pop_treat_var
#' @export
pop_treat_var <- function(table) {
  rlang::env_get(table, "pop_treat_var")
}

#' @param pop_treat_var A named quosure from the pop_data environment
#'
#' @rdname pop_treat_var
#' @export
set_pop_treat_var <- function(table, pop_treat_var) {
  pop_treat_var <- rlang::enquo(pop_treat_var)

  assert_that(class(rlang::quo_get_expr(pop_treat_var)) == "name",
                          as_label(rlang::quo_get_expr(pop_treat_var)) %in% names(table$pop_data),
                          msg = paste0("pop_treat_var passed to tplyr_table is not a column of pop_data"))

  rlang::env_bind(table, pop_treat_var = pop_treat_var)

  table
}

#' Return or set treatment groups binding
#'
#' Treatment groupings are used to create combenations of groups to group. TODO:
#' Add 'total' option
#'
#' @param table A tplyr_table object
#'
#' @return For \code{treat_grps} the treat_grp binding of the \code{tplyr_talbe}
#'   object. For \code{treat_grps<-} and \code{set_treat_grps} the modified
#'   object. \code{add_treat_group} adds a treatment group without removing others.
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' add_treat_group(tab, "Total", as.character(unique(iris$Species)))
#'
#' @export
#' @rdname treat_grps
treat_grps <- function(table) {
  env_get(table, "treat_grps")
}

#' @param group_name A character vector with the treatment group names.
#' @param groupings A character vector specifiying the treatment variable names.
#'
#' @export
#' @rdname treat_grps
set_treat_grps <- function(table, group_name, groupings) {
  assert_that(is.character(group_name), is.character(groupings),
              msg = "'group_name' and 'groupings' argument passed to set_treat_grps must be a character")

  # Name a list to bind to the environment
  a_list <- list(groupings)
  names(a_list) <- group_name

  rlang::env_bind(table, treat_grps = a_list)

  table
}

#' @export
#' @rdname treat_grps
add_treat_group <- function(table, group_name, groupings) {
  assert_that(is.character(group_name), is.character(groupings),
              msg = "'group_name' and 'groupings' argument passed to add_treat_group must be a character")

  # Get existing treatment groups
  a_list <- env_get(table, "treat_grps")

  # Append new treatment group to existing treatment groups
  new_list <- list(groupings)
  names(new_list) <- group_name
  a_list <- append(a_list, new_list)

  env_bind(table, treat_grps = a_list)

  table
}

