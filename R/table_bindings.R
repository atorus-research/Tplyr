### Table Properties These shouldn't require a method dispatch

#' Return or set header_n binding
#'
#' The `header_n()` functions can be used to automatically pull the header_n
#' derivations from the table or change them for future use.
#'
#' @details
#' The `header_n` object is created by Tplyr when a table is built and intended
#' to be used by the `add_column_headers()` function when displaying table level
#' population totals. These methods are intended to be used for calling the
#' population totals calculated by Tplyr, and to overwrite them if a user
#' chooses to.
#'
#' If you have a need to change the header Ns that appear in your table headers,
#' say you know you are working with a subset of the data that doesn't represent
#' the totals, you can replace the data used with `set_header_n()`.
#'
#' @param table A \code{tplyr_table} object
#'
#' @return For \code{tplyr_header_n} the header_n binding of the
#'   \code{tplyr_table} object. For \code{tplyr_header_n<-} and
#'   \code{set_tplyr_header_n} the modified object.
#'
#' @examples
#' tab <- tplyr_table(mtcars, gear)
#'
#' header_n(tab) <- data.frame(
#'   gear = c(3, 4, 5),
#'   n = c(10, 15, 45)
#' )
#'
#'
#'
#' @export
#' @rdname header_n
header_n <- function(table) {
  env_get(table, "header_n")
}

#' @param x A \code{tplyr_table} object
#' @param value A data.frame with columns with the treatment variable, column
#'   variabes, and a variable with counts named 'n'.
#'
#' @export
#' @rdname header_n
`header_n<-` <- function(x, value) {
  set_header_n(x, value)
}

#' @param header_n A data.frame with columns with the treatment variable, column
#'   variabes, and a variable with counts named 'n'.
#'
#' @export
#' @rdname header_n
set_header_n <- function(table, value) {
  assert_that(is.data.frame(value),
                          msg = "header_n argument must be numeric")

  assert_that("n" %in% names(value))

  assert_that(is.numeric(value$n),
              msg = "header_n argument must be named")

  env_bind(table, header_n = value)

  table
}

#' Return or set population data bindings
#'
#' The population data is used to gather information that may not be available
#' from the target dataset. For example, missing treatment groups, population N
#' counts, and proper N counts for denominators will be provided through the
#' population dataset. The population dataset defaults to the target dataset
#' unless otherwise specified using \code{set_pop_data}.
#'
#' @param table A \code{tplyr_table} object
#' @param pop_data A \code{data.frame} object containing the population level
#'   information.
#'
#' @return For \code{tplyr_pop_data} the pop_data binding of the
#'   \code{tplyr_table} object. For \code{tplyr_pop_data<-} nothing is returned,
#'   the pop_data binding is set silently. For \code{set_tplyr_pop_data} the
#'   modified object.
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' pop_data(tab) <- mtcars
#'
#' tab <- tplyr_table(iris, Species) %>%
#'   set_pop_data(mtcars)
#' @export
#' @rdname pop_data
pop_data <- function(table) {
  env_get(table, "pop_data")
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
#'
set_pop_data <- function(table, pop_data) {
  pop_data_name <- enexpr(pop_data)
  # table should be a data.frame
  assert_that(inherits(pop_data, "data.frame"),
                          msg = paste0("'pop_data' argument passed to tplyr_table must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(pop_data),
                                       "' was passed."))

  if(pop_data_name == "target") pop_data_name <- attr(table, "target")
  attr(table, "pop_data_name") <- pop_data_name
  env_bind(table, pop_data = pop_data)

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
  env_get(table, "treat_var")
}

#' @param treat_var Variable containing treatment group assignments. Supply unquoted.
#'
#' @export
#' @rdname treat_var
set_treat_var <- function(table, treat_var) {
  treat_var <- enquo(treat_var)

  assert_that(!quo_is_missing(treat_var),
                          msg = "A treat_var argument must be supplied")

  assert_that(class(quo_get_expr(treat_var)) == "name",
                          as_name(quo_get_expr(treat_var)) %in% names(table$target),
                          msg = "treat_var column not found in target dataset")

  env_bind(table, treat_var = treat_var)

  table
}

#' Return or set pop_treat_var binding
#'
#' The treatment variable used in the target data may be different than the
#' variable within the population dataset. \code{set_pop_treat_var} allows you
#' to change this.
#'
#' @param table A \code{tplyr_table} object
#'
#' @return For \code{tplyr_pop_treat_var} the pop_treat_var binding of the
#'   \code{tplyr_table} object. For \code{set_tplyr_pop_treat_var} the modified
#'   object.
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
  env_get(table, "pop_treat_var")
}

#' @param pop_treat_var Variable containing treatment group assignments within the \code{pop_data} binding. Supply unquoted.
#'
#' @rdname pop_treat_var
#' @export
set_pop_treat_var <- function(table, pop_treat_var) {
  pop_treat_var <- enquo(pop_treat_var)

  assert_that(class(quo_get_expr(pop_treat_var)) == "name",
                          as_name(quo_get_expr(pop_treat_var)) %in% names(table$pop_data),
                          msg = paste0("pop_treat_var passed to tplyr_table is not a column of pop_data"))

  env_bind(table, pop_treat_var = pop_treat_var)

  table
}

#' @export
#' @rdname treat_grps
treat_grps <- function(table) {
  env_get(table, "treat_grps")
}

#' Set or return where binding for layer or table
#'
#' @param obj A \code{tplyr_layer} or \code{tplyr_table} object.
#'
#' @return For \code{where}, the where binding of the supplied object.
#'   For \code{set_where}, the modified object
#' @export
#' @rdname where
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_where(Petal.Length > 3) %>%
#'   # Set logic for pop_data as well
#'   set_pop_where(Petal.Length > 3)
#'
get_where <- function(obj) {
  UseMethod("get_where")
}

#' @rdname where
#' @export
get_where.tplyr_table <- function(obj) {
  env_get(obj, "table_where")
}

#' @param where An expression (i.e. syntax) to be used to subset the data.
#'   Supply as programming logic (i.e. x < 5 & y == 10)
#'
#' @export
#' @rdname where
set_where <- function(obj, where) {
  UseMethod("set_where")
}

#' @rdname where
#' @export
set_where.tplyr_table <- function(obj, where) {
  where <- enquo(where)

  assert_that(is_logical_or_call(where),
              msg = "The `where` parameter must contain subsetting logic (enter without quotes)")

  env_bind(obj, table_where = where)

  obj
}

#' @rdname where
#' @export
set_pop_where <- function(obj, where) {
  where <- enquo(where)

  assert_that(is_logical_or_call(where),
              msg = "The `where` parameter must contain subsetting logic (enter without quotes)")

  env_bind(obj, pop_where = where)

  obj
}

#' @export
#' @rdname where
get_pop_where <- function(obj) {
  env_get(obj, "pop_where")
}

#' Get or set the default format strings for descriptive statistics layers
#'
#' Tplyr provides you with the ability to set table-wide defaults of format
#' strings. You may wish to reuse the same format strings across numerous
#' layers. \code{set_desc_layer_formats} and \code{set_count_layer_formats}
#' allow you to apply your desired format strings within the entire scope of the
#' table.
#'
#' For descriptive statistic layers, you can also use \code{set_format_strings}
#' and \code{set_desc_layer_formats} together within a table, but not within the
#' same layer. In the absence of specified format strings, first the table will
#' be checked for any available defaults, and otherwise the
#' \code{tplyr.desc_layer_default_formats} option will be used.
#' \code{set_format_strings} will always take precedence over either. Defaults
#' cannot be combined between \code{set_format_strings},
#' \code{set_desc_layer_formats}, and the
#' \code{tplyr.desc_layer_default_formats} because the order of presentation of
#' results is controlled by the format strings, so relying on combinations of
#' these setting would not be intuitive.
#'
#' For count layers, you can override the \code{n_counts} or \code{riskdiff}
#' format strings separately, and the narrowest scope available will be used
#' from layer, to table, to default options.
#'
#' @param obj A tplyr_table object
#'
#' @export
#' @rdname table_format_defaults
get_desc_layer_formats <- function(obj) {
  # Bind the formats into the table
  env_get(obj, 'desc_layer_formats')
}

#' @param ... formats to pass forward
#'
#' @export
#' @rdname table_format_defaults
set_desc_layer_formats <- function(obj, ...) {
  # Bind the formats into the table
  env_bind(obj, desc_layer_formats = list2(...))
  obj
}

#' @export
#' @rdname table_format_defaults
get_count_layer_formats <- function(obj) {
  # Bind the formats into the table
  env_get(obj, 'count_layer_formats')
}

#' @param ... formats to pass forward
#'
#' @export
#' @rdname table_format_defaults
set_count_layer_formats <- function(obj, ...) {
  # Bind the formats into the table

  if (length(list2(...)) > 0) params <- count_f_str_check(...)
  else params <- list2(...)

  env_bind(obj, count_layer_formats = params)
  obj
}

#' @export
#' @rdname table_format_defaults
get_shift_layer_formats <- function(obj) {
  # Bind the formats into the table
  env_get(obj, 'shift_layer_formats')
}

#' @export
#' @rdname table_format_defaults
set_shift_layer_formats <- function(obj, ...) {
  # Bind the formats into the table
  env_bind(obj, shift_layer_formats = list2(...))
  obj
}
