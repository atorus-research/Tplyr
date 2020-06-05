#'
#' #' Set or return treat_var binding
#' #'
#' #' @param layer A \code{tplyr_layer} object
#' #'
#' #' @return For \code{treat_var}, the treatment variable binding of the layer
#' #'   object. For \code{set_treat_var}, the modified layer environment.
#' #' @export
#' #' @rdname treat_var
#' #'
#' #' @examples
#' layer_treat_var <- function(layer) {
#'   env_get(layer, "treat_var")
#' }
#'
#' #' @param treat_var A symbol to perform the analysis on
#' #'
#' #' @export
#' #' @rdname treat_var
#' set_layer_treat_var <- function(layer, treat_var) {
#'   assert_that(is_quosure(treat_var),
#'               msg = "treat_var must be a Symbol")
#'
#'   treat_var <- enquo(treat_var)
#'
#'   assert_that(quo_get_expr(treat_var) == "name",
#'               msg = "treat_var must be a variable name")
#'
#'   env_bind(layer, treat_var = treat_var)
#' }
#'
#' #' Set or return by layer binding
#' #'
#' #' @param layer A \code{tplyr_layer} object
#' #'
#' #' @return For \code{tplyr_by}, the by binding of the supplied layer. For
#' #'   \code{set_tplyr_by} the modified layer environment.
#' #' @export
#' #' @rdname by
#' #'
#' #' @examples
#' tplyr_by <- function(layer) {
#'   env_get(layer, "by")
#' }
#'
#' #' @param by A string, a variable name, or a list of variable names supplied
#' #'   using \code{dplyr::vars}.
#' #'
#' #' @export
#' #' @rdname by
#' set_tplyr_by <- function(layer, by) {
#'   dmessage(paste("By came in as: ",class(by)))
#'
#'   # Make sure that by variables not submitted as characters exist in the target dataframe
#'   if (!quo_is_null(by[[1]])) {
#'     # Make sure the variables provided to `by` are of the correct type
#'     msg = paste0("Invalid input to `by`. Submit either a string, a variable name, ",
#'                  "or multiple variable names using `dplyr::vars`.")
#'     are_quosures <- all(sapply(by, function(x) is_quosure(x)))
#'     assert_that(are_quosures, msg = msg)
#'
#'     # Check each element of the `by`` list
#'     for (v in by) {
#'       dmessage(print(quo_get_expr(v)))
#'       dmessage(paste("Checking", as.character(quo_get_expr(v))))
#'       if (class(quo_get_expr(v)) == "name") {
#'         vname <- as.character(quo_get_expr(v))
#'         assert_that(vname %in% vnames,
#'                     msg = paste0("By variable `",vname, "` does not exist in target dataset"))
#'       }
#'       # While looping, making sure calls weren't submitted
#'       if (class(quo_get_expr(v)) == "call") {
#'         abort("Arguments to `by` must be names or character strings - cannot be calls (i.e. x + y, list(a, b c)).")
#'       }
#'       else if (!class(quo_get_expr(v)) %in% c('name', 'character')) {
#'         abort("Invalid input to `by`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`.")
#'       }
#'     }
#'   }
#'
#'   env_bind(layer, by = by)
#' }
#'
#' #' Set or return where layer binding
#' #'
#' #' @param layer A \code{tplyr_layer} object.
#' #'
#' #' @return
#' #' @export
#' #' @rdname where
#' #'
#' #' @examples
#' tplyr_where <- function(layer) {
#'   env_get(layer, "where")
#' }
#'
#' set_tplyr_where <- function(layer, where) {
#'   where <- enquo(where)
#'   assert_that(is.null(quo_get_expr(where)) || class(quo_get_expr(where)) == 'call',
#'               msg = "The `where` parameter must contain subsetting logic (enter without quotes)")
#'
#'   env_bind(layer, where = where)
#' }
#'
#' #' Return or set sort_vars layer binding
#' #'
#' #' @param layer
#' #'
#' #' @return For \code{sort_vars}, the bindings of the layer object. For
#' #'   \code{set_sort_vars}, the modified layer environment.
#' #' @export
#' #' @rdname sort_vars
#' #'
#' #' @examples
#' sort_vars <- function(layer) {
#'   env_get(layer, "sort_vars")
#' }
#'
#' #' @param sort_vars A character vector to sort the results of the summary.
#' #'
#' #' @export
#' #' @rdname sort_vars
#' set_sort_vars <- function(layer, sort_vars) {
#'   assert_that((is.character(sort_vars) |
#'                quo_get_expr(sort_vars) == "name"),
#'               msg = "sort_vars must be a character vector or variable name")
#'
#'   env_bind(layer, sort_vars = sort_vars)
#' }
#'
#' #' Set or return sort layer binding
#' #'
#' #' @param layer A \code{tplyr_layer} object
#' #'
#' #' @return For \code{layer_sort}, the sort binding of the layer object. For
#' #'   \code{set_layer_sort}, the modified layer environment.
#' #' @export
#' #' @rdname sort
#' #'
#' #' @examples
#' layer_sort <- function(layer) {
#'   env_get(layer, "sort")
#' }
#'
#' #' @param sort A string containing the sort method.
#' #'
#' #' @export
#' #' @rdname sort
#' set_layer_sort <- function(layer, sort) {
#'   assert_that(length(sort) == 1,
#'               sort %in% c("asc", "desc"),
#'               msg = "sort must be 'asc', 'desc'")
#'
#'   env_bind(layer, sort = sort)
#' }
#'
#' #' Set or return layer formatter
#' #'
#' #' @param layer A \code{tplyr_layer} object
#' #'
#' #' @return For \code{layer_formatter}, the formetter function bound to the
#' #'   supplied layer. For \code{set_layer_formatter} the modified layer environment.
#' #' @export
#' #' @rdname formatter
#' #'
#' #' @examples
#' layer_formatter <- function(layer) {
#'   env_get(layer, "formatter")
#' }
#'
#' #' @param formatter A function used to create the string formats for the
#' #'   resulting numbers in output presentation.
#' #'
#' #' @export
#' #' @rdname formatter
#' set_layer_formatter <- function(layer, formatter) {
#'   assert_that(is_function(formatter),
#'               msg = "formatter must be a function")
#'
#'   env_bind(layer, formatter = formatter)
#' }
#'
