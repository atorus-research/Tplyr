### Table Constructor

#' Safety Table
#'
#' @description
#' The function `tplyr_table()` creates a 'tplyr_table' object which is composed
#' of a table and table display options, and logic for creating the table. The
#' layer environments passed to a 'tplyr_table' are used to evaluate the code
#' used to create the table.
#'
#' The tplyr_table is the parent environment of all the binded tplyr_layers.
#' The table can contain variables that are visable to all layers.
#'
#' @section Environment Bindings/Properties:
#' \itemize{
#' \item{target - The main piece of data the table will be created from.}
#' \item{headers - The column headers for the table. These will be repeated
#'   across pages as appropriate.}
#' \item{header_n - The counts of the population groups that is used for column
#'   headers and percentages.}
#' \item{pop_data - The data.frame containing population data.}
#' \item{treat_var - The treatment variable arms/sets are split up on.}
#' \item{layers - The logic used to prepare and render the table. Stored as a
#'   list binded to the environment. Use <add layer function> to add a layer.}
#' }
#'
#' @param target Source data used to create the table. A 'data.frame' object.
#'   This is the analysis dataset the table is generated from.
#' @param treat_var Treatment variable in target used to split treatment
#'   groups.
#'
#' @return A safety_table object which is a parent environment for the layers
#'   where the code creating the table is evaluated.
#'
#' @importFrom rlang env
#' @importFrom magrittr %>%
#' @seealso [layer()]
#'
#' @examples
#' iris_tab <- tplyr_table(iris, Species)
#'
#'
#' @export
tplyr_table <- function(target, treat_var) {

  if(missing(target)){
    #return a blank environment if no table information is passed
    return(structure(rlang::env(),
                     class = c("tplyr_table", "environment")))
  }
  new_tplyr_table(target, enquo(treat_var))
}

#' @importFrom rlang env
#' @noRd
new_tplyr_table <- function(target, treat_var) {
  validate_tplyr_table(target)

  # Create table object with default bindings and class of `tplyr_table`
  structure(rlang::env(
    target = target,
    layers = structure(list(),
                       class = c("tplyr_layer_container", "list"))
  ), class = c("tplyr_table", "environment")) %>%
    # Set default bindings with standard setter methods
    set_tplyr_treat_var(!!treat_var) %>%
    set_tplyr_pop_data(target) %>%
    set_tplyr_pop_treat_var(!!treat_var) %>%
    # header_n is set with a default here instead of standard function
    default_header_n()

}

#' @importFrom assertthat assert_that
#' @noRd
validate_tplyr_table <- function(target) {

  # table should be a data.frame
  assertthat::assert_that(inherits(target, "data.frame"),
                          msg = paste0("'pop_data' argument passed to tplyr_table must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(target),
                                       "' was passed."))
}


