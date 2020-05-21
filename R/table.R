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
#' \item{target - The main piece of data the table will be created from.}
#' \item{headers - The column headers for the table. These will be repeated
#'   across pages as appropriate.}
#' \item{pop_data - The data.frame containing population data.}
#' \item{treat_var - The treatment variable arms/sets are split up on.}
#' \item{layers - The logic used to prepare and render the table. Stored as a
#'   list binded to the environment. Use <add layer function> to add a layer.}
#' }
#'
#' @param target Source data used to create the table. A 'data.frame' object.
#'
#' @return A safety_table object which is an environment where the code
#'   creating the table is evaluated.
#'
#' @importFrom rlang env
#' @importFrom magrittr %>%
#' @seealso [layer()]
#'
#' @examples
#' df <- data.frame(a = 1:9, b = 10:18, c = rep(c("Placebo", "Low", "High"), 3))
#' adsl <- data.frame(d = letters[1:9])
#'
#' tab <- tplyr_table(df) %>%
#'   set_tplyr_treat_var(c) %>%
#'   set_tplyr_pop_data(adsl) %>%
#'   set_tplyr_header(c("Placebo", "Low", "High"))
#' @export
tplyr_table <- function(target) {
  if(missing(target)){
    #return a blank environment if no table information is passed
    return(structure(rlang::env(),
           class = c("tplyr_table", "environment")))
  }
  new_tplyr_table(target)
}

#' @importFrom rlang env
#' @noRd
new_tplyr_table <- function(target) {
  validate_tplyr_table(target)

  structure(rlang::env(
    target = target,
    layers = list()
  ),
           class = append("tplyr_table", "environment"))
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


