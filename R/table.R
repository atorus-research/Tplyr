### Table Constructor


#' Create a Tplyr table object
#'
#' The \code{tplyr_table} object is the main container upon which a Tplyr table is constructed. Tplyr tables are made up of
#' one or more layers. Each layer contains an instruction for a summary to be performed. The \code{tplyr_table} object contains
#' those layers, and the general data, metadata, and logic necessary.
#'
#' @details
#' When a \code{tplyr_table} is created, it will contain the following bindings:
#' \itemize{
#' \item{target - The dataset upon which summaries will be performed}
#' \item{pop_data - The data containing population information. This defaults to the target dataset}
#' \item{cols - A categorical variable to present summaries grouped by column (in addition to treat_var)}
#' \item{table_where - The \code{where} parameter provided, used to subset the target data}
#' \item{treat_var - Variable used to distinguish treatment groups.}
#' \item{header_n - Default header N values based on \code{treat_var}}
#' \item{pop_treat_var - The treatment variable for \code{pop_data} (if different)}
#' \item{layers - The container for individual layers of a \code{tplyr_table}}
#' \item{treat_grps - Additional treatment groups to be added to the summary (i.e. Total)}
#' }
#'
#' \code{tplyr_table} allows you a basic interface to instantiate the object. Modifier functions are available to change
#' individual parameters catered to your analysis. For example, to add a total group, you can use the
#' \code{\link{add_total_group}}.
#'
#' In future releases, we will provide vignettes to fully demonstrate these capabilities.
#'
#' @param target Dataset upon which summaries will be performed
#' @param treat_var Variable containing treatment group assignments. Supply unquoted.
#' @param where A general subset to be applied to all layers. Supply as programming logic (i.e. x < 5 & y == 10)
#' @param cols A grouping variable to summarize data by column (in addition to treat_var). Provide multiple
#' column variables by using \code{\link[dplyr]{vars}}
#'
#' @return A \code{tplyr_table} object
#' @export
#'
#' @examples
#'
#' tab <- tplyr_table(iris, Species, where = Sepal.Length < 5.8)
#'
tplyr_table <- function(target, treat_var, where = TRUE, cols = vars()) {
  target_name <- enexpr(target)
  new_tplyr_table(target, enquo(treat_var), enquo(where), enquos(cols), target_name)
}

#' Construct new tplyr_table
#'
#' @inheritParams tplyr_table
#' @noRd
new_tplyr_table <- function(target, treat_var, where, cols, target_name) {
  cols <- unpack_vars(cols)

  validate_tplyr_table(target, cols)

  # Create table object with default bindings and class of `tplyr_table`
  table_ <- structure(rlang::env(
    target = target,
    treat_grps = list(),
    cols = cols,
    layers = structure(list(),
                       class = c("tplyr_layer_container", "list"))
  ), class = c("tplyr_table", "environment"))
  attr(table_, "target_name") <- target_name

  table_ <- table_ %>%
    # Set default bindings with standard setter methods
    set_treat_var(!!treat_var) %>%
    set_pop_data(target) %>%
    set_pop_treat_var(!!treat_var) %>%
    set_where(!!where) %>%
    set_pop_where(!!where) %>%
    set_desc_layer_formats() %>%
    set_count_layer_formats() %>%
    set_shift_layer_formats()



  table_
}

#' Validate tplyr_table target dataset
#'
#' Most validation is done in the binding functions to reduce code duplication
#'
#' @param target target dataset passed from new_tplyr_table
#' @param cols cols argument passed from new_tplyr_table
#'
#' @noRd
validate_tplyr_table <- function(target, cols) {

  # table should be a data.frame
  assertthat::assert_that(inherits(target, "data.frame"),
                          msg = paste0("'pop_data' argument passed to tplyr_table must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(target),
                                       "' was passed."))

  assert_quo_var_present(cols, names(target), allow_character = FALSE)
}


