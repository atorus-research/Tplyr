#' Set Summaries
#'
#' If Tplyr on only being used to summarize numeric data, then layer formatting
#' components of the settings become unnecessary. As such, a second interface is
#' necessary to specify which summaries should actually be performed. the
#' `set_summaries()` function provided an interface to each layer type to
#' establish the numbers that must be calculated.
#'
#' @param e Layer upon which summaries should be bound
#' @param ... Named parameters containing lists of quosures created using `dplyr::vars()`
#'
#' @return The layer environment with the summary variables binding applied
#' @export
#' @md
#'
#' @rdname set_summaries
#'
#' @examples
#' # This is the desired API for count layers
#' t <- tplyr_table(adsl, TRT01P) %>%
#'   add_layer(
#'     group_desc(AGE, by = "Age (years)", where= SAFFL=="Y") %>%
#'       set_summaries(
#'         "n"        = vars(n),
#'         "Mean (SD)"= vars(mean, sd),
#'         "Median"   = vars(median),
#'         "Q1, Q3"   = vars(q1, q3),
#'         "Min, Max" = vars(min, max),
#'         "Missing"  = vars(missing)
#'       )
#'   )
#'
set_summaries  <- function(e, ...) {
  UseMethod("set_summaries")
}

#' Set summaries for descriptive stats layer
#'
#' @export
#' @rdname set_summaries
#'
set_summaries.tplyr_layer <- function(e, ...) {

  # Must be named to create row labels
  # Element must all be lists of quosures

  # Create the summary_vars object
  summaries <- list(...)
  summary_vars <- flatten(summaries)

  # Translate names to preserve row label -> This should go in process_summaries
  # From current process_summaries.desc_layer: row_labels <- name_translator(format_strings)

  env_bind(e,
           summary_grps = summaries,
           summary_vars = vars(!!!summary_vars),
           using_f_strs = FALSE
  )

  e
}
