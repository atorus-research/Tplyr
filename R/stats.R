#' Process a tplyr_statistic object
#'
#' This is an internal function that is not meant for use externally, but must be exported.
#' Use with caution.
#'
#' @param x A tplyr_statistic environment
#' @param ... Additional pass through parameters
#'
#' @return Numeric statistc data from a tplyr statistc
#' @export
#' @keywords internal
process_statistic_data <- function(x, ...) {
  UseMethod('process_statistic_data')
}

#' Risk difference numeric processing
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from statistic environment
#' 2. Processes risk difference calculations in function environment
#' 3. Binds results back to statistic environment
#'
#' @param x a tplyr_statistic object
#' @param ... pass through parameters
#'
#' @return numeric risk difference data
#' @noRd
#' @export
process_statistic_data.tplyr_riskdiff <- function(x, ...) {

  # EXTRACT: Get what we need from the statistic environment
  # Note: The statistic environment inherits from the layer environment
  # Some variables are in the statistic env, others in the parent layer env
  comparisons <- x$comparisons
  args <- x$args
  comp_distinct <- x$comp_distinct
  
  # These come from the parent layer environment
  by <- env_get(x, "by", default = NULL, inherit = TRUE)
  cols <- env_get(x, "cols", default = NULL, inherit = TRUE)
  numeric_data <- env_get(x, "numeric_data", default = NULL, inherit = TRUE)
  treat_var <- env_get(x, "treat_var", default = NULL, inherit = TRUE)
  pop_treat_var <- env_get(x, "pop_treat_var", default = NULL, inherit = TRUE)
  header_n <- env_get(x, "header_n", default = NULL, inherit = TRUE)
  target_var <- env_get(x, "target_var", default = NULL, inherit = TRUE)
  distinct_by <- env_get(x, "distinct_by", default = NULL, inherit = TRUE)
  is_built_nest <- env_get(x, "is_built_nest", default = FALSE, inherit = TRUE)
  
  # PROCESS: Work in function environment
  comp_numeric_data <- vector('list', length(comparisons))
  trans_numeric_data <- vector('list', length(comparisons))

  # Execute over each set of comparisons
  for (i in seq_along(comparisons)) {
    comp <- comparisons[[i]]
    
    # Prep the two-way data - pass all needed variables explicitly
    two_way_data <- prep_two_way_riskdiff(
      comp = comp,
      numeric_data = numeric_data,
      treat_var = treat_var,
      pop_treat_var = pop_treat_var,
      cols = cols,
      header_n = header_n,
      by = by,
      is_built_nest = is_built_nest,
      comp_distinct = comp_distinct,
      distinct_by = distinct_by,
      target_var = target_var
    )
    
    # Calculate the risk-difference and form the data frame
    comp_numeric_data[[i]] <- two_way_data %>%
      pmap_dfr(riskdiff, args=args)

    # Put in the group name
    names(comp_numeric_data)[[i]] <- paste0(comp, collapse="_")

    # Create a numeric copy of the data in long form
    trans_numeric_data[[i]] <- comp_numeric_data[[i]] %>%
      # Pivot all of the measures into long form, rename group to the value column name
      pivot_longer(cols = tail(names(comp_numeric_data[[i]]), 5),
                   names_to='measure',
                   values_to=paste0(comp, collapse="_"))

  }

  # Join each of the comparisons together
  stats_numeric_data <- reduce(trans_numeric_data,
                               full_join,
                               by=c(match_exact(c(by, cols)),'summary_var', 'measure'))

  # BIND: Write results back to statistic environment
  x$comp_numeric_data <- comp_numeric_data
  x$stats_numeric_data <- stats_numeric_data
  
  stats_numeric_data
}

#' Process string formatting on a tplyr_statistic object
#'
#' This is an internal function that is not meant for use externally, but must be exported.
#' Use with caution.
#'
#' @param x A tplyr_statistic environment
#' @param ... Additional pass through parameters
#'
#' @return Formatted tplyr_statistic data
#' @export
#' @keywords internal
process_statistic_formatting <- function(x, ...) {
  UseMethod('process_statistic_formatting')
}

#' Risk difference string formatting
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from statistic environment
#' 2. Processes formatting in function environment
#' 3. Binds results back to statistic environment
#'
#' @param x A tplyr_statistc object
#' @param ... Pass through paramters
#'
#' @return Formatted risk difference data
#' @noRd
#' @export
process_statistic_formatting.tplyr_riskdiff <- function(x, ...) {

  # EXTRACT: Get what we need from the statistic environment
  comp_numeric_data <- x$comp_numeric_data
  
  # Get these from the parent layer environment
  format_strings <- env_get(x, "format_strings", default = list(), inherit = TRUE)
  by <- env_get(x, "by", default = NULL, inherit = TRUE)
  cols <- env_get(x, "cols", default = NULL, inherit = TRUE)
  target_var <- env_get(x, "target_var", default = NULL, inherit = TRUE)
  
  # PROCESS: Work in function environment
  
  # Grab the default format string
  if (!"riskdiff" %in% names(format_strings)) {
    format_strings[['riskdiff']] <- gather_defaults(env_parent(x))[['riskdiff']]
  }

  # Grab the format string object
  fmt <- format_strings$riskdiff

  # Prepare the formatted datasets
  formatted_statistic_data <- comp_numeric_data

  for (name in names(comp_numeric_data)) {

    # Construct the display string from the numeric variables
    display_string <- comp_numeric_data[[name]] %>%
      pmap_chr(construct_riskdiff_string, .fmt_str = fmt)

    # Pick off all the labels
    formatted_statistic_data[[name]] <- formatted_statistic_data[[name]] %>%
      select(summary_var, !!!head(target_var, -1), map_chr(by, as_label) , !!!cols)

    # Put the display string in
    formatted_statistic_data[[name]][paste0('rdiff_', name)] <- display_string

  }

  # Join the rdiff columns together
  formatted_statistic_data <- reduce(formatted_statistic_data,
                                     full_join,
                                     by=c(match_exact(c(by, cols, head(target_var, -1))),  'summary_var'))

  if (length(cols) > 0) {

    # If only one comparison was made, the columns won't prefix with the transposed variable name
    # So trick it by introducing a column I can drop later. Not great, but functional
    formatted_statistic_data['rdiffx'] <- ''

    # Pivot by column
    formatted_statistic_data <- formatted_statistic_data %>%
      pivot_wider(id_cols=c(match_exact(c(by, head(target_var, -1))),  'summary_var'),
                  names_from = match_exact(cols),
                  names_sep = "_",
                  values_from=starts_with('rdiff'))

    # Drop the dummied columns
    formatted_statistic_data <- formatted_statistic_data %>% select(-starts_with('rdiffx'))

  }

  # BIND: Write results back to statistic environment
  x$formatted_statistic_data <- formatted_statistic_data
  
  formatted_statistic_data
}

