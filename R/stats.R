#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
process_statistic_data <- function(x, ...) {
  UseMethod('process_statistic_data')
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
process_statistic_data.tplyr_riskdiff <- function(x, ...) {

  evalq({

    comp_numeric_data <- vector('list', length(comparisons))
    trans_numeric_data <- vector('list', length(comparisons))

    # Execute over each set of comparisons
    for (i in seq_along(comparisons)) {
      comp <- comparisons[[i]]
      # Prep the two-way data
      comp_numeric_data[[i]] <- prep_two_way() %>%
        # Calculate the risk-difference and form the data frame
        pmap_dfr(riskdiff, args=args)

      # Create a numeric copy of the data in long form
      trans_numeric_data[[i]] <- comp_numeric_data[[i]] %>%
        # Pivot all of the measures into long form, rename group to the value column name
        pivot_longer(cols = tail(names(comp_numeric_data[[i]]), 5),
                     names_to='measure',
                     values_to=unique(comp_numeric_data[[i]]$group)) %>%
        select(-group)

    }

    # Join each of the comparisons together
    stats_numeric_data <- reduce(trans_numeric_data, full_join, by=c(match_exact(append(by, target_var)), 'measure'))

  }, envir=x)
}
