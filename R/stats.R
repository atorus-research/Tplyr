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
                                 by=c(match_exact(append(by, target_var)), 'measure'))

    stats_numeric_data

  }, envir=x)
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
process_statistic_formatting <- function(x, ...) {
  UseMethod('process_statistic_formatting')
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
process_statistic_formatting.tplyr_riskdiff <- function(x, ...) {

  evalq({

    if (!"riskdiff" %in% names(format_strings)) {
      format_strings[['riskdiff']] <- f_str('x.xxx (x.xxx, x.xxx)', dif, low, high)
    }

    fmt <- format_strings$riskdiff

    formatted_statistic_data <- vector('list', length(comparisons))

    for (name in names(comp_numeric_data)) {

      display_string <- comp_numeric_data[[name]] %>%
        pmap_chr(construct_riskdiff_string, .fmt_str = fmt)

    }

    display_string


  }, envir=x)
}

