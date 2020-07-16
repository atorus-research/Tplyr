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

    # Execute over each set of comparisons
    for (comp in comparisons) {
      # Prep the two-way data
      .dat <- prep_two_way(envir=current_env())


    }
    .dat

  }, envir=x)
}
