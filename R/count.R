#' Process a layer of type \code{group_count}
#'
#' @param e A layer environment
#'
#' @return A
#' @export
#'
#' @examples
process_count_layer <- function(e) {
  evalq({

    summary_stat <- target %>%
      # filter(!!where) %>%
      group_by(!!treat_var, syms(treat_grps), !!!by) %>%
      # Tally here is used to get any missing values
      summarize(count = tally())

  }, envir = e)
}
