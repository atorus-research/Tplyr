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

    summary_stat <- built_target %>%
      filter(!!where) %>%
      group_by(!!treat_var, !!!target_var, !!!by) %>%
      tally() %>%
      ungroup() %>%
      complete(!!treat_var, !!!target_var, !!!by, fill = list(n = 0))

    # if(quo_is_null(cols[[1]])) {
      built_table <- summary_stat %>%
        pivot_wider(id_cols = c(match_exact(by), match_exact(target_var)), names_from = c(!!treat_var, match_exact(cols)), values_from = n)
    # } else {
    #
    #
    # }
  }, envir = e)
}



