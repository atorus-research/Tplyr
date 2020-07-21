# Functions to calculate risk difference

#' Title
#'
#' @param layer Layer upon which the risk difference will be attached
#' @param ... Comparison groups, provided as character vectors where the first group is the comparison,
#' and the second is the reference
#'
#' @export
#'
#' @examples
add_risk_diff <- function(layer, ..., args=list()) {

  comps <- list(...)

  assert_that(all(map_lgl(comps, is.character)),
              msg="Comparisons provided must be character vectors")


  # Risk diff must be run on count layers
  assert_inherits_class(layer, 'count_layer')

  # Package up the
  rd <- structure(
      env(
        layer,
        comparisons = comps,
        args = args
      ),
      class=c("tplyr_statistic", "tplyr_riskdiff")
    )

  layer$stats <- append(layer$stats, rd)

  layer
}

#' Title
#'
#' @param e Environment two way table is being prepped from
#' @param ref_comp The reference and comparison group
#'
#' @return A dataframe containing the necessary two-way table data on the same row
#'
#' @examples
prep_two_way <- function(comp) {

  # Make sure the function is executing in a Tplyr statistic environment
  # assert_that(inherits(env_parent(), "tplyr_statistic"),
  #             msg = paste("This function is only intended to run on `tplyr_statistic` environments.",
  #                         "Do not use in other contexts."))

  evalq({
    # Process on the numeric data
    numeric_data %>%
      # Subset down to only treatments with the ref and comp groups
      filter(!!treat_var %in% comp) %>%
      mutate(!!treat_var := case_when(
        !!treat_var == comp[1] ~ 'ref',
        !!treat_var == comp[2] ~ 'comp'
      )) %>%
      pivot_wider(id_cols = match_exact(c(by, cols, target_var)),
                  names_from=!!treat_var,
                  values_from = c('value', 'total'))

  }, envir=caller_env())

}

#' Calculate risk difference
#'
#' This function is intended to be called using \code{pmap_dfr} against the prepared two-day data
#' from \code{prep_two_way}
#'
#' @param diff_group The concateations of the reference and comparison groups
#' @param value_comp The count of of the comparison
#' @param value_ref The count of the reference
#' @param total_comp The total of the comp
#' @param total_ref The total of the reference
#' @param args Arguments that will be passed into prop.test, provided as a named list
#' @param ...
#'
#' @return  A dataframe containing the group, the proportions of each comparator, the difference,
#' and the lower and upper CI
#'
riskdiff <- function(diff_group, value_comp, value_ref, total_comp, total_ref, args=list(), ...) {

  assert_that(all(names(args) %in% c('p', 'alternative', 'conf.level', 'correct')),
              msg = "All arguments provided via `args` must be valid arguments of `prop.test`")

  # Create output container with initial values
  out <- list(
    prop1 = NA,
    prop2 = NA,
    dif = NA,
    low = NA,
    high = NA
  )

  out <- append(list(...), out)

  # Totals in the 2 way must be positive
  if (all(c(total_comp, total_ref) > 0)) {

    # Run the risk difference
    test <- do.call('prop.test', append(list(x=c(value_comp,value_ref), n=c(total_comp, total_ref)), args))

    # Collect results into standardized format
    out$prop1 = unname(test$estimate[1])
    out$prop2 = unname(test$estimate[2])
    out$dif = unname(test$estimate[1] - test$estimate[2])
    out$low = unname(test$conf.int[1])
    out$high = unname(test$conf.int[2])
  }

  as.data.frame(out, stringsAsFactors=FALSE)
}

construct_riskdiff_string <- function(..., .fmt_str=NULL) {
  # Unpack names into current namespace for ease
  list2env(list(...), envir=environment())

  # Return empty when necessary
  if (any(is.na(list(...)))) {
    return(.fmt_str$empty)
  }

  # Start constructing the arguments to call
  fmt_args <- list(fmt = .fmt_str$repl_str)

  # Grab the num formatting for each value in the format string
  fmt_args <- append(fmt_args, imap(.fmt_str$vars,
                                    function(val, i, fmt) num_fmt(eval(val), i, fmt),
                                    fmt=.fmt_str))

  # Apply the call to sprintf
  do.call(sprintf, fmt_args)
}
