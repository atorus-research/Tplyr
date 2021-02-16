# Functions to calculate risk difference

#' Add risk difference to a count layer
#'
#' A very common requirement for summary tables is to calculate the risk difference between treatment
#' groups. \code{add_risk_diff} allows you to do this. The underlying risk difference calculations
#' are performed using the Base R function \code{\link{prop.test}} - so prior to using this function,
#' be sure to familiarize yourself with its functionality.
#'
#' \code{add_risk_diff} can only be attached to a count layer, so the count layer must be constructed
#' first. \code{add_risk_diff} allows you to compare the difference between treatment group, so all
#' comparisons should be based upon the values within the specified \code{treat_var} in your
#' \code{tplyr_table} object.
#'
#' Comparisons are specified by providing two-element character vectors. You can provide as many of
#' these groups as you want. You can also use groups that have been constructed using
#' \code{\link{add_treat_grps}} or \code{\link{add_total_group}}. The first element provided will be considered
#' the 'reference' group (i.e. the left side of the comparison), and the second group will be considered
#' the 'comparison'. So if you'd like to see the risk difference of 'T1 - Placebo', you would specify
#' this as \code{c('T1', 'Placebo')}.
#'
#' Tplyr forms your two-way table in the background, and then runs \code{\link{prop.test}} appropriately.
#' Similar to way that the display of layers are specified, the exact values and format of how you'd like
#' the risk difference display are set using \code{\link{set_format_strings}}. This controls both the values
#' and the format of how the risk difference is displayed. Risk difference formats are set within
#' \code{\link{set_format_strings}} by using the name 'riskdiff'.
#'
#' You have 5 variables to choose from in your data presentation:
#' \describe{
#'   \item{\strong{comp}}{Probability of the left hand side group (i.e. comparison)}
#'   \item{\strong{ref}}{Probability of the right hand side group (i.e. reference)}
#'   \item{\strong{dif}}{Difference of comparison - reference}
#'   \item{\strong{low}}{Lower end of the confidence interval (default is 95\%, override with the \code{args} paramter)}
#'   \item{\strong{high}}{Upper end of the confidence interval (default is 95\%, override with the \code{args} paramter)}
#' }
#'
#' Use these variable names when forming your \code{\link{f_str}} objects. The default presentation, if no
#' string format is specified, will be:
#'
#'   \code{f_str('xx.xxx (xx.xxx, xx.xxx)', dif, low, high)}
#'
#' Note - within Tplyr, you can account for negatives by allowing an extra space within your integer
#' side settings. This will help with your alignment.
#'
#' If columns are specified on a Tplyr table, risk difference comparisons still only take place between
#' groups within the \code{treat_var} variable - but they are instead calculated treating the \code{cols}
#' variables as by variables. Just like the tplyr layers themselves, the risk difference will then be transposed
#' and display each risk difference as separate variables by each of the \code{cols} variables.
#'
#' If \code{distinct} is TRUE (the default), all calculations will take place on the distinct counts, if
#' they are available. Otherwise, non-distinct counts will be used.
#'
#' One final note - \code{\link{prop.test}} may throw quite a few warnings. This is natural, because it
#' alerts you when there's not enough data for the approximations to be correct. This may be unnerving
#' coming from a SAS programming world, but this is R is trying to alert you that the values provided
#' don't have enough data to truly be statistically accurate.
#'
#' @param layer Layer upon which the risk difference will be attached
#' @param ... Comparison groups, provided as character vectors where the first group is the comparison,
#' and the second is the reference
#' @param args Arguments passed directly into \code{\link{prop.test}}
#' @param distinct Logical - Use distinct counts (if available).
#'
#' @export
#'
#' @examples
#' library(magrittr)
#'
#' ## Two group comparisons with default options applied
#' t <- tplyr_table(mtcars, gear)
#'
#' # Basic risk diff for two groups, using defaults
#' l1 <- group_count(t, carb) %>%
#'   # Compare 3 vs. 4, 3 vs. 5
#'   add_risk_diff(
#'     c('3', '4'),
#'     c('3', '5')
#'   )
#'
#' # Build and show output
#' add_layers(t, l1) %>% build()
#'
#' ## Specify custom formats and display variables
#' t <- tplyr_table(mtcars, gear)
#'
#' # Create the layer with custom formatting
#' l2 <- group_count(t, carb) %>%
#'   # Compare 3 vs. 4, 3 vs. 5
#'   add_risk_diff(
#'     c('3', '4'),
#'     c('3', '5')
#'   ) %>%
#'   set_format_strings(
#'     'n_counts' = f_str('xx (xx.x)', n, pct),
#'     'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
#'   )
#'
#' # Build and show output
#' add_layers(t, l2) %>% build()
#'
#' ## Passing arguments to prop.test
#' t <- tplyr_table(mtcars, gear)
#'
#' # Create the layer with args option
#' l3 <- group_count(t, carb) %>%
#'   # Compare 3 vs. 4, 4 vs. 5
#'   add_risk_diff(
#'     c('3', '4'),
#'     c('3', '5'),
#'     args = list(conf.level = 0.9, correct=FALSE, alternative='less')
#'   )
#'
#' # Build and show output
#' add_layers(t, l3) %>% build()
add_risk_diff <- function(layer, ..., args=list(), distinct=TRUE) {

  # grab the ellipsis args into a list
  comps <- list(...)

  # Must be character, must have 2 elements
  assert_that(all(map_lgl(comps, is.character)), all(map_lgl(comps, ~ length(.x) == 2)),
              msg="Comparisons provided must be two-element character vectors")

  assert_that(all(names(args) %in% c('p', 'alternative', 'conf.level', 'correct')),
              msg = "All arguments provided via `args` must be valid arguments of `prop.test`")

  # Risk diff must be run on count layers
  assert_that(inherits(layer, 'count_layer'), msg = "Risk difference can only be applied to a count layer.")

  # Package up the environment
  rd <- structure(
      env(
        layer,
        comparisons = comps,
        args = args,
        distinct = distinct
      ),
      class=c("tplyr_statistic", "tplyr_riskdiff")
    )

  # Add to the stats container
  layer$stats <- append(layer$stats, list(riskdiff = rd))

  layer
}

#' Prepare a two-way table
#'
#' @param e Environment two way table is being prepped from
#' @param ref_comp The reference and comparison group
#'
#' @return A dataframe containing the necessary two-way table data on the same row
#'
#' @noRd
prep_two_way <- function(comp) {

  # Make sure the function is executing in a Tplyr statistic environment
  # assert_that(inherits(env_parent(), "tplyr_statistic"),
  #             msg = paste("This function is only intended to run on `tplyr_statistic` environments.",
  #                         "Do not use in other contexts."))

  evalq({

    # Make sure that the comparisons issued actually exist within the data
    invalid_groups <- comp[!comp %in% unique(numeric_data[as_name(treat_var)])[[1]]]
    assert_that(length(invalid_groups) == 0,
                msg = paste0("There are no records for the following groups within the variable ", as_name(treat_var),
                             ": ", paste(invalid_groups, collapse=", ")))

    two_way <- numeric_data

    # Nested layers need to plug the NAs left over - needs revision in the future
    if (is_built_nest) {
      two_way <- two_way %>%
        # Need to fill in NAs in the numeric data that
        # are patched later in formatting
        mutate(
          !!by[[1]] := ifelse(is.na(!!by[[1]]), summary_var, !!by[[1]])
        )
    }


    # If distinct is set and distinct values are there, use them
    if (distinct == TRUE && any(str_detect(names(two_way), 'distinct'))) {
      two_way <- two_way %>%
        select(-n, -total) %>%
        rename(n = distinct_n, total = distinct_total)
    }
    # Process on the numeric data
    two_way <- two_way %>%
      # Subset down to only treatments with the ref and comp groups
      filter(!!treat_var %in% comp) %>%
      # Rename the treatment groups to ref and comp
      mutate(!!treat_var := case_when(
        !!treat_var == comp[1] ~ 'comp',
        !!treat_var == comp[2] ~ 'ref'
      )) %>%
      # Pivot out to give the var names n_ref, n_comp, total_ref, total_comp for two way
      pivot_wider(id_cols = c(match_exact(c(by, cols, head(target_var, -1))),  'summary_var'),
                  names_from=!!treat_var,
                  values_from = c('n', 'total'))

  }, envir=caller_env())

}

#' Calculate risk difference
#'
#' This function is intended to be called using \code{pmap_dfr} against the prepared two-day data
#' from \code{prep_two_way}
#'
#' @param diff_group The concateations of the reference and comparison groups
#' @param n_comp The count of of the comparison
#' @param n_ref The count of the reference
#' @param total_comp The total of the comp
#' @param total_ref The total of the reference
#' @param args Arguments that will be passed into prop.test, provided as a named list
#' @param ...
#'
#' @return  A dataframe containing the group, the proportions of each comparator, the difference,
#' and the lower and upper CI
#'
#' @noRd
#'
riskdiff <- function(diff_group, n_comp, n_ref, total_comp, total_ref, args=list(), ...) {

  # Create output container with initial values
  out <- list(
    comp = NA,
    ref = NA,
    dif = NA,
    low = NA,
    high = NA
  )

  out <- append(list(...), out)
  # Rename

  # Totals in the 2 way must be positive
  if (all(c(total_comp, total_ref) > 0)) {

    # Run the risk difference
    test <- do.call('prop.test', append(list(x=c(n_comp, n_ref), n=c(total_comp, total_ref)), args))

    # Collect results into standardized format
    out$comp = unname(test$estimate[1])
    out$ref = unname(test$estimate[2])
    out$dif = unname(test$estimate[1] - test$estimate[2])
    out$low = unname(test$conf.int[1])
    out$high = unname(test$conf.int[2])
  }

  # Return as a dataframe
  as.data.frame(out, stringsAsFactors=FALSE, optional = TRUE)
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
