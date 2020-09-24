

#' Rebuild the header_n to include treatment groups
#'
#' This is exactly the same as default header_n execpt it works on the built
#' pop_data.
#'
#' @noRd
build_header_n <- function(table) {
  evalq({

    # Error out if the cols variables around found in the pop_data
    assert_quo_var_present(cols, names(built_pop_data))

    # If there is a distinct_by, use it to make the header_n
    if(is.null(distinct_by)) {
      df <- built_pop_data %>%
        group_by(!!pop_treat_var, !!!cols) %>%
        tally() %>%
        ungroup() %>%
        complete(!!pop_treat_var, !!!cols, fill = list(n = 0))
    } else {
      df <- built_pop_data %>%
        distinct(!!!distinct_by, .keep_all = TRUE) %>%
        group_by(!!pop_treat_var, !!!cols) %>%
        tally() %>%
        ungroup() %>%
        complete(!!pop_treat_var, !!!cols, fill = list(n = 0))
    }

    header_n <- df
    rm(df)
  }, envir = table)
  table
}

#' Combine existing treatment groups for summary
#'
#' Summary tables often present individual treatment groups,
#' but may additionally have a "Treatment vs. Placebo" or "Total" group added
#' to show grouped summary statistics or counts. This set of functions offers
#' an interface to add these groups at a table level and be consumed by
#' subsequent layers.
#'
#' \code{add_treat_grps} allows you to specify specific groupings. This is done
#' by supplying named arguments, where the name becomes the new treatment group's
#' name, and those treatment groups are made up of the argument's values.
#'
#' \code{add_total_group} is a simple wrapper around \code{add_treat_grps}. Instead of
#' producing custom groupings, it produces a "Total" group by the supplied name, which
#' defaults to "Total". This "Total" group is made up of all existing treatment
#' groups within the population dataset.
#'
#' The function \code{treat_grps} allows you to see the custom treatment groups available
#' in your \code{tplyr_table} object
#'
#' @param table A \code{tplyr_table} object
#' @param ... A named vector where names will become the new treatment group names,
#' and values will be used to construct those treatment groups
#'
#' @return The modified table object
#' @export
#' @rdname treat_grps
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' # A custom group
#' add_treat_grps(tab, "Not Setosa" = c("versicolor", "virginica"))
#'
#' # Add a total group
#' add_total_group(tab)
#'
#' treat_grps(tab)
#' # Returns:
#' # $`Not Setosa`
#' #[1] "versicolor" "virginica"
#' #
#' #$Total
#' #[1] "setosa"     "versicolor" "virginica"

add_treat_grps <- function(table, ...) {

  assert_that(is_named(list(...)), msg="Treatment group arguments must have names")

  assert_that(inherits(table, "tplyr_table"),
                   msg = "Treatment groups can only be added to `tplyr_table` objects")

  # Check parameters
  fargs <- list(...)

    # Bind the specified treatment groups to the table
  env_bind(table, treat_grps = append(treat_grps(table), fargs))

  table
}

#' @param group_name The treatment group name used for the constructed 'Total' group
#'
#' @export
#' @rdname treat_grps
add_total_group <- function(table, group_name="Total") {

  assert_has_class(group_name, "character")

  # Temporarily bind the group_name parameter to the table environment
  env_bind(table, .tmp_name = group_name)

  evalq({
    # Create the function arguments and gather the list of all available treatment groups
    treat_args <- list(current_env(), as.character(unlist(unique(pop_data[, quo_name(pop_treat_var)]))))
    # Name the arguments
    names(treat_args) <- c("table", .tmp_name)
    # Call add_treat_grps with the derived arguments
    do.call(add_treat_grps, treat_args)
    # Remove the temporary variable
    rm(.tmp_name, treat_args)
  }, envir = table)
  table
}
