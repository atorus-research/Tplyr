#' Add a Total row into a count summary.
#'
#' Adding a total row creates an additional observation in the count summary
#' that presents the total counts (i.e. the n's that are summarized). The format
#' of the total row will be formatted in the same way as the other count
#' strings.
#'
#' Totals are calculated using all grouping variables, including treat_var and
#' cols from the table level. If by variables are included, the grouping of the
#' total and the application of denominators becomes ambiguous. You will be
#' warned specifically if a percent is included in the format. To rectify this,
#' use \code{set_denoms_by()}, and the grouping of \code{add_total_row()} will
#' be updated accordingly.
#'
#' @param e A \code{count_layer} object
#' @param fmt An f_str object used to format the total row. If none is provided,
#'   display is based on the layer formatting.
#' @param count_missings Whether or not to ignore the named arguments passed in
#'   `set_count_missing()` when calculating counts total row. This is useful if
#'   you need to exclude/include the missing counts in your total row. Defaults
#'   to TRUE meaning total row will not ignore any values.
#' @param sort_value The value that will appear in the ordering column for total
#'   rows. This must be a numeric value.
#'
#' @export
#' @examples
#' # Load in Pipe
#' library(magrittr)
#'
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       add_total_row(f_str("xxxx", n))
#'    ) %>%
#'    build()
add_total_row <- function(e, fmt = NULL, count_missings = TRUE, sort_value = NULL) {
  if(!is.null(fmt)) assert_inherits_class(fmt, "f_str")
  if(!is.null(sort_value)) assert_inherits_class(sort_value, "numeric")
  if("shift_layer" %in% class(e)) {
    rlang::abort("`add_total_row` for shift layers is not yet supported")
  }
  assert_inherits_class(e, "count_layer")


  env_bind(e, include_total_row = TRUE)
  env_bind(e, count_missings = count_missings)
  env_bind(e, total_count_format = fmt)
  env_bind(e, total_row_sort_value = sort_value)

  e
}

#' Set the label for the total row
#'
#' The row label for a total row defaults to "Total", however this can be
#' overriden using this function.
#'
#' @param e A \code{count_layer} object
#' @param total_row_label A character to label the total row
#'
#' @return The modified \code{count_layer} object
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       add_total_row() %>%
#'       set_total_row_label("Total Cyl")
#'   )
#' build(t)
set_total_row_label <- function(e, total_row_label) {

  assert_has_class(total_row_label, "character")
  assert_that(length(total_row_label) == 1)
  if("shift_layer" %in% class(e)) {
    rlang::abort("`set_total_row_label` for shift layers is not yet supported")
  }
  assert_inherits_class(e, "count_layer")

  env_bind(e, total_row_label = total_row_label)

  e
}

#' Set counts to be distinct by some grouping variable.
#'
#' In some situations, count summaries may want to see distinct counts by a
#' variable like subject. For example, the number of subjects in a population
#' who had a particular adverse event. `set_distinct_by` allows you to set
#' the by variables used to determine a distinct count.
#'
#' When a `distinct_by` value is set, distinct counts will be used by
#' default. If you wish to combine distinct and not distinct counts, you can
#' choose which to display in your `f_str()` objects using `n`,
#' `pct`, `distinct_n`, and `distinct_pct`. Additionally, denominators
#' may be presented using `total` and `distinct_total`
#'
#' @param e A `count_layer/shift_layer` object
#' @param distinct_by Variable(s) to get the distinct data.
#'
#' @return The layer object with
#' @export
#' @md
#'
#' @examples
#' #Load in pipe
#' library(magrittr)
#'
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_distinct_by(carb)
#'   ) %>%
#'   build()
set_distinct_by <- function(e, distinct_by) {

  distinct_by <- unpack_vars(enquos(distinct_by))

  assert_quo_var_present(distinct_by, envir = e)

  env_bind(e, distinct_by = distinct_by)

  e
}

#' Set the option to prefix the row_label values when a layer is formatted
#'
#' This is generally used internally with a nested count layer.
#'
#' @param e A \code{count_layer} object
#' @param count_row_prefix A character to prefix the row labels
#'
#' @return The modified count_layer environment
#' @noRd
set_count_row_prefix <- function(e, count_row_prefix) {

  assert_inherits_class(count_row_prefix, "character")

  assert_that(length(count_row_prefix) == 1)

  env_bind(e, count_row_prefix = count_row_prefix)

  e
}

#' Set the option to prefix the row_labels in the inner count_layer
#'
#' When a count layer uses nesting (i.e. triggered by \code{\link{set_nest_count}}),
#' the \code{indentation} argument's value will be used as a prefix for the inner layer's
#' records
#'
#' @param e A \code{count_layer} object
#' @param indentation A character to prefix the row labels in an inner
#'   count layer
#'
#' @return The modified count_layer environment
#' @export
set_indentation <- function(e, indentation) {

  assert_inherits_class(indentation, "character")

  assert_that(length(indentation) == 1)

  env_bind(e, indentation = indentation)

  e
}

#' Set the option to nest count layers
#'
#' If set to TRUE, the second variable specified in \code{target_var}
#' will be nested inside of the first variable. This allows you to create
#' displays like those commonly used in adverse event tables, where
#' one column holds both the labels of the outer categorical variable
#' and the inside event variable (i.e. AEBODSYS and AEDECOD).
#'
#' @param e A \code{count_layer} object
#' @param nest_count A logical value to set the nest option
#'
#' @return The modified layer
#' @export
set_nest_count <- function(e, nest_count) {

  assert_inherits_class(nest_count, "logical")
  assert_inherits_class(e, "count_layer")

  assert_that(length(nest_count) == 1)

  env_bind(e, nest_count = nest_count)

  e

}


#' Set the ordering logic for the count layer
#'
#' @description The sorting of a table can greatly vary depending on the
#'   situation at hand. For count layers, when creating tables like adverse
#'   event summaries, you may wish to order the table by descending occurrence
#'   within a particular treatment group. But in other situations, such as AEs
#'   of special interest, or subject disposition, there may be a specific order
#'   you wish to display values. Tplyr offers solutions to each of these
#'   situations.
#'
#'   Instead of allowing you to specify a custom sort order, Tplyr instead
#'   provides you with order variables that can be used to sort your table after
#'   the data are summarized. Tplyr has a default order in which the table will
#'   be returned, but the order variables will always persist. This allows you
#'   to use powerful sorting functions like \code{\link[dplyr]{arrange}}
#'   to get your desired order, and in double programming situations, helps your
#'   validator understand the how you achieved a particular sort order and where
#'   discrepancies may be coming from.
#'
#'   When creating order variables for a layer, for each 'by' variable Tplyr
#'   will search for a <VAR>N version of that variable (i.e. VISIT <-> VISITN,
#'   PARAM <-> PARAMN). If available, this variable will be used for sorting. If
#'   not available, Tplyr will created a new ordered factor version of that
#'   variable to use in alphanumeric sorting. This allows the user to control a
#'   custom sorting order by leaving an existing <VAR>N variable in your dataset
#'   if it exists, or create one based on the order in which you wish to sort -
#'   no custom functions in Tplyr required.
#'
#'   Ordering of results is where things start to differ. Different situations
#'   call for different methods. Descriptive statistics layers keep it simple -
#'   the order in which you input your formats using
#'   \code{\link{set_format_strings}} is the order in which the results will
#'   appear (with an order variable added). For count layers, Tplyr offers three
#'   solutions: If there is a <VAR>N version of your target variable, use that.
#'   If not, if the target variable is a factor, use the factor orders. Finally,
#'   you can use a specific data point from your results columns. The result
#'   column can often have multiple data points, between the n counts, percent,
#'   distinct n, and distinct percent. Tplyr allows you to choose which of these
#'   values will be used when creating the order columns for a specified result
#'   column (i.e. based on the \code{treat_var} and \code{cols} arguments). See
#'   the 'Sorting a Table' section for more information.
#'
#'   Shift layers sort very similarly to count layers, but to order your row
#'   shift variable, use an ordered factor.
#'
#' @param e A \code{count_layer} object
#' @param order_count_method The logic determining how the rows in the final
#'   layer output will be indexed. Options are 'bycount', 'byfactor', and
#'   'byvarn'.
#' @param break_ties In certain cases, a 'bycount' sort will result in conflicts
#'   if the counts aren't unique. break_ties will add a decimal to the sorting
#'   column so resolve conflicts. A character value of 'asc' will add a decimal
#'   based on the alphabetical sorting. 'desc' will do the same
#'   but sort descending in case that is the intention.
#'
#'
#' @section Sorting a Table: When a table is built, the output has several
#'   ordering(ord_) columns that are appended. The first represents the layer
#'   index. The index is determined by the order the layer was added to the
#'   table. Following are the indices for the by variables and the target
#'   variable. The by variables are ordered based on:
#'
#'   \enumerate{
#'   \item{The `by` variable is a factor in the target dataset}
#'   \item{If the variable isn't a factor, but has a <VAR>N variable (i.e. VISIT
#'   -> VISITN, TRT -> TRTN)}
#'   \item{If the variable is not a factor in the target dataset, it is coerced
#'   to one and ordered alphabetically.}
#'   }
#'
#'   The target variable is ordered depending on the type of layer. See more
#'   below.
#'
#' @section Ordering a Count Layer: There are many ways to order a count layer
#'   depending on the preferences of the table programmer. \code{Tplyr} supports
#'   sorting by a descending amount in a column in the table, sorting by a
#'   <VAR>N variable, and sorting by a custom order. These can be set using the
#'   `set_order_count_method` function. \describe{ \item{Sorting by a numeric
#'   count}{A selected numeric value from a selected column will be indexed
#'   based on the descending numeric value. The numeric value extracted defaults
#'   to 'n' but can be changed with `set_result_order_var`. The column selected
#'   for sorting defaults to the first value in the treatment group variable. If
#'   there were arguments passed to the 'cols' argument in the table those must
#'   be specified with `set_ordering_columns`.} \item{Sorting by a 'varn'
#'   variable}{If the treatment variable has a <VAR>N variable. It can be
#'   indexed to that variable.} \item{Sorting by a factor(Default)}{If a factor
#'   is found for the target variable in the target dataset that is used to
#'   order, if no factor is found it is coerced to a factor and sorted
#'   alphabetically.} \item{Sorting a nested count layer}{If two variables are
#'   targeted by a count layer, two methods can be passed to `set_order_count`.
#'   If two are passed, the first is used to sort the blocks, the second is used
#'   to sort the "inside" of the blocks. If one method is passed, that will be
#'   used to sort both.} }
#'
#' @section Ordering a Desc Layer: The order of a desc layer is mostly set
#'   during the object construction. The by variables are resolved and index
#'   with the same logic as the count layers. The target variable is ordered
#'   based on the format strings that were used when the layer was created.
#'
#'
#' @return Returns the modified layer object. The 'ord_' columns are added
#'   during the build process.
#' @export
#' @rdname ordering
#'
#' @examples
#' library(dplyr)
#'
#' # Default sorting by factor
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl)
#'   )
#' build(t)
#'
#' # Sorting by <VAR>N
#' mtcars$cylN <- mtcars$cyl
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_order_count_method("byvarn")
#'   )
#'
#' # Sorting by row count
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_order_count_method("bycount") %>%
#'       # Orders based on the 6 gear group
#'       set_ordering_cols(6)
#'   )
#'
#' # Sorting by row count by percentages
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_order_count_method("bycount") %>%
#'       set_result_order_var(pct)
#'   )
#'
#' # Sorting when you have column arguments in the table
#' t <- tplyr_table(mtcars, gear, cols = vs) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       # Uses the fourth gear group and the 0 vs group in ordering
#'       set_ordering_cols(4, 0)
#'   )
#'
#' # Using a custom factor to order
#' mtcars$cyl <- factor(mtcars$cyl, c(6, 4, 8))
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       # This is the default but can be used to change the setting if it is
#'       #set at the table level.
#'       set_order_count_method("byfactor")
#'   )
set_order_count_method <- function(e, order_count_method, break_ties = NULL) {

  assert_inherits_class(order_count_method, "character")

  assert_that(all(order_count_method %in% c("bycount", "byfactor", "byvarn")),
              msg = "Invalid input passed to set_order_count_method.
              Options are: 'bycount', 'byfactor', or 'byvarn'")

  assert_that(length(env_get(e, "target_var")) == length(order_count_method) ||
              length(order_count_method) == 1,
              msg = "The length of the order_count_method must be equal to the length of the target_var, or 1")

  assert_inherits_class(e, "count_layer")

  assert_that(is.null(break_ties) || length(env_get(e, "target_var")) == 2,
              msg = "break_ties argument can only be used with nested count layers")
  assert_that(is.null(break_ties) || break_ties %in% c("asc", "desc"),
              msg = "break_ties must be 'asc', 'desc', or NULL")

  env_bind(e, order_count_method = order_count_method)
  env_bind(e, break_ties = break_ties)

  e
}

#' @param ... Unquoted variables used to select the columns whose values will be
#'   extracted for ordering.
#'
#' @export
#' @rdname ordering
set_ordering_cols <- function(e, ...) {

  ordering_cols <- enquos(...)

  check_ordering_cols <- unpack_vars(ordering_cols, allow_character = FALSE)

  treat_var <- env_get(e, "treat_var", inherit = TRUE)
  cols <- env_get(e, "cols", inherit = TRUE)

  assert_that(length(check_ordering_cols) == length(c(treat_var, cols)),
              msg = "You need to pass a variable for each treat_var and cols variable.")

  assert_inherits_class(e, "count_layer")

  env_bind(e, ordering_cols = ordering_cols)

  e
}

#' @param result_order_var The numeric value the ordering will be done on.
#'   This can be either n, distinct_n, pct, or distinct_pct. Due to the
#'   evaluation of the layer you can add a value that isn't actually being
#'   evaluated, if this happens this will only error out in the ordering.
#'
#' @export
#' @rdname ordering
set_result_order_var <- function(e, result_order_var) {

  result_order_var <- enquo(result_order_var)

  assert_that(as_name(result_order_var) %in% c("n", "distinct_n", "pct", "distinct_pct"),
              msg = "Invalid argument for result_order_var It can be n, distinct_n, pct, or distinct_pct.")
  assert_inherits_class(e, "count_layer")

  env_bind(e, result_order_var = result_order_var)

  e
}

#' Set the display for missing strings
#'
#' Controls how missing counts are handled and displayed in the layer
#'
#' @param e A \code{count_layer} object
#' @param fmt An f_str object to change the display of the missing counts
#' @param sort_value A numeric value that will be used in the ordering column.
#'   This should be numeric. If it is not supplied the ordering column will be
#'   the maximum value of what appears in the table plus one.
#' @param denom_ignore A boolean. Specifies Whether or not to include the
#'   missing counts specified within the ... parameter within denominators. If
#'   set to TRUE, the values specified within ... will be ignored.
#' @param ... Parameters used to note which values to describe as missing.
#'   Generally NA and "Missing" would be used here. Parameters can be named
#'   character vectors where the names become the row label.
#'
#' @return The modified layer
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#'   mtcars2 <- mtcars %>%
#' mutate_all(as.character)
#' mtcars2[mtcars$cyl == 6, "cyl"] <- NA
#'
#' tplyr_table(mtcars2, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_missing_count(f_str("xx ", n), Missing = NA)
#'   ) %>%
#'   build()
set_missing_count <- function(e, fmt = NULL, sort_value = NULL, denom_ignore = FALSE, ...) {

  missings <- list(...)
  assert_that(length(missings) > 0, msg = "No missing values were specified.")

  if(!is.null(fmt)) assert_inherits_class(fmt, "f_str")
  if(!is.null(sort_value)) assert_inherits_class(sort_value, "numeric")
  if("shift_layer" %in% class(e)) {
    rlang::abort("`set_missing_count` for shift layers is not yet supported")
  }
  assert_inherits_class(e, "count_layer")

  # f_str object for formatting
  env_bind(e, missing_count_string = fmt)
  # Named list of strings and their replacements
  env_bind(e, missing_count_list = missings)
  # All replacements without names
  env_bind(e, missing_string = names(missings))
  env_bind(e, missing_sort_value = sort_value)
  # Used to trigger missing count formatting if the fmt is null.
  env_bind(e, has_missing_count = TRUE)

  if(denom_ignore){
    env_bind(e, denom_ignore = as.list(names(missings)))
  }

  e
}


#' Set values the denominator calculation will ignore
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This is generally used for missing values. Values like "", NA, "NA" are
#' common ways missing values are presented in a data frame. In certain cases,
#' percentages do not use "missing" values in the denominator. This function
#' notes different values as "missing" and excludes them from the denominators.
#'
#' @param e A \code{count_layer} object
#' @param ... Values to exclude from the percentage calculation. If you use
#'   `set_missing_counts()` this should be the name of the parameters instead of
#'   the values, see the example below.
#'
#'
#' @return The modified layer object
#' @export
#'
#' @examples
#' library(magrittr)
#' mtcars2 <- mtcars
#' mtcars2[mtcars$cyl == 6, "cyl"] <- NA
#' mtcars2[mtcars$cyl == 8, "cyl"] <- "Not Found"
#'
#' tplyr_table(mtcars2, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_missing_count(f_str("xx ", n), Missing = c(NA, "Not Found"))
#'       # This function is currently deprecated. It was replaced with an
#'       # argument in set_missing_count
#'       # set_denom_ignore("Missing")
#'   ) %>%
#'   build()
set_denom_ignore <- function(e, ...) {

  lifecycle::deprecate_stop("0.2.2", "set_denom_ignore()", with = "set_missing_count(count_missings = FALSE)")

}

#' Set the value of a outer nested count layer to Inf or -Inf
#'
#' @param e A \code{count_layer} object
#' @param outer_sort_position Either 'asc' or 'desc'. If desc the final ordering helper
#'   will be set to Inf, if 'asc' the ordering helper is set to -Inf.
#'
#' @return The modified count layer.
#' @export
set_outer_sort_position <- function(e, outer_sort_position) {

  assert_that(outer_sort_position %in% c("asc", "desc"),
              msg = "outer_sort_position must be 'asc' 'desc'")
  assert_inherits_class(e, "count_layer")

  outer_inf <- outer_sort_position == "desc"

  env_bind(e, outer_inf = outer_inf)

  e
}

#' Set Logic for denominator subsetting
#'
#' By default, denominators in count layers are subset based on the layer level
#' where logic. In some cases this might not be correct. This functions allows
#' the user to override this behavior and pass custom logic that will be used to
#' subset the target dataset when calculating denominators for the layer.
#'
#' @param e A \code{count_layer/shift_layer} object
#' @param denom_where An expression (i.e. syntax) to be used to subset the
#'   target dataset for calculating layer denominators. Supply as programming
#'   logic (i.e. x < 5 & y == 10). To remove the layer where parameter
#'   subsetting for the total row and thus the percentage denominators,
#'   pass 'TRUE' to this function.
#'
#' @return The modified Tplyr layer object
#' @export
#'
#' @examples
#' library(magrittr)
#' t10 <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl, where = cyl != 6) %>%
#'     set_denom_where(TRUE)
#'     # The denominators will be based on all of the values, including 6
#'   ) %>%
#'  build()
#'
set_denom_where <- function(e, denom_where) {
  denom_where <- enquo(denom_where)

  assert_that(is_logical_or_call(denom_where),
              msg = "The `where` parameter must contain subsetting logic (enter without quotes)")

  env_bind(e, denom_where = denom_where)

  e
}

#' @export
#' @noRd
set_denoms_by.count_layer <- function(e, ...) {
  dots <- vars(...)
  dots_chr <- map_chr(dots, as_name)

  # Pull these variables to make sure the denoms used make sense
  by_ <- map_chr(env_get(e, "by"), as_name)
  cols_ <- map_chr(env_get(e, "cols", inherit = TRUE), as_name)
  treat_var_ <- as_name(env_get(e, "treat_var", inherit = TRUE))
  target_var <- env_get(e, "target_var")
  target_var_ <- map_chr(target_var, as_name)

  assert_that(all(dots_chr %in% c(by_, cols_, treat_var_, target_var_)),
              msg = "A denom_by wasn't found as a grouping variable in the layer/table.")

  if (length(target_var) == 2) {
    assert_that(!(as_name(target_var[[2]]) %in% dots_chr),
                msg = "You can not pass the second variable in `vars` as a denominator.")
  }

  # If the row variable is here, rename it to summary_var
  if (as_name(target_var[[1]]) %in% dots_chr) {
    dots[dots_chr %in% as_name(target_var[[1]])] <- quos(summary_var)
  }

  env_bind(e, denoms_by = dots)

  e
}

#' Select levels to keep in a count layer
#'
#' @description
#' In certain cases you only want a layer to include certain values of a factor.
#' The `keep_levels()` function allows you to pass character values to be
#' included in the layer. The others are ignored.
#' **NOTE: Denominator calculation is unaffected by this function, see the
#' examples on how to include this logic in your percentages'**
#'
#' @param e A \code{count_layer} object
#' @param ... Character values to count int he layer
#'
#' @return The modified Tplyr layer object
#' @export
#'
#' @examples
#' library(dplyr)
#' mtcars <- mtcars %>%
#'   mutate_all(as.character)
#'
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       keep_levels("4", "8") %>%
#'       set_denom_where(cyl %in% c("4", "8"))
#'  ) %>%
#'  build()
#'
keep_levels <- function(e, ...) {
  dots <- list(...)
  assert_that(all(map_lgl(dots, is.character)),
              msg = "must pass character values to `keep_levels`")

  # Create the logic for the filtering. Pull out the second variable if it is a
  # nested count layer, or the first if it isn't nested
  env_bind(e, kept_levels = expr(!!tail(env_get(e, "target_var"), 1)[[1]] %in% !!unlist(dots)))
  env_bind(e, levels_to_keep = dots)

  e
}

#' Set a numeric cutoff
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' In certain tables, it may be necessary to only include rows that meet numeric
#' conditions. Rows that are less than a certain cutoff can be suppressed from
#' the output. This function allows you to pass a cutoff, a cutoff stat(n,
#' distinct_n, pct, or distinct_pct) to supress values that are lesser than the
#' cutoff.
#'
#'
#'
#' @param e A \code{count_layer} object
#' @param numeric_cutoff A numeric value where only values greater than or equal
#'   to will be displayed.
#' @param stat The statistic to use when filtering out rows. Either 'n',
#'   'distinct_n', or 'pct' are allowable
#' @param column If only a particular column should be used to cutoff values, it
#'   can be supplied here as a character value.
#'
#' @return The modified Tplyr layer object
#' @export
#' @md
#'
#' @examples
#' mtcars %>%
#' tplyr_table(gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_numeric_threshold(10, "n") %>%
#'       add_total_row() %>%
#'       set_order_count_method("bycount")
#'   )
set_numeric_threshold <- function(e, numeric_cutoff, stat, column = NULL) {

  assert_that(is.numeric(numeric_cutoff),
              msg = "The `numeric_cutoff` parameter must be numeric")
  assert_that(stat %in% c("n", "distinct_n", "pct"),
              msg = "Allowed values for 'stat' are 'n', 'distinct_n', or 'pct'")

  env_bind(e, numeric_cutoff = numeric_cutoff)
  env_bind(e, numeric_cutoff_stat = stat)
  env_bind(e, numeric_cutoff_column = column)

  e
}
