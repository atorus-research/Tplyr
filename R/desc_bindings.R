#' Get custom user summaries added to the layer environment via \code{set_custom_summaries}
#'
#' This method is an internal intended to be called within the \code{tplyr_layer} environment
#'
#' @param e Environment to extract custom summaries from
#'
#' @return \code{custom_summaries} binding in the layer environment
#' @noRd
get_custom_summaries <- function(e) {

  # Grab any custom summaries set within an option
  cust_sums <- append(list(), as.list(getOption('tplyr.custom_summaries')))

  # If the custom_summaries object exists in the layer environment then grab it
  if (exists("custom_summaries", envir=e)){
    cust_sums <- append(env_get(e, "custom_summaries"), cust_sums, after=0)
  }

  # Check to make sure all the summaries are named
  assert_that(is_empty(cust_sums) || is_named(cust_sums), msg = "All custom summaries must have names.")

  cust_sums
}

#' Set custom summaries to be performed within a descriptive statistics layer
#'
#' This function allows a user to define custom summaries to be performed in a
#' call to \code{dplyr::summarize()}. A custom summary by the same name as a
#' default summary will override the default. This allows the user to override
#' the default behavior of summaries built into 'Tplyr', while also adding new
#' desired summary functions.
#'
#' When programming the logic of the summary function, use the variable name
#' \code{.var} to within your summary functions. This allows you apply the
#' summary function to each variable when multiple target variables are
#' declared.
#'
#' @details An important, yet not immediately obvious, part of using
#' \code{set_custom_summaries} is to understand the link between the named
#' parameters you set in \code{set_custom_summaries} and the names called in
#' \code{\link{f_str}} objects within \code{\link{set_format_strings}}. In
#' \code{\link{f_str}}, after you supply the string format you'd like your
#' numbers to take, you specify the summaries that fill those strings.
#'
#' When you go to set your format strings, the name you use to declare a summary
#' in \code{set_custom_summaries} is the same name that you use in your
#' \code{\link{f_str}} call. This is necessary because
#' \code{\link{set_format_strings}} needs some means of putting two summaries in
#' the same value, and setting a row label for the summary being performed.
#'
#' Review the examples to see this put into practice. Note the relationship
#' between the name created in \code{set_custom_summaries} and the name used in
#' \code{\link{set_format_strings}} within the \code{\link{f_str}} call
#'
#' @param e \code{desc} layer on which the summaries should be bound
#' @param ... Named parameters containing syntax to be used in a call to
#'   \code{dplyr::summarize()}
#'
#' @return Binds a variable \code{custom_summaries} to the specified layer
#' @export
#'
#' @examples
#' #Load in pipe
#' library(magrittr)
#'
#' tplyr_table(iris, Species) %>%
#'   add_layer(
#'     group_desc(Sepal.Length, by = "Sepal Length") %>%
#'       set_custom_summaries(
#'         geometric_mean = exp(sum(log(.var[.var > 0]),
#'                                      na.rm=TRUE) / length(.var))
#'       ) %>%
#'       set_format_strings(
#'         'Geometric Mean' = f_str('xx.xx', geometric_mean)
#'       )
#'   ) %>%
#'   build()
set_custom_summaries <- function(e, ...){
  # Make sure you're modifying a tplyr_layer
  assert_inherits_class(e, 'desc_layer')

  # Convert the ellipsis to a named list
  params <- enexprs(...)

  # All items in list must be named so check them all
  assert_that(is_named(params),
              msg="In `set_custom_summaries` all summaries submitted must have names.")

  # Check each param submitted
  for (name in names(params)) {
    # Each parameter must be a call object
    assert_that(is_call(params[[name]]),
                msg = paste0("In `set_custom_summaries` parameter `", name,
                             "` is not a call. All parameters must be syntax (i.e. mean(variable, na.rm=TRUE))."))

  }

  # Bind to the layer environment
  env_bind(e, custom_summaries = params)
  e
}
