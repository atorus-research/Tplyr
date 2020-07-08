#' Get custom user summaries added to the layer environment via \code{set_custom_summaries}
#'
#' This method is an internal intended to be called within the \code{tplyr_layer} environment
#'
#' @param e Environment to extract custom summaries from
#'
#' @return \code{custom_summaries} binding in the layer environment
#' @export
get_custom_summaries <- function(e) {
  # If the custom_summaries object exists in the layer environment then grab it
  if (exists("custom_summaries", envir=e)){
    env_get(e, "custom_summaries")
  } else {
    # Otherwise return a list
    list()
  }
}

#' Set custom summaries to be performed within a decsriptive statistics layer
#'
#' This function allows a user to define custom summaries to be performed in a call to \code{dplyr::summarize()}. A custom
#' summary by the same name as a default summary will override the default. This allows the user to override the default behaivor
#' of summaries built into 'Tplyr', while also adding new desired summary functions.
#'
#' When programming the logic of the summary function, use the target variable name in your code.
#' \strong{NOTE:} This is likely to change in the future, as it currently does not support descriptive statistic layers with two
#' target variables provided.
#'
#' @param e \code{desc} layer the summaries should be bound to
#' @param ... Named parameters containing syntax to be used in a call to \code{dplyr::summarize()}
#'
#' @return Binds a variable \code{custom_summaries} to the specified layer
#' @export
#'
#' @examples
#'
#' # TBD
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
}
