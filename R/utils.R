### Utility Functions

#' Extract the top function from a nested call and insert desired arguments
#'
#' @param c An R expression
#' @param allowable_calls A character vector of function names allowed to be called within a piping sequence
#' @param ... Parameters to insert into topmost call
#'
#' @return The original call object with
#'
#' @noRd
#' @examples
#'
#' modify_nested_call(mean(c(1,2,3)) %>% print(), na.rm=TRUE)
modify_nested_call <- function(c, allowable_calls = getNamespaceExports("Tplyr"), ...) {
  # If the call is not from magrittr, then modify the contents and return the call
  if (call_name(c) != "%>%") {
    # Only allow the user to use `tplyr` functions
    if (!is.null(allowable_calls)) {
      assert_that(call_name(c) %in% allowable_calls, msg = "Functions called within `add_layer` must be part of `Tplyr`")
    }
    c <- call_modify(.call=c, ...)

  } else {
    if (!is.null(allowable_calls)) {
      # Only allow the user to use `tplyr` functions
      assert_that(all(map_chr(call_args(c), call_name) %in% c(allowable_calls, '%>%')),
                  msg="Functions called within `add_layer` must be part of `Tplyr`")
    }

    # Recursively extract the left side of the magrittr call to work your way up
    e <- call_standardise(c)
    c <- modify_nested_call(call_args(e)$lhs, allowable_calls = allowable_calls, ...)
    # Modfify the magittr call by inserting the call retrieved from recursive command back in
    c <- call_modify(e, lhs=c)
    c
  }
}

#' Find depth of a layer object
#'
#' This function returns the number of containers "above" a layer object. As
#' layers can be nested layers may contain layers and so on. This uses
#' recursion to find the table environment
#'
#' @param layer A layer object
#' @param i The current index
#'
#' @return the number of containers a layer is in
#' @noRd
depth_from_table <- function(layer, i){
  if(class(env_parent(layer))[1] == "tplyr_table") return(i + 1)
  else {
    return(depth_from_table(env_parent(layer), i+1))
  }
}


#' Convert a list of quosures to character strings
#'
#' Intended for use in a tidyselect context. Pivots take arguments as character strings or indices. Tidyselect tools return those
#' indices. This allows you to pass a list of quosures (which Tplyr carries a lot of) without explicitly converting types
#'
#' @param var_list List of quosures containing variables
#'
#' @return Character string of labels
#'
#' @noRd
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' library(dplyr)
#' library(tidyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarize(mean=mean(Sepal.Length), median = median(Sepal.Length)) %>%
#'   pivot_longer(cols = match_exact(vars(mean, median)))
#'
match_exact <- function(var_list) {
  # Should have been a list of quosures on input
  assert_inherits_class(var_list, "quosures")
  # Return the variable names as a character string in appropriate tidyselect format
  out <- map_chr(var_list, as_label) # as_label is needed here vs as_name
  unname(out[out != 'NULL']) # Exclude NULL quosures and remove names
}

#' Organize row labels within a layer output
#'
#' @param dat A data.frame/tibble to have row labels renamed
#' @param by The \code{by} object within a layer
#'
#' @return A tibble with renamed variables and row labels re-ordered to the front of the tibble
#' @noRd
replace_by_string_names <- function(dat, by) {
  # By must be a list of quosures
  assert_that(is_quosures(by), msg = "`by` must be a list of quosures")

  # If there were character strings in the by variables then rename them
  # with an index, starting at 1
  for (i in seq_along(by)) {
    # If stats are present in a table and there are character values in the by variables
    # The name may be `value` or `"value"` this check catches those scenerios
    if(as_label(by[[i]]) %in% names(dat)) {
      dat <- rename(dat, !!paste0('row_label', i) := as_label(by[[i]]))
    } else if(as_name(by[[i]]) %in% names(dat)) {
      dat <- rename(dat, !!paste0('row_label', i) := as_name(by[[i]]))
    }

  }

  # If i iterated above, it will be have a value. Otherwise it's null, so set it to 0
  i <- ifelse(is.null(i), 0, i)

  # If there was a column named `row_label` the index it
  if ('row_label' %in% names(dat)) {
    dat <- rename(dat, !!paste0('row_label', i + 1) := row_label)
  }

  # Sort the row labels by index
  row_labels <- names(dat)[str_detect(names(dat), 'row_label')]

  # Insert row labels to the front of the tibble
  select(dat, all_of(sort(row_labels)), everything()) %>%
    ungroup() %>%
    mutate_at(row_labels, ~ as.character(.x)) # Coerce all row labels into character
}

#' Get the unique levels/factors of a dataset
#'
#' @param e An environment, generally a table or a layer object
#' @param x A target variable to get the levels/unique values of
#'
#' @return Unique target values
#' @noRd
get_target_levels <- function(e, x) {
  # If its a factor just return the levels
  if(is.factor(env_get(e, "target", inherit = TRUE)[, as_name(x)])) levels(env_get(e, "built_target", inherit = TRUE)[, as_name(x)])
  # Otherwise return the unique values
  else {
    unique(env_get(e, "built_target", inherit = TRUE)[, as_name(x)])
  }
}

#' Replace repeating row label variables with blanks in preparation for display.
#'
#' Depending on the display package being used, row label values may need to be
#' blanked out if they are repeating. This gives the data frame supporting the
#' table the appearance of the grouping variables being grouped together in
#' blocks. \code{apply_row_masks} does this work by blanking out the value of
#' any row_label variable where the current value is equal to the value
#' before it. Note - \code{apply_row_masks} assumes that the data frame has
#' already be sorted and therefore should only be applied once the data frame is
#' in its final sort sequence.
#'
#' Additionally, \code{apply_row_masks} can add row breaks for you between each
#' layer. Row breaks are inserted as blank rows. This relies on the "break by"
#' variables (submitted via \code{...}) constructed in \code{build} still being
#' attached to the dataset. An additional order variable is attached named
#' \code{ord_break}, but the output dataset is sorted to properly insert the row
#' breaks between layers.
#'
#' @param dat Data.frame / tibble to mask repeating row_labels
#' @param row_breaks Boolean - set to TRUE to insert row breaks
#' @param ... Variable used to determine where row-breaks should be inserted.
#'   Breaks will be inserted when this group of variables changes values. This
#'   is determined by dataset order, so sorting should be done prior to using
#'   \code{apply_row_masks}. If left empty, \code{ord_layer_index} will be used.
#'
#' @return tibble with blanked out rows where values are repeating
#' @export
apply_row_masks <- function(dat, row_breaks=FALSE, ...) {

  # Capture the break_by variables
  break_by <- enquos(...)

  # Get the row labels that need to be masked
  nlist <- names(dat)[str_detect(names(dat), "row_label")]

  # Iterate each variable
  for (name in nlist){
    dat <- dat %>%
      # Identify if the value was repeating (ugly compensation for first row)
      mutate(mask = ifelse(!(is.na(lag(!!sym(name)))) & !!sym(name) == lag(!!sym(name)), TRUE, FALSE),
             # If repeating then blank out
             !!name := ifelse(mask == TRUE, '', !!sym(name))
      )
  }
  # Drop the dummied mask variable
  dat <- dat %>% select(-mask)

  # Break rows if specified
  if (row_breaks) {

    # Default to ord_layer_index
    if (is_empty(break_by)) break_by <- quos(ord_layer_index)

    # All the break by variables must be variable names
    assert_that(all(map_chr(map(break_by, quo_get_expr), class) == "name"),
                msg = "All parameters submitted through `...` must be variable names")

    assert_that(all(map_chr(break_by, as_name) %in% names(dat)),
                msg = paste0("If `row_breaks` is specified, variables submitted via `...` ",
                             "must be `ord` variables included in the input data frame.\n",
                             "Remember to sort prior to using `apply_row_masks`."))

    assert_that(all(str_starts(map_chr(break_by, as_name), "ord")),
                msg = paste0("Break-by variables submitted via `...` must be 'Tplyr' order variables ",
                             "that start with `ord`"))

    # Create the breaks dataframe
    breaks <- dat %>%
      distinct(!!!break_by) %>%
      mutate(ord_break = 2)

    # Add in a sorting variable to the data
    dat <- dat %>%
      mutate(ord_break = 1)

    # bind and fill the NAs
    dat <- bind_rows(dat, breaks) %>%
      arrange(!!!break_by, ord_break) %>%
      mutate_if(is.character, ~replace_na(., ""))
  }

  dat
}

#' Take a list of quosures and pull out things that aren't symbols
#'
#' @param var_list List of quosures
#'
#' @return Quosures that aren't symbols
#' @noRd
extract_character_from_quo <- function(var_list) {

  is_symbol_ <- map_lgl(var_list, quo_is_symbol)

  var_list[!is_symbol_]
}

#' Get maximum string format recursivly
#'
#' @param lay A layer object
#'
#' @return Maximum length of sub layers
#' @noRd
get_max_length <- function(lay) {
  # Initalize max_ to -1
  max_ <- -1L
  # Get maximum length of all sub layers
  if(length(lay$layers) > 0) max_ <- max(map_int(lay$layers, get_max_length))

  # return greatest between sub layers and current layer
  max(max_, lay$format_strings$size)
}

#' Clean variable attributes
#'
#' @param dat Dataframe to strip of variable attributes
#'
#' @return Dataframe with variable attributes removed, except for factor levels
#' @noRd
clean_attr <- function(dat) {
  for (n in names(dat)) {
    for (a in names(attributes(dat[[n]]))) {
      if (!a  %in% c('levels', 'class', 'names', 'row.names', 'groups')) {
        attr(dat[[n]], a) <- NULL
      }
    }
  }
  dat
}

#' Simulate IBM rounding
#'
#' This logic is from the github issue
#' https://github.com/atorus-research/Tplyr/issues/9
#'
#' @param x The numeric values to round
#' @param n The number of decimal rounding points
#'
#' @return The rounded value
#' @noRd
ut_round <- function(x, n=0)
{
  # x is the value to be rounded
  # n is the precision of the rounding
  scale <- 10^n
  y <- trunc(x * scale + sign(x) * 0.5) / scale
  # Return the rounded number
  return(y)
}
