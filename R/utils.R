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
    dat <- rename(dat, !!paste0('row_label', i) := as_label(by[[i]]))
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
#' @param dat Data.frame / tibble to mask repeating row_labels
#'
#' @return tibble with blanked out rows where appropriate
#' @export
apply_row_masks <- function(dat) {
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
  dat
}

#' Create a table based on a count layer with two target_vars
#'
#' This is intended to be called in the count build
#'
#' @param target_var_1_i A single value contained in the first target variable
#' @param x The count layer
#'
#' @noRd
bind_nested_count_layer <- function(target_var_1_i, x) {

  indentation <- env_get(x, "indentation", default = "\t", inherit = TRUE)
  # Logic for it its inheriting from the global environment
  indentation <- ifelse(is.null(indentation), "\t", indentation)

  # This contains the subset of the first target variable.
  # If nest_counts is true. the first treat_var is added in the by so it appears
  # in its own column
    inner_layer <- process_summaries(group_count(x, target_var = !!get_target_var(x)[[2]],
                                                 by = vars(!!!get_by(x), !!get_target_var(x)[[1]]), cols = vars(!!!env_get(x, "cols")),
                                                 where = !!get_where(x) & !!get_target_var(x)[[1]] == !!target_var_1_i) %>%
                                       # Set the value for how to prefix the inner layer
                                       set_count_row_prefix(indentation))


    # Process the formatting here to get the metadata. We just need the number of rows
    inner_layer_form <- inner_layer$numeric_data %>%
      # Pivot table
      pivot_wider(id_cols = c(match_exact(get_by(x)), "summary_var", match_exact(head(get_target_var(x), -1))),
                  names_from = c(!!env_get(x, "treat_var", inherit = TRUE), match_exact(env_get(x, "cols", inherit = TRUE))),
                  values_from = n,
                  names_prefix = "var1_")


    # This should be a single row with the total of target_var 1
    outer_layer <- process_summaries(group_count(x, target_var = !!get_target_var(x)[[1]],
                                                 by = vars(!!!get_by(x)), cols = vars(!!!env_get(x, "cols")),
                                                 where = !!get_where(x) & !!get_target_var(x)[[1]] == !!target_var_1_i))

    outer_layer$numeric_data <- outer_layer$numeric_data %>%
      mutate(!!env_get(x, "target_var")[[1]] := !!target_var_1_i)

    # Add the index for this part of the layer
    save_nested_layer_order(x, nrow(inner_layer_form) + 1)

    # Bind these two to gether and add a row mask
    bind_rows(outer_layer$numeric_data, inner_layer$numeric_data)
}

#' Bind an index for a nested count for use in sorting.
#' @noRd
save_nested_layer_order <- function(x, num_rows) {

  # Number to add as the sorting column
  current_nest <- env_get(x, "current_nest", default = 0) + 1

  # Pull current index vector
  nest_sort_index <- env_get(x, "nest_sort_index", default = numeric(0))

  # Append current nest level to vecotr
  env_bind(x, nest_sort_index = append(nest_sort_index, rep(current_nest, num_rows)))

  # add new current nest value
  env_bind(x, current_nest = current_nest)

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
