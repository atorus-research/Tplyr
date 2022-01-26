
#' @noRd
#' @export
process_summaries.count_layer <- function(x, ...) {

  if(env_get(x, "is_built_nest", default = FALSE)) {
    refresh_nest(x)
  }

  # Subset the local built_target based on where
  # Catch errors
  evalq({
    tryCatch({
      # Check 'kept_levels' and stop if they're not in the target dataset
      #Logic to check for keep_levels
      # If this is not a built nest
      if(!("tplyr_layer" %in% class(env_parent()))) {
        keep_levels_logic <- expr(!is.null(levels_to_keep))
        # If this is a built nest and we're begining to process
      }else if("tplyr_layer" %in% class(env_parent()) && length(target_var) == 2){
        keep_levels_logic <- expr(!is.null(levels_to_keep) && quo_is_symbol(target_var[[1]]))
        # If this is a built nest and we are processing the "sub" layers
      } else {
        keep_levels_logic <- expr(FALSE)
      }

      # Check that all values in 'keep levels' are present in the data
      if(eval_tidy(keep_levels_logic)) {
        if(is.factor(target[[as_name(tail(target_var, 1)[[1]])]])){
          target_levels <- levels(target[[as_name(tail(target_var, 1)[[1]])]])
        } else {
          target_levels <- unique(target[[as_name(tail(target_var, 1)[[1]])]])
        }
        kept_levels_found <- unlist(levels_to_keep) %in% target_levels
        assert_that(all(kept_levels_found),
                    msg = paste0("level passed to `kept_levels` not found: ",
                                 paste0(levels_to_keep[!kept_levels_found],
                                        collapse = "",
                                        sep = " ")))
      }

      # Save this for the denominator where, but only if it hasn't been saved yet.
      if(is.null(built_target_pre_where)) built_target_pre_where <- built_target



      built_target <- built_target %>%
        filter(!!where) %>%
        filter(!!kept_levels)

      ## Drop levels if target var is factor and kept levels used
      if(eval_tidy(keep_levels_logic) &&
         is.factor(built_target[[as_name(tail(target_var, 1)[[1]])]])) {
        # Pull out the levels that weren't in keep levels.
        target_levels <- levels(built_target[[as_name(tail(target_var, 1)[[1]])]])
        drop_levels_ind <- !(target_levels %in% levels_to_keep)
        drop_these_levels <- target_levels[drop_levels_ind]
        # Use forcats to remove the levels that weren't in the "keep levels"
        built_target <- built_target %>%
          mutate(!!tail(target_var,1)[[1]] := fct_drop(!!tail(target_var,1)[[1]], only = drop_these_levels))
      }

    }, error = function(e) {
      abort(paste0("group_count `where` condition `",
                   as_label(where),
                   "` is invalid. Filter error:\n", e))
    })

    if(!quo_is_symbol(target_var[[1]]) && as_name(target_var[[1]]) %in% names(target)) {
      warning(paste0("The first target variable has been coerced into a symbol.",
                     " You should pass variable names unquoted."), immediate. = TRUE)

      target_var[[1]] <- quo(!!sym(as_name(target_var[[1]])))
    }

    if(length(target_var) == 2 && !quo_is_symbol(target_var[[2]]) &&
                as_name(target_var[[2]]) %in% names(target)) {
      warning(paste0("The second target variable has been coerced into a symbol.",
                     "You should pass variable names unquoted."), immediate. = TRUE)

      target_var[[2]] <- quo(!!sym(as_name(target_var[[2]])))
    }

  }, envir = x)

  rename_missing_values(x)

  # Preprocssing in the case of two target_variables
  if(length(env_get(x, "target_var")) > 2) abort("Only up too two target_variables can be used in a count_layer")

  else if(length(env_get(x, "target_var")) == 2) {

    # Change treat_var to factor so all combinations appear in nest
    factor_treat_var(x)

    # If the nest_sort_index isn't null, reset it
    # This happens if the layer is reloaded
    if (!is.null(env_get(x, "nest_sort_index", default = NULL))) env_bind(x, nest_sort_index = NULL)

    process_nested_count_target(x)

  } else {

    process_count_denoms(x)

    process_single_count_target(x)

  }

  prepare_format_metadata(x)

  # Trigger any derivation of additional statistics
  map(x$stats, process_statistic_data)

  x
}

#' @param x A count layer with a single target_var
#'
#' This function uses dplyr to filter out the where call, pull out the distinct
#' rows if applicable, and tallies the different target_var values.
#'
#' If include_total_row is true a row will be added with a total row labeled
#' with total_row_label.
#'
#' Complete is used to complete the combinaions of by, treat_var, and target_var
#'
#' @noRd
process_single_count_target <- function(x) {
  evalq({

    if(is.null(include_total_row)) include_total_row <- FALSE
    if(is.null(total_row_label)) total_row_label <- "Total"

    # The current environment should be the layer itself
    process_count_n(current_env())

    if(!is.null(distinct_by)) process_count_distinct_n(current_env())

    if(include_total_row){
      process_count_total_row(current_env())
      if(!is.null(distinct_by)) {
        process_count_distinct_total_row(current_env())
      }

      # Used to temporarily check formats
      if(is.null(format_strings)) tmp_fmt <- gather_defaults.count_layer(current_env())
      if(count_missings && !(is.null(denom_ignore) || length(denom_ignore) == 0) &&
         (("pct" %in% total_count_format$vars || "distinct_pct" %in% total_count_format$vars) ||
         # Logic if no total_count format
         (is.null(total_count_format) && is.null(format_strings) && ("pct" %in% tmp_fmt$n_counts$vars || "distinct_pct" %in% tmp_fmt$n_counts$vars)) ||
         (is.null(total_count_format) && ("pct" %in% count_fmt$n_counts$vars || "distinct_pct" %in% count_fmt$n_counts$vars))
         )
         ) {
           warning("Your total row is ignoring certain values. The 'pct' in this row may not be 100%",
                   immediate. = TRUE)
         }
    }

    if(is.null(count_row_prefix)) count_row_prefix <- ""

    if(is.null(denoms_by)) denoms_by <- c(treat_var, cols)

    # rbind tables together
    numeric_data <- summary_stat %>%
      bind_rows(total_stat) %>%
      rename("summary_var" = !!target_var[[1]]) %>%
      group_by(!!!denoms_by) %>%
      do(get_denom_total(., denoms_by, denoms_df, denoms_distinct_df, "n")) %>%
      mutate(summary_var = prefix_count_row(summary_var, count_row_prefix)) %>%
      ungroup()


    if(!is.null(distinct_stat)) {
      if(include_total_row) {
        distinct_stat <- distinct_stat %>%
          bind_rows(total_stat_denom) %>%
          group_by(!!!denoms_by) %>%
          do(get_denom_total(., denoms_by, denoms_df, denoms_distinct_df, "distinct_n"))
      }
      numeric_data <- bind_cols(numeric_data,
                                distinct_stat[, c("distinct_n", "distinct_total")])
    }
  }, envir = x)
}

#' @noRd
process_nested_count_target <- function(x) {

  evalq({

    if(is.null(indentation)) indentation <- "   "



    assert_that(quo_is_symbol(target_var[[2]]),
                msg = "Inner layers must be data driven variables")

    first_layer <- process_summaries(group_count(current_env(), target_var = !!target_var[[1]],
                                                 by = vars(!!!by), where = !!where))

    second_layer <- process_summaries(group_count(current_env(), target_var = !!target_var[[2]],
                                                  by = vars(!!target_var[[1]], !!!by), where = !!where) %>%
                                        set_count_row_prefix(indentation))

    first_layer_final <- first_layer$numeric_data
    # add_column(!!target_var[[1]] := .[["summary_var"]])

    second_layer_final <- second_layer$numeric_data %>%
      group_by(!!target_var[[1]]) %>%
      do(filter_nested_inner_layer(., target, target_var[[1]], target_var[[2]], indentation))

    # Bind the numeric data together
    numeric_data <- bind_rows(first_layer_final, second_layer_final)

    # Save the original by and target_vars incase the layer is rebuilt
    by_saved <- by
    target_var_saved <- target_var
    is_built_nest <- TRUE

    by <- vars(!!target_var[[1]], !!!by)
    target_var <- vars(!!target_var[[2]])


  }, envir = x)

}

#' This function resets the variables for a nested layer after it was built
#' @noRd
refresh_nest <- function(x) {
  env_bind(x, by = env_get(x, "by_saved"))
  env_bind(x, target_var = env_get(x, "target_var_saved"))
}

#' This function is meant to remove the values of an inner layer that don't
#' appear in the target data
#' @noRd
filter_nested_inner_layer <- function(.group, target, outer_name, inner_name, indentation) {

  # Is outer variable text? If it is don't filter on it
  text_outer <- !quo_is_symbol(outer_name)
  outer_name <- as_name(outer_name)
  inner_name <- as_name(inner_name)

  if(text_outer) {
    target_inner_values <- target %>%
      select(any_of(inner_name)) %>%
      unlist() %>%
      paste0(indentation, .)
  } else {
    current_outer_value <- unique(.group[, outer_name])[[1]]

    target_inner_values <- target %>%
      filter(!!sym(outer_name) == current_outer_value) %>%
      select(any_of(inner_name)) %>%
      unlist() %>%
      paste0(indentation, .)
  }

  .group %>%
    filter(summary_var %in% target_inner_values)

}

#' Process the n count data and put into summary_stat
#'
#' @param x Count layer
#' @noRd
process_count_n <- function(x) {

  evalq({

    summary_stat <- built_target %>%
      # Group by variables including target variables and count them
      group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
      tally(name = "n") %>%
      mutate(n = as.double(n)) %>%
      ungroup()

    # If there is a missing_count_string, but its not in the dataset
    if(!is.null(missing_count_string) &&

       !((any(unname(unlist(missing_count_list)) %in% unique(built_target[, as_name(target_var[[1]])]))) ||
        any(is.na(built_target[, as_name(target_var[[1]])])))) {
      # This adds the missing string as a factor to the tallies. This is needed
      # to make sure the missing row is added even if there are no missing values.
      summary_stat <- summary_stat %>%
        mutate(!!target_var[[1]] := fct_expand(.data[[as_name(target_var[[1]])]],
                                               names(missing_count_list)))
    }

    summary_stat <- summary_stat %>%
      # complete all combinations of factors to include combinations that don't exist.
      # add 0 for combinations that don't exist
      complete(!!treat_var, !!!by, !!!target_var, !!!cols, fill = list(n = 0, total = 0)) %>%
      # Change the treat_var and first target_var to characters to resolve any
      # issues if there are total rows and the original column is numeric
      mutate(!!treat_var := as.character(!!treat_var)) %>%
      mutate(!!as_name(target_var[[1]]) := as.character(!!target_var[[1]]))

    # If there is no values in summary_stat, which can happen depending on where. Return nothing
    if(nrow(summary_stat) == 0) return()
  }, envir = x)

}

#' Process the distinct n count data and put into summary_stat
#'
#' @param x Count layer
#' @noRd
process_count_distinct_n <- function(x) {

  evalq({

    # Subset the local built_target based on where
    # Catch errors



    if(is.null(denoms_by)) denoms_by <- c(treat_var, cols)

    distinct_stat <- built_target %>%
      # Filter out based on where
      filter(!!where) %>%
      mutate(
        across(
          .cols = any_of(map_chr(c(denoms_by, target_var, by), ~as_name(.))),
          .fns = function(x) if(is.factor(x)) x else as.factor(x)
          )
      ) %>%
      # Distinct based on the current distinct_by, target_var, and treat_var
      # treat_var is added because duplicates would be created when there are
      # treatment group totals
      distinct(!!!distinct_by, !!treat_var, !!!target_var, .keep_all = TRUE) %>%
      # Group by variables including target variables and count them
      group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
      tally(name = "distinct_n") %>%
      ungroup()

    if(!is.null(missing_count_string) &&

       !((unname(unlist(missing_count_list)) %in% unique(built_target[, as_name(target_var[[1]])])) ||
         any(is.na(built_target[, as_name(target_var[[1]])])))) {
      # This adds the missing string as a factor to the tallies. This is needed
      # to make sure the missing row is added even if there are no missing values.
      summary_stat <- summary_stat %>%
        mutate(!!target_var[[1]] := fct_expand(as.character(.data[[as_name(target_var[[1]])]]),
                                               names(missing_count_list)))
    }


    # complete all combinations of factors to include combinations that don't exist.
    # add 0 for combinations that don't exist
    distinct_stat <- distinct_stat %>%
      complete(!!treat_var, !!!by, !!!cols, !!!target_var, fill = list(distinct_n = 0, distinct_total = 0)) %>%
      # Change the treat_var and first target_var to characters to resolve any
      # issues if there are total rows and the original column is numeric
      mutate(!!treat_var := as.character(!!treat_var)) %>%
      group_by(!!!denoms_by) %>%
      do(get_denom_total(., denoms_by, denoms_df, denoms_distinct_df, "distinct_n")) %>%
      ungroup() %>%
      rename("distinct_total" = "total")


    # If there is no values in summary_stat, which can happen depending on where. Return nothing
    if(nrow(summary_stat) == 0) return()
  }, envir = x)
}

#' Process the amounts for a total row
#'
#' @param x A Count layer
#' @noRd
process_count_total_row <- function(x) {
  evalq({

    # Check if denoms_by wasn't passed and by was passed.
    if(is.null(denoms_by) & any(map_lgl(by, quo_is_symbol)) > 0) {
      warning("A total row was added in addition to non-text by variables, but
no denoms_by variable was set. This may cause unexpected results. If you wish to
change this behavior, use `set_denoms_by()`.", immediate. = TRUE)
    }

    # Make sure the denoms_by is stripped
    # Stripped of cols and treat_var variables, otherwise it will error out in the group_by
    # I thought of replacing the group by with !!!unique(c(treat_var, cols, denoms_by))
    # but that doesn't work due to the denoms_by having an environment set

    # Logical vector that is used to remove the treat_var and cols
    needed_denoms_by <- map_lgl(denoms_by, function(x, treat_var, cols) {
      all(as_name(x) != as_name(treat_var),
          as_name(x) != map_chr(cols, as_name))
    }, treat_var, cols)

    #Create an expression to evaluate filter
    if(!count_missings){
      filter_logic <- expr(!(!!target_var[[1]] %in% names(missing_count_list)))
    } else {
      filter_logic <- expr(TRUE)
    }

    # create a data.frame to create total counts
    total_stat <- summary_stat %>%
      #Filter out any ignored denoms
      filter(!!filter_logic) %>%
      # Use distinct if this is a distinct total row
      # Group by all column variables
      group_by(!!treat_var, !!!cols, !!!denoms_by[needed_denoms_by]) %>%
      summarize(n = sum(n)) %>%
      ungroup() %>%
      mutate(total = n) %>%
      # Create a variable to label the totals when it is merged in.
      mutate(!!as_name(target_var[[1]]) := total_row_label) %>%
      # Create variables to carry forward 'by'. Only pull out the ones that
      # aren't symbols
      group_by(!!!extract_character_from_quo(by)) %>%
      # ungroup right away to make sure the complete works
      ungroup() %>%
      # complete based on missing groupings
      complete(!!treat_var, !!!cols, fill = list(n = 0, total = 0))
  }, envir = x)
}

process_count_distinct_total_row <- function(x) {
  evalq({

    # Check if denoms_by wasn't passed and by was passed.
    if(is.null(denoms_by) & any(map_lgl(by, quo_is_symbol)) > 0) {
      warning("A total row was added in addition to non-text by variables, but
no denoms_by variable was set. This may cause unexpected results. If you wish to
change this behavior, use `set_denoms_by()`.", immediate. = TRUE)
    }

    # Make sure the denoms_by is stripped
    # Stripped of cols and treat_var variables, otherwise it will error out in the group_by
    # I thought of replacing the group by with !!!unique(c(treat_var, cols, denoms_by))
    # but that doesn't work due to the denoms_by having an environment set

    # Logical vector that is used to remove the treat_var and cols
    needed_denoms_by <- map_lgl(denoms_by, function(x, treat_var, cols) {
      all(as_name(x) != as_name(treat_var),
          as_name(x) != map_chr(cols, as_name))
    }, treat_var, cols)

    #Create an expression to evaluate filter
    if(!count_missings){
      filter_logic <- expr(!(!!target_var[[1]] %in% names(missing_count_list)))
    } else {
      filter_logic <- expr(TRUE)
    }

    # create a data.frame to create total counts
    total_stat_denom <- distinct_stat %>%
      #Filter out any ignored denoms
      filter(!!filter_logic) %>%
      # Group by all column variables
      group_by(!!treat_var, !!!cols, !!!denoms_by[needed_denoms_by]) %>%
      summarize(distinct_n = sum(distinct_n)) %>%
      ungroup() %>%
      mutate(distinct_total = distinct_n) %>%
      # Create a variable to label the totals when it is merged in.
      mutate(!!as_name(target_var[[1]]) := total_row_label) %>%
      # Create variables to carry forward 'by'. Only pull out the ones that
      # aren't symbols
      group_by(!!!extract_character_from_quo(by)) %>%
      # ungroup right away to make sure the complete works
      ungroup() %>%
      # complete based on missing groupings
      complete(!!treat_var, !!!cols, fill = list(distinct_n = 0, distinct_total = 0))
  }, envir = x)
}

#' Prepare metadata for table
#'
#' @param x count_layer object
#' @noRd
prepare_format_metadata.count_layer <- function(x) {
  evalq({

    # Get formatting metadata prepared
    if(is.null(format_strings)) {
      format_strings <- gather_defaults(environment())
    } else if (!'n_counts' %in% names(format_strings)) {
      format_strings[['n_counts']] <- gather_defaults(environment())[['n_counts']]
    }


    # If there is both n & distinct, or pct and distinct_pct there has to be a
    # distinct_by
    # If both distinct and n
    if(((("distinct_n" %in% map(format_strings$n_counts$vars, as_name) &
         "n" %in% map(format_strings$n_counts$vars, as_name)) |
        # or both distinct_pct and pct
        ("distinct_pct" %in% map(format_strings$n_counts$vars, as_name) &
         "pct" %in% map(format_strings$n_counts$vars, as_name))) &
        # AND distinct_by is null
        is.null(distinct_by))) {
      stop("You can't use distinct and non-distinct parameters without specifying a distinct_by")
    }

    # If distinct_by isn't there, change distinct and distinct_pct
    if(is.null(distinct_by) & "distinct_n" %in% map(format_strings$n_counts$vars, as_name)) {
      distinct_ind <- which(map(format_strings$n_counts$vars, as_name) %in% "distinct_n")
      format_strings$n_counts$vars[[distinct_ind]] <- expr(n)
    }
    if(is.null(distinct_by) & "distinct_pct" %in% map(format_strings$n_counts$vars, as_name)) {
      distinct_ind <- which(map(format_strings$n_counts$vars, as_name) %in% "distinct_pct")
      format_strings$n_counts$vars[[distinct_ind]] <- expr(pct)
    }

    # Pull max character length from counts. Should be at least 1
    n_width <- max(c(nchar(numeric_data$n), 1L))

    # If a layer_width flag is present, edit the formatting string to display the maximum
    # character length
    if(str_detect(format_strings[['n_counts']]$format_string, "a")) {
      # Replace the flag with however many xs
      replaced_string <- str_replace(format_strings[['n_counts']]$format_string, "a",
                                     paste(rep("x", n_width), collapse = ""))

      # Make a new f_str and replace the old one
      format_strings[['n_counts']] <- f_str(replaced_string, !!!format_strings$n_counts$vars)
    }
    max_length <- format_strings[['n_counts']]$size
  }, envir = x)
}

#' @noRd
#' @export
process_formatting.count_layer <- function(x, ...) {
  evalq({

    # Calculate the indentation length. This is needed if there are missing
    #values in a nested count layer. Length is sent to string construction and
    #used to split the string.
    indentation_length <- ifelse(is.null(indentation), 0, nchar(encodeString(indentation)))

    formatted_data <- numeric_data

    # if(is_built_nest && !quo_is_symbol(by[[1]])) {
    #   names(formatted_data) <- str_remove_all(names(formatted_data), "\\\"")
    # }


    formatted_data <- formatted_data %>%
      # Mutate value based on if there is a distinct_by
      mutate(n = {
        construct_count_string(.n=n, .total=total,
                               .distinct_n=distinct_n, .distinct_total=distinct_total,
                               count_fmt=format_strings[['n_counts']],
                               max_layer_length=max_layer_length,
                               max_n_width=max_n_width,
                               missing_string = missing_string,
                               missing_f_str = missing_count_string,
                               summary_var = summary_var,
                               indentation_length = indentation_length,
                               total_count_format = total_count_format,
                               total_row_label = total_row_label,
                               has_missing_count = has_missing_count)
      }) %>%
      # Pivot table
        pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                  names_from = c(!!treat_var, match_exact(cols)), values_from = n,
                  names_prefix = "var1_") %>%
    # Replace the by variables and target variable names with `row_label<n>`
    replace_by_string_names(quos(!!!by, summary_var))

    if(is_built_nest) {

      # I had trouble doing this in a 'tidy' way so I just did it here.
      # First column is always the outer target variable.
      # Last row label is always the inner target variable
      row_labels <- vars_select(names(formatted_data), starts_with("row_label"))
      # Replace the missing 'outer' with the original target
      # The indexing looks weird but the idea is to get rid of the matrix with the '[, 1]'
      formatted_data[is.na(formatted_data[[1]]), 1] <- formatted_data[is.na(formatted_data[[1]]),
                                                                      tail(row_labels, 1)]
    }

    if (!is_empty(stats)) {
      # Process the statistical data formatting
      formatted_stats_data <- map(stats, process_statistic_formatting) %>%
        reduce(full_join, by=c('summary_var', match_exact(c(by, head(target_var, -1))))) %>%
        # Replace the by variables and target variable names with `row_label<n>`
        replace_by_string_names(quos(!!!by, summary_var))

      formatted_data <- full_join(formatted_data, formatted_stats_data,
                                  by = vars_select(names(formatted_data), starts_with("row_label")))
    }




  }, envir = x)

  add_order_columns(x)

  env_get(x, "formatted_data")

}

#' Format n counts for display in count_layer
#'
#' left padding = (maximum_n_width - this_n_width)
#' right padding = (maximum_layer_width - this_layer_width[after left padding])
#'
#' @param .n Vector of counts for each cell
#' @param .total  Vector of totals. Should be the same length as .n and be the
#'   denominator that column is based off of.
#' @param count_fmt The f_str object the strings are formatted around.
#' @param max_layer_length The maximum layer length of the whole table
#' @param max_n_width The maximum length of the actual numeric counts
#' @param .distinct_n Vector of distinct counts
#' @param .distinct_total Vector of total counts for distinct
#' @param missing_string The value of the string used to note missing. Usually NA
#' @param missing_f_str The f_str object used to display missing values
#' @param summary_var The summary_var values that contain the values of the
#'   target variable.
#' @param indentation_length If this is a nested count layer. The row prefixes
#'   must be removed
#'
#' @return A tibble replacing the original counts
#' @noRd
construct_count_string <- function(.n, .total, .distinct_n = NULL, .distinct_total = NULL,
                                   count_fmt = NULL, max_layer_length, max_n_width, missing_string,
                                   missing_f_str, summary_var, indentation_length, total_count_format,
                                   total_row_label, has_missing_count) {

  ## Added this for processing formatting in nested count layers where this won't be processed yet
  if (is.null(max_layer_length)) max_layer_length <- 0
  if (is.null(max_n_width)) max_n_width <- 0
  missing_rows <- FALSE
  total_rows <- FALSE

  # Add in the missing format if its null and there are missing counts
  if(has_missing_count && is.null(missing_f_str)) {
    missing_f_str <- count_fmt
  }

  if (!is.null(missing_f_str)) {

    # This subsets the indentation length for nested count layers. The 'outer'
    # values will be cut off but they will never be "missing" so that shouldn't
    # be an issue.
    summary_var <- str_sub(summary_var, indentation_length)

    missing_rows <- summary_var %in% missing_string
    missing_vars_ord <- map_chr(missing_f_str$vars, as_name)
  }

  ## Pull out string information for total rows
  if(!is.null(total_count_format)) {
    total_rows <- summary_var %in% total_row_label
    total_vars_ord <- map_chr(total_count_format$vars, as_name)
  }

  vars_ord <- map_chr(count_fmt$vars, as_name)

  # str_all is a list that contains character vectors for each parameter that might be calculated
  str_all <- vector("list", 5)
  # Append the repl_str to be passed to do.call
  str_all[1] <- count_fmt$repl_str
  # Iterate over every variable
  for(i in seq_along(vars_ord)) {
    str_all[[i+1]] <-  count_string_switch_help(vars_ord[i], count_fmt, .n[!missing_rows & !total_rows], .total[!missing_rows & !total_rows],
                                                .distinct_n[!missing_rows & !total_rows], .distinct_total[!missing_rows & !total_rows], vars_ord)
  }


  # Logic for missing
  # Same logic as above, just add for missing
  missing_str_all <- vector("list", 5)
  missing_str_all[1] <- missing_f_str$repl_str
  for(i in seq_along(missing_vars_ord)) {
    missing_str_all[[i+1]] <- count_string_switch_help(missing_vars_ord[i], missing_f_str, .n[missing_rows], .total[missing_rows],
                                                       .distinct_n[missing_rows], .distinct_total[missing_rows], missing_vars_ord)
  }

  total_str_all <- vector("list", 5)
  total_str_all[1] <- total_count_format$repl_str
  for(i in seq_along(total_vars_ord)) {
    total_str_all[[i+1]] <- count_string_switch_help(total_vars_ord[i], total_count_format, .n[total_rows], .total[total_rows],
                                                     .distinct_n[total_rows], .distinct_total[total_rows], total_vars_ord)
  }

  # Put the vector strings together. Only include parts of str_all that aren't null
  # nm is non-missing, m is mising, and t is total.
  string_nm <- do.call(sprintf, str_all[!map_lgl(str_all, is.null)])
  if(!is.null(missing_vars_ord)) string_m <-  do.call(sprintf, missing_str_all[!map_lgl(missing_str_all, is.null)])
  if(!is.null(total_vars_ord)) string_t <- do.call(sprintf, total_str_all[!map_lgl(total_str_all, is.null)])
  # string_ is the final string to return. Merge the missing, non-missing, and others together
  string_ <- character(length(string_nm) + length(string_m) + length(string_t))
  string_[!missing_rows & !total_rows] <- string_nm
  string_[total_rows] <-   string_t
  string_[missing_rows] <-  string_m




  # Left pad set to 0 meaning it won't pad to the left at all
  # right pad is set to the maximum n count in the table
  string_ <- pad_formatted_data(string_, 0, max_n_width)

  string_
}

#' Switch statement used in processing
#'
#' @param x Current parameter to format
#' @param count_fmt f_str object used to format
#' @param .n values used in 'n'
#' @param .total values used in pct calculations
#' @param .distinct_n values used in 'distinct_n'
#' @param vars_ord values used in distinct pct
#'
#' @noRd
count_string_switch_help <- function(x, count_fmt, .n, .total,
                                     .distinct_n, .distinct_total, vars_ord){

  switch(x,
         "n" = map_chr(.n, num_fmt, which(vars_ord == "n"), fmt = count_fmt),
         "pct" = {
           # Makea vector of ratios between n and total. Replace na values with 0
           pcts <- replace(.n/.total, is.na(.n/.total), 0)
           # Make a vector of percentages
           map_chr(pcts*100, num_fmt, which(vars_ord == "pct"), fmt = count_fmt)
         },
         "distinct_n" =  map_chr(.distinct_n, num_fmt, which(vars_ord == "distinct_n"), fmt = count_fmt),
         "distinct_pct" = {
           # Same as pct
           pcts <- replace(.distinct_n/.distinct_total, is.na(.distinct_n/.distinct_total), 0)

           map_chr(pcts*100, num_fmt, which(vars_ord == "distinct_pct"), fmt = count_fmt)
         }
  )


}

#' @param x Count Layer
#'
#' When nesting a count layer in some cases a treatment group will not apear in one of the
#' groups so this will turn the variable into a factor to force it to complete in the
#' complete logic
#'
#' @noRd
factor_treat_var <- function(x) {
  evalq({

    built_target[, as_name(treat_var)] <- as.factor(unlist(built_target[, as_name(treat_var)]))

  }, envir = env_parent(x))
}


#' Prefix a row with a specifed character
#'
#' @param row_i The row to prefix
#' @param count_row_prefix The prefix
#'
#' @return The modified row
#' @noRd
prefix_count_row <- function(row_i, count_row_prefix) {

  paste0(count_row_prefix, row_i)

}

#' @noRd
process_count_denoms <- function(x) {

  evalq({

    # This used in case there is a character passed to the layer
    layer_params <- c(target_var, treat_var, by, cols)
    # Logical vector indicating if the param appears in the target dataset.
    param_apears <- map_lgl(layer_params, function(x) {
      as_name(x) %in% names(target)
    })

    # Raise errors if a denom is ignored but there isn't a missing count string
    if(!is.null(denom_ignore) && is.null(missing_count_string)) {
      abort("A value(s) were set with 'denom_ignore' but no missing count was set. Your percentages/totals may not have meaning.")
    }

    # Logic to determine how to subset target for denominators
    if(is.null(denom_where)) {
      denom_where <- where
    }

    # Because the missing strings haven't replaced the missing strings, it has to happen here.
    # Expand denoms contains the
    if(!is.null(missing_count_list)) {
      expand_denoms <- names(missing_count_list) %in% unlist(denom_ignore)
      denom_ignore <- c(denom_ignore, unname(missing_count_list[expand_denoms]))
    }


    # Subset the local built_target based on where
    # Catch errors
    tryCatch({
      denom_target <- built_target_pre_where %>%
        filter(!!denom_where) %>%
        filter(!(!!target_var[[1]] %in% unlist(denom_ignore)))
    }, error = function(e) {
      abort(paste0("group_count `where` condition `",
                   as_label(denom_where),
                   "` is invalid. Filter error:\n", e))
    })


    if(!is.null(distinct_by)) {
      # For distinct counts, we want to defer back to the
      # population dataset. Trigger this by identifying that
      # the population dataset was overridden
      if (!isTRUE(try(identical(pop_data, target)))) {
        if(deparse(denom_where) != deparse(where)){
          warning(paste0("A `denom_where` has been set with a pop_data. The `denom_where` has been ignored.",
          "You should use `set_pop_where` instead of `set_denom_where`.", sep = "\n"),
          immediate. = TRUE)
        }
        denoms_distinct_df <- built_pop_data %>%
          rename(!!treat_var := !!pop_treat_var)
      } else {
        denoms_distinct_df <- denom_target
      }
      denoms_distinct_df <- denoms_distinct_df %>%
        distinct(!!!distinct_by, !!treat_var, .keep_all = TRUE) %>%
        group_by(!!!cols, !!treat_var) %>%
        summarize(distinct_n = n()) %>%
        ungroup() %>%
        complete(!!!cols, !!treat_var, fill = list(distinct_n = 0))
    }

    denoms_df <- denom_target %>%
      group_by(!!!layer_params[param_apears]) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      complete(!!!layer_params[param_apears], fill = list(n = 0))

    if(as_name(target_var[[1]]) %in% names(target)) {
      denoms_df <- denoms_df %>%
        rename("summary_var" := !!target_var[[1]])
    }

  }, envir = x)

}

rename_missing_values <- function(x) {
  evalq({
    # Rename missing values
    if(!is.null(missing_count_list)){
      missing_count_list_ <- missing_count_list
      # If the target variable isn't a character or a factor. Coerse it as a
      # character. This can happen if the target var is numeric
      if(!(class(built_target[, as_name(target_var[[1]])][[1]]) %in% c("factor", "character"))) {
        built_target <- built_target %>%
          mutate(!!target_var[[1]] := as.character(!!target_var[[1]]))
      }
      # Collapse the factors that were missing.
      for(i in seq_along(missing_count_list)) {

        # Logic if the missing_count_list contains an implicit NA
        if(any(is.nan(missing_count_list[[i]]))){
          ## Repalce the NA in the missing_count list with an explicit value
          missing_count_list_[[i]] <- ifelse(missing_count_list[[i]] == "NaN", "(Missing_NAN)", as.character(missing_count_list[[i]]))
          # Replace the implicit values in built_target
          built_target <- built_target %>%
            mutate(!!target_var[[1]] := fct_expand(!!target_var[[1]], "(Missing_NAN)")) %>%
            mutate(!!target_var[[1]] := ifelse(is.nan(!!target_var[[1]]), "(Missing_NAN)", as.character(!!target_var[[1]])))

        } else if(any(is.na(missing_count_list[[i]]))){
          ## Repalce the NA in the missing_count list with an explicit value
          missing_count_list_[[i]] <- ifelse(is.na(as.character(missing_count_list[[i]])) , "(Missing)", as.character(missing_count_list[[i]]))
          # Replace the implicit values in built_target
          built_target <- built_target %>%
            mutate(!!target_var[[1]] := fct_expand(!!target_var[[1]], "(Missing)")) %>%
            mutate(!!target_var[[1]] := fct_explicit_na(!!target_var[[1]]))

        }
        built_target <- built_target %>%
          mutate(
            # Warnings suppressed here. They can happen if something is called missing
            # That isn't in the data, that isn't something to warn about in this context
            !!target_var[[1]] := suppressWarnings(fct_collapse(!!target_var[[1]], !!names(missing_count_list)[i] := missing_count_list_[[i]]))
          )
      }
    }
  }, envir = x)
}
