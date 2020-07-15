
#' @noRd
#' @export
process_summaries.count_layer <- function(x, ...) {

  # Preprocssing in the case of two target_variables
  if(length(env_get(x, "target_var")) > 2) abort("Only up too two target_variables can be used in a count_layer")

  else if(length(env_get(x, "target_var")) == 2) {

    # Change treat_var to factor so all combinations appear in nest
    factor_treat_var(x)

    # Begin with the layer itself and process the first target vars values one by one
    env_bind(x, numeric_data = map_dfr(unlist(get_target_levels(x, env_get(x, "target_var")[[1]])),
            bind_nested_count_layer, x = x))

  } else {

    process_single_count_target(x)

  }

  prepare_format_metadata(x)

  x
}

#' @param x A count layer with a single target_var
#'
#' This function uses dplyr to filter out the where call, pull out the distinct
#' rows if applicable, and tallys the different target_var values.
#'
#' If include_total_row is true a row will be added with a total row labeled
#' with total_row_label.
#'
#' Complete is used to complete the combinaions of by, treat_var, and target_var
#'
#' @noRd
process_single_count_target <- function(x) {
  evalq({

    if(!exists("include_total_row")) include_total_row <- FALSE
    if(!exists("total_row_label")) total_row_label <- "Total"

    # Construct the counts for each target grouping
    summary_stat <- built_target %>%
      # Filter out based on where
      filter(!!where)

    # get unique variables based on distinct_by value
    if (!is.null(distinct_by)) {
      summary_stat <- summary_stat %>%
        # Distinct based on the current distinct_by, target_var, and treat_var
        # treat_var is added because duplicates would be created when there are
        # treatment group totals
        distinct(!!distinct_by, !!treat_var, !!!target_var, .keep_all = TRUE)
    }

    summary_stat <- summary_stat %>%
      # Group by varaibles including target variables and count them
      group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
      tally(name = "value") %>%
      ungroup() %>%
      # Group by all column variables
      group_by(!!treat_var, !!!cols) %>%
      add_tally(name = "Total", wt = value) %>%
      ungroup() %>%
      # complete all combiniations of factors to include combiniations that don't exist.
      # add 0 for combintions that don't exist
      complete(!!treat_var, !!!by, !!!target_var, !!!cols, fill = list(value = 0, Total = 0)) %>%
      # Change the treat_var and first target_var to characters to resolve any
      # issues if there are total rows and the original column is numeric
      mutate(!!treat_var := as.character(!!treat_var)) %>%
      mutate(!!as_label(target_var[[1]]) := as.character(!!target_var[[1]]))

    # If there is no values in summary_stat, which can happen depending on where. Return nothing
    if(nrow(summary_stat) == 0) return()

    total_stat <- NULL
    if(include_total_row) {
      # create a data.frame to create total counts
      total_stat <- summary_stat %>%
        # Group by all column variables
        group_by(!!treat_var, !!!cols) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(Total = value) %>%
        # Create a variable to label the totals when it is merged in.
        mutate(!!as_label(target_var[[1]]) := total_row_label) %>%
        # Create variables to carry forward 'by'. Only pull out the ones that
        # aren't symbols
        group_by(!!!extract_character_from_quo(by)) %>%
        # complete based on missing groupings
        complete(!!treat_var, !!!cols, fill = list(value = 0, Total = 0))
    }

    # rbind tables together
    numeric_data <- summary_stat %>%
      bind_rows(total_stat)

    rm(summary_stat, total_stat)
  }, envir = x)
}

#' Prepare metadata for table
#'
#' @param x count_layer object
#' @noRd
prepare_format_metadata <- function(x) {
  evalq({

    # Get formatting metadata prepared
    if(is.null(format_strings)) format_strings <- f_str("ax (xxx.x%)", n, pct)

    # Pull max character length from counts. Should be at least 1
    n_width <- max(c(nchar(numeric_data$value), 1L))

    # If a layer_width flag is present, edit the formatting string to display the maximum
    # character length
    if(str_detect(format_strings$format_string, "ax")) {
      # Replace the flag with however many xs
      replaced_string <- str_replace(format_strings$format_string, "ax",
                                     paste(rep("x", n_width), collapse = ""))

      # Make a new f_str and replace the old one
      format_strings <- f_str(replaced_string, n, pct)
    }
    max_length <- format_strings$size
  }, envir = x)
}

#' @noRd
#' @export
process_formatting.count_layer <- function(x, ...) {
  evalq({

    formatted_data <- numeric_data %>%
      mutate(value = construct_count_string(value, Total, format_strings, max_layer_length, max_n_width)) %>%
      # Pivot table
      pivot_wider(id_cols = c(match_exact(by), match_exact(target_var)),
                  names_from = c(!!treat_var, match_exact(cols)), values_from = value,
                  names_prefix = "var1_") %>%
      # Replace String names for by and target variables. target variables are included becasue they are
      # equivilant to by variables in a count layer
      replace_by_string_names(c(by, target_var))
  }, envir = x)
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
#'
#' @return A tibble replacing the originial counts
construct_count_string <- function(.n, .total, count_fmt = NULL,
                                   max_layer_length, max_n_width) {

  str1 <- NA
  if("n" %in% count_fmt$vars) {
    # Make a vector of ncounts
    str1 <- map_chr(.n, num_fmt, 1, fmt = count_fmt)
  }

  str2 <- NA
  if("pct" %in% count_fmt$vars) {
    # Makea vector of ratios between n and total. Replace na values with 0
    pcts <- replace(.n/.total, is.na(.n/.total), 0)
    # Make a vector of percentages
    str2 <- map_chr(pcts*100, num_fmt, 2, fmt = count_fmt)
  }

  # Put the vector strings together
  string_ <- sprintf(count_fmt$repl_str, str1, str2)

  string_ <- pad_formatted_data(string_, max_layer_length, max_n_width)

  string_
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

#' #' Process a layer of type \code{group_count}
#' #'
#' #' @param e A layer environment
#' #'
#' #' @return A
#' #' @export
#' #'
#' #' @examples
#' process_count_layer <- function(e) {
#'   evalq({
#'
#'     if(!exists("count_fmt")) count_fmt <- f_str("ax (xxx.x%)", n, pct)
#'     if(!exists("include_total_row")) include_total_row <- TRUE
#'
#'     # Construct the counts for each target grouping
#'     summary_stat <- built_target %>%
#'       # Filter out based on where
#'       filter(!!where) %>%
#'       # Group by varaibles including target variables and count them
#'       group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
#'       tally() %>%
#'       ungroup() %>%
#'       # Group by all column variables
#'       group_by(!!treat_var, !!!cols) %>%
#'       add_tally(name = "Total", wt = n) %>%
#'       ungroup() %>%
#'       # complete all combiniations of factors to include combiniations that don't exist.
#'       # add 0 for combintions that don't exist
#'       complete(!!treat_var, !!!by, !!!target_var, !!!cols, fill = list(n = 0, Total = 0))
#'
#'     # If there is no values in summary_stat, which can happen depending on where. Return nothing
#'     if(nrow(summary_stat) == 0) return()
#'
#'     #
#'     total_stat <- NULL
#'     if(include_total_row) {
#'       # create a data.frame to create total counts
#'       total_stat <- summary_stat %>%
#'         # filter out based on where
#'         filter(!!where) %>%
#'         # Group by all column variables
#'         group_by(!!treat_var, !!!cols) %>%
#'         summarise(n = sum(n)) %>%
#'         ungroup() %>%
#'         mutate(Total = n) %>%
#'         # Create a variable to label the totals when it is merged in.
#'         mutate(!!target_var[[1]] := "Total") %>%
#'         # Create variables to carry forward 'by'
#'         group_by(!!!by) %>%
#'         # complete based on missing groupings
#'         complete(!!treat_var, !!!cols, fill = list(n = 0, Total = 0))
#'     }
#'
#'
#'     # rbind tables together
#'       built_table <- summary_stat %>%
#'         bind_rows(total_stat) %>%
#'         mutate(n = construct_count_string(n, Total, count_fmt)) %>%
#'         # Pivot table
#'         pivot_wider(id_cols = c(match_exact(by), match_exact(target_var)),
#'                     names_from = c(!!treat_var, match_exact(cols)), values_from = n,
#'                     names_prefix = "var1_") %>%
#'         # Replace String names for by and target variables. target variables are included becasue they are
#'         # equivilant to by variables in a count layer
#'         replace_by_string_names(c(by, target_var))
#'
#'   }, envir = e)
#' }



