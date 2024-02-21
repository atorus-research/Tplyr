#' Process numeric data for a layer of type \code{desc}
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_summaries.desc_layer <- function(x, ...) {

  # If format strings weren't provided, then grab the defaults
  if (!has_format_strings(x)) {
    # Grab the defaults available at the table or option level
    params <- gather_defaults(x)
    # Place the formats
    x <- do.call('set_format_strings', append(x, params))
  }

  # Execute in the layer environment
  evalq({
    # trans_sums is the data that will pass forward to be formatted
    trans_sums <- vector("list", length(target_var))
    # num_sums is the data that will be bound together and returned to provide
    # the numeric internal values
    # num_sums_raw is kept separate to better facililate use for prep of metadata
    num_sums_raw <- vector("list", length(target_var))
    num_sums <- vector("list", length(target_var))

    # Get the row labels out from the format strings list
    row_labels <- name_translator(format_strings)

    # Subset the local built_target based on where
    # Catch errors
    tryCatch({
      built_target <- built_target %>%
        filter(!!where)
    }, error = function(e) {
      abort(paste0("group_desc `where` condition `",
                   as_label(where),
                   "` is invalid. Filter error:\n", e))
    })

    # Extract the list of summaries that need to be performed
    for (i in seq_along(target_var)) {

      # Pull out the target variable being iterated
      cur_var <- target_var[[i]]

      # Get the summaries that need to be performed for this layer
      summaries <- get_summaries()[match_exact(summary_vars)]

      # Create the numeric summary data
      cmplt1 <- built_target %>%
        # Rename the current variable to make each iteration use a generic name
        rename(.var = !!cur_var) %>%
        # Group by treatment, provided by variable, and provided column variables
        group_by(!!treat_var, !!!by, !!!cols) %>%
        # Execute the summaries
        summarize(!!!summaries) %>%
        ungroup()

      num_sums_raw[[i]] <- complete_and_limit(cmplt1, treat_var, by, cols, limit_data_by=limit_data_by)

      # Create the transposed summary data to prepare for formatting
      trans_sums[[i]] <- num_sums_raw[[i]] %>%
        # Transpose the summaries that make up the first number in a display string
        # into the the `value` column with labels by `stat`
        pivot_longer(cols = match_exact(trans_vars), names_to = "stat") %>%
        rowwise() %>%
        # Add in the row labels
        mutate(
           row_label = row_labels[[stat]]
        )

      # If precision is required, then create the variable identifier
      if (need_prec_table) {
        trans_sums[[i]] <- trans_sums[[i]] %>%
          mutate(
            precision_on = as_name(precision_on)
          )
      }

      # Numeric data needs the variable names replaced and add summary variable name
      num_sums[[i]] <- replace_by_string_names(num_sums_raw[[i]], by) %>%
        mutate(summary_var = as_name(cur_var)) %>%
        select(summary_var, everything())

      # Clean up loop
      rm(cur_var, summaries, i)
    }

    # Bind the numeric data together within the layer
    numeric_data <- pivot_longer(bind_rows(num_sums), cols = match_exact(summary_vars), names_to = "stat")

  }, envir=x)
}

#' Format processing for desc layers
#'
#' @param x layer object
#'
#' @return Formatted and processed data
#' @noRd
#' @export
process_formatting.desc_layer <- function(x, ...) {

  # Execute in the layer environment
  evalq({
    # Initialize list for formatted, transposed outputs
    form_sums <- vector("list", length(target_var))

    if (need_prec_table) {
      if ('prec' %in% ls()) {
        # If precision data was manually specified, grab it
        prec <- get_prec_data(built_target, prec, precision_by, precision_on, cap, prec_error)
      } else {
        # Otherwise create it
        prec <- make_prec_data(built_target, precision_by, precision_on, cap)
      }
    }

    for (i in seq_along(trans_sums)) {
      # Format the display strings - this is just applying construct_desc_string to each row of
      # the data.frame

      if (need_prec_table) {
        # Merge the precision data on
        trans_sums[[i]] <- left_join(trans_sums[[i]], prec, by=c(match_exact(precision_by), 'precision_on'))
      }

      # Reset the scientific notation presentation settings temporarily
      trans_sums[[i]]['display_string'] <- pmap_chr(trans_sums[[i]],
                                            function(...) construct_desc_string(...,
                                                                                .fmt_str = format_strings),
                                            format_strings=format_strings)

      # Now do one more transpose to split the columns out
      # Default is to use the treatment variable, but if `cols` was provided
      # then also transpose by cols.
      if (stats_as_columns) {
        form_sums[[i]] <- trans_sums[[i]] %>%
          pivot_wider(id_cols=c(!!treat_var, match_exact(by)), # Keep row_label and the by variables
                      names_from = match_exact(vars(row_label, !!!cols)), # Pull the names from treatment and cols argument
                      names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                      values_from = display_string # Use the created display_string variable for values
          )

      } else {
        form_sums[[i]] <- trans_sums[[i]] %>%
          pivot_wider(id_cols=c('row_label', match_exact(by)), # Keep row_label and the by variables
                      names_from = match_exact(vars(!!treat_var, !!!cols)), # Pull the names from treatment and cols argument
                      names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                      values_from = display_string # Use the created display_string variable for values
        )

      }
    }

    # Join the final outputs
    if (stats_as_columns) {
      formatted_data <- reduce(form_sums, full_join, by=c(as_label(treat_var), match_exact(by)))

      # Replace row label names
      formatted_data <- replace_by_string_names(formatted_data, by, treat_var)
    } else {
      formatted_data <- reduce(form_sums, full_join, by=c('row_label', match_exact(by)))

      # Replace row label names
      formatted_data <- replace_by_string_names(formatted_data, by)
    }


    # Don't want to delete this until I'm absolutely sure it's not necessary
    # formatted_data <- formatted_data %>%
    #   rowwise() %>%
    #   # Replace NA values with the proper empty strings
    #   mutate_at(vars(starts_with('var')), ~ replace_na(.x, format_strings[[row_label]]$empty))


    # Clean up
    rm(form_sums, i)

    formatted_data <- assign_row_id(formatted_data, 'd')

  }, envir=x)

  add_order_columns(x)

  env_get(x, "formatted_data")
}

# Small helper function to help with builtins
inf_to_na <- function(x) if_else(is.infinite(x), NA, x)

#' Get the summaries to be passed forward into \code{dplyr::summarize()}
#'
#' @param e the environment summaries are stored in.
#'
#' @return A list of expressions to be unpacked in \code{dplyr::summarize}
#' @noRd
get_summaries <- function(e = caller_env()) {

  # Define the default list of summaries
  summaries <- exprs(
    n       = n(),
    mean    = mean(.var, na.rm=TRUE),
    sd      = sd(.var, na.rm=TRUE),
    median  = median(.var, na.rm=TRUE),
    var     = var(.var, na.rm=TRUE),
    min     = inf_to_na(min(.var, na.rm=TRUE)),
    max     = inf_to_na(max(.var, na.rm=TRUE)),
    iqr     = IQR(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type')),
    q1      = quantile(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type'))[[2]],
    q3      = quantile(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type'))[[4]],
    missing = sum(is.na(.var))
  )

  append(summaries, get_custom_summaries(e), after=0)
}

#' Format a descriptive statistics display string
#'
#' Intended to be applied through a map_chr call
#'
#' @param ... A row of a data frame - captured through the ellipsis argument
#' @param .fmt_str The format strings container with all f_str objects and row labels
#'
#' @return A character vector of display formatted strings
#' @noRd
construct_desc_string <- function(..., .fmt_str=NULL) {
  # Unpack names into current namespace for ease
  list2env(list2(...), envir=environment())

  # Get the current format to be applied
  fmt <- .fmt_str[[row_label]]

  # If all the values summarized are NA then return the empty string
  if (all(is.na(append(map(fmt$vars[-1], eval, envir=environment()), value)))) {
    if ('.overall' %in% names(fmt$empty)) {
      return(fmt$empty['.overall'])
    }
  }

  # Make the autos argument
  if (fmt$auto_precision) {
    autos <- c('int'=max_int, 'dec'=max_dec)
  } else {
    autos <- c('int'=0, 'dec'=0)
  }
  # Format the transposed value
  fmt_args <- list(fmt = fmt$repl_str, num_fmt(value, 1, fmt, autos))


  # Now evaluate any additional numbers that must be formatted
  # Exclude the initial variable because it's already been evaluated
  # i is intended to start on the second argument so +1 in the num_fmt call
  fmt_args <- append(fmt_args,
                     imap(fmt$vars[-1],
                          function(val, i, fmt, autos) num_fmt(eval(val), i+1, fmt, autos),
                          fmt=fmt,
                          autos=autos)
  )

  # Apply the call to sprintf
  do.call(sprintf, fmt_args)
}
