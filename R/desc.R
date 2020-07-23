#' Process numeric data for a layer of type \code{desc}
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_summaries.desc_layer <- function(x, ...) {

  if (!has_format_strings(x)) {
    x <- set_format_strings(x,
      "n"        = f_str("xx", n),
      "Mean (SD)"= f_str("xx.x (xx.xx)", mean, sd),
      "Median"   = f_str("xx.x", median),
      "Q1, Q3"   = f_str("xx, xx", q1, q3),
      "Min, Max" = f_str("xx, xx", min, max),
      "Missing"  = f_str("xx", missing)
    )
  }
  # Execute in the layer environment
  evalq({
    # trans_sums is the data that will pass forward to be formatted
    trans_sums <- vector("list", length(target_var))
    # num_sums is the data that will be bound together and returned to provide the numeric internal values
    num_sums <- vector("list", length(target_var))

    # Get the row labels out from the format strings list
    row_labels <- name_translator(format_strings)

    # Extract the list of summaries that need to be performed
    for (i in seq_along(target_var)) {

      # Pull out the target variable being iterated
      cur_var <- target_var[[i]]

      # Get the summaries that need to be performed for this layer
      summaries <- get_summaries()[match_exact(summary_vars)]

      # Create the numeric summary data
      num_sums[[i]] <- built_target %>%
        # Rename the current variable to make each iteration use a generic name
        rename(.var = !!cur_var) %>%
        # Subset by the logic specified in `where`
        filter(!!where) %>%
        # Group by treatment, provided by variable, and provided column variables
        group_by(!!treat_var, !!!by, !!!cols) %>%
        # Execute the summaries
        summarize(!!!summaries)

      # Create the transposed summary data to prepare for formatting
      trans_sums[[i]] <- num_sums[[i]] %>%
        # Transpose the summaries that make up the first number in a display string
        # into the the `value` column with labels by `stat`
        pivot_longer(cols = match_exact(trans_vars), names_to = "stat") %>%
        rowwise() %>%
        # Add in the row labels
        mutate(
           row_label = row_labels[[stat]]
        )

      # Numeric data needs the variable names replaced and add summary variable name
      num_sums[[i]] <- replace_by_string_names(num_sums[[i]], by) %>%
        mutate(summary_var = as_label(cur_var)) %>%
        select(summary_var, everything())

      # Clean up loop
      rm(cur_var, summaries, i)
    }

    # Bind the numeric data together within the layer
    numeric_data <- pivot_longer(bind_rows(num_sums), cols = match_exact(summary_vars), names_to = "stat")

    # Delete the listed numeric data
    rm(num_sums)

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

    for (i in seq_along(trans_sums)) {
      # Format the display strings - this is just applying construct_desc_string to each row of
      # the data.frame
      trans_sums[[i]]['display_string'] <- pmap_chr(trans_sums[[i]],
                                            function(...) construct_desc_string(..., .fmt_str = format_strings),
                                            format_strings=format_strings
      )

      # String pad each of the display strings to match the longest value across layers
      # TODO: Introduce auto-padding after alhpa release
      # trans_sums[[i]] <- trans_sums[[i]] %>%
      #   mutate(display_string = str_pad(display_string, max_layer_length, side='right'))

      # Now do one more transpose to split the columns out
      # Default is to use the treatment variable, but if `cols` was provided
      # then also tranpose by cols.
      form_sums[[i]] <- trans_sums[[i]] %>%
        pivot_wider(id_cols=c('row_label', match_exact(by)), # Keep row_label and the by variables
                    names_from = match_exact(vars(!!treat_var, !!!cols)), # Pull the names from treatment and cols argument
                    names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                    values_from = display_string # Use the created display_string variable for values
        )
    }

    # Join the final outputs
    formatted_data <- reduce(form_sums, full_join, by=c('row_label', match_exact(by))) %>%
      rowwise() %>%
      # Replace NA values with the proper empty strings
      mutate_at(vars(starts_with('var')), ~ replace_na(.x, format_strings[[row_label]]$empty))

    # Replace row label names
    formatted_data <- replace_by_string_names(formatted_data, by)

    # Clean up
    rm(trans_sums, form_sums, i)

    formatted_data
  }, envir=x)
}

#' Get the summaries to be passed forward into \code{dplyr::summarize()}
#'
#' @param e the environment summaries are stored in.
#'
#' @return A list of expressions to be unpacked in \code{dplyr::summarize}
get_summaries <- function(e = caller_env()) {

  # Define the default list of summaries
  summaries <- exprs(
    n       = n()                             ,
    mean    = mean(.var, na.rm=TRUE)         ,
    sd      = sd(.var, na.rm=TRUE)           ,
    median  = median(.var, na.rm=TRUE)       ,
    var     = var(.var, na.rm=TRUE)          ,
    min     = min(.var, na.rm=TRUE)          ,
    max     = max(.var, na.rm=TRUE)          ,
    iqr     = IQR(.var, na.rm=TRUE)          ,
    q1      = quantile(.var, na.rm=TRUE)[[2]],
    q3      = quantile(.var, na.rm=TRUE)[[4]],
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
  list2env(list(...), envir=environment())

  # Get the current format to be applied
  fmt <- .fmt_str[[row_label]]

  # Format the transposed value
  fmt_args <- list(fmt = fmt$repl_str, num_fmt(value, 1, fmt))

  # Now evaluate any additional numbers that must be formatted
  # Exclude the initial variable because it's already been evaluated
  # i is intended to start on the second argument so +1 in the num_fmt call
  fmt_args <- append(fmt_args,
                     imap(fmt$vars[-1], function(val, i, fmt) num_fmt(eval(val), i+1, fmt), fmt=fmt)
  )

  # Apply the call to sprintf
  do.call(sprintf, fmt_args)
}
