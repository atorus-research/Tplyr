#' Process a layer of type \code{desc}
#'
#' @param e Environment within which a
#'
#' @return Processed data within layer environment
process_desc_layer <- function(e) {
  # Execute in the layer environment
  evalq({
    # Allocate the list elements for the output list
    out <- vector("list", length(target_var))

    # Extract the list of summaries that need to be performed
    for (i in seq_along(target_var)) {

      # Get the summaries that need to be performed for this layer
      summaries <- get_summaries(var)[match_exact(summary_vars)]

      # Get the row labels out from the format strings list
      row_labels <- name_translator(format_strings)

      # Pull out the target variable being iterated
      var <- target_var[[i]]

      # Start the tplyr processing
      current <- target %>%
        # Subset by the logic specified in `where`
        filter(!!where) %>%
        # Group by treatment, provided by variable, and provided column variables
        group_by(!!treat_var, !!!by, !!!cols) %>%
        # Execute the summaries
        summarize(!!!summaries) %>%
        # Transpose the summaries that make up the first number in a display string
        # into the the `value` column with labels by `stat`
        pivot_longer(match_exact(trans_vars), names_to = "stat") %>%
        rowwise() %>%
        # Add in the row labels
        mutate(
           row_label = row_labels[[stat]]
        )

      # Format the display strings - this is just applying construct_desc_string to each row of
      # the data.frame
      current['display_string'] <- pmap_chr(current,
                                            function(...) construct_desc_string(..., .fmt_str = format_strings),
                                            format_strings=format_strings
                                            )

      # Now do one more transpose to split the columns out
      # Default is to use the treatment variable, but if `cols` was provided
      # then also tranpose by cols.
      out[[i]] <- current %>%
        pivot_wider(id_cols=c('row_label', match_exact(by)), # Keep row_label and the by variables
                    names_from = match_exact(vars(!!treat_var, !!!cols)), # Pull the names from treatment and cols argument
                    names_prefix = paste0(as_label(var), "_"), # Prefix with the name of the target variable
                    values_from = display_string # Use the created display_string variable for values
                    )
    }
    out

  }, envir=e)
}

#' Get the summaries to be passed forward into \code{dplyr::summarize()}
#'
#' @param var A varaible to perform summaries on.
#' @param e the environment summaries are stored in.
#'
#' @return A list of expressions to be unpacked in \code{dplyr::summarize}
get_summaries <- function(var, e = caller_env()) {

  # Define the default list of summaries
  summaries <- exprs(
    n       = n()                 ,
    mean    = mean(!!var)         ,
    sd      = sd(!!var)           ,
    var     = var(!!var)          ,
    median  = median(!!var)       ,
    min     = min(!!var)          ,
    max     = max(!!var)          ,
    iqr     = IQR(!!var)          ,
    q1      = quantile(!!var)[[2]],
    q3      = quantile(!!var)[[4]],
    missing = sum(is.na(!!var))
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
#'
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
