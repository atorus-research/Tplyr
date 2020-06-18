#' Process a layer of type \code{desc}
#'
#' @param e Environment within which a
#'
#' @return Processed data within layer environment
process_desc_layer <- function(e) {
  evalq({
    out <- vector("list", length(target_var))

    # Extract the list of summaries that need to be performed
    for (i in seq_along(target_var)) {
      row_labels <- name_translator(format_strings)

      var <- target_var[[i]]
      as_label(var)

      summaries <- get_summaries(var)[match_exact(summary_vars)]

      # Start the tplyr processing
      current <- target %>%
        filter(!!where) %>%
        group_by(!!treat_var, !!!by, !!!cols) %>%
        summarize(!!!summaries) %>%
        pivot_longer(match_exact(trans_vars), names_to = "stat") %>%
        rowwise() %>%
        mutate(
           row_label = row_labels[[stat]]
        )

      current['display_string'] <- pmap_chr(current,
                                            function(...) construct_desc_string(..., .fmt_str = format_strings),
                                            format_strings=format_strings
                                            )
      out[[i]] <- current %>%
        pivot_wider(id_cols=c('row_label', match_exact(by)),
                    names_from = match_exact(vars(!!treat_var, !!!cols)),
                    names_prefix = paste0(as_label(var), "_"),
                    values_from = display_string
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






# process_desc_layer(l)

# evalq(class(environment()), envir=l)
