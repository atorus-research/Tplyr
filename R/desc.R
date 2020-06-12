#' Process a layer of type \code{desc}
#'
#' @param e Environment within which a
#'
#' @return Processed data within layer environment
process_desc_layer <- function(e) {
  evalq({
    # Extract the list of summaries that need to be performed
    summaries <- get_summaries(target_var)

    # Start the tplyr processing
    summary_dat <- target %>%
      filter(!!where) %>%
      group_by(!!pop_treat_var, !!!by) %>%
      summarize(!!!summaries)


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







# process_desc_layer(l)

# evalq(class(environment()), envir=l)
