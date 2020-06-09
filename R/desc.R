#' Process a layer of type \code{desc}
#'
#' @param e Environment within which a
#'
#' @return Processed data within layer environment
process_desc_layer <- function(e) {
  evalq({
    .dat <- target %>%
      filter(!!where) %>%
      group_by(!!pop_treat_var) %>%
      summarize(!!!tplyr_summaries)
  }, envir=e)
}

#' Get the summaries to be passed forward into \code{dplyr::summarize()}
#'
#' @return A list of expressions to be unpacked in \code{dplyr::summarize}
get_summaries <- function(e = caller_env()) {

  # Define the default list of summaries
  summaries <- list(
    n      = expr( n()                             ),
    mean   = rlang::expr( mean({{target_var}})     ),
    sd     = expr( sd({{ target_var }})            ),
    var    = expr( var({{ target_var }})           ),
    median = expr( median({{ target_var }})        ),
    min    = expr( min({{ target_var }})           ),
    max    = expr( max({{ target_var }})           ),
    iqr    = expr( IQR({{ target_var }})           ),
    q1     = expr( quantile({{ target_var }})[[2]] ),
    q3     = expr( quantile({{ target_var }})[[4]] )
  )

  append(summaries, get_custom_summaries(e), after=0)

}





# process_desc_layer(l)

# evalq(class(environment()), envir=l)
