#' @importFrom rlang env enquo enquos caller_env abort inform is_quosure quo_get_expr quo_is_null env_get env_bind env_has quo_is_missing
#' @importFrom rlang call_modify call_standardise call_name call_args as_label is_call as_label current_env quo_name trace_back is_function
#' @importFrom rlang expr exprs enexprs enexpr is_named env_parent env_label is_logical is_empty is_quosures quo_is_symbol sym := as_name
#' @importFrom rlang quos quo env_names
#' @importFrom stringr str_split str_extract_all regex str_detect str_replace_all str_replace str_locate_all fixed str_count str_trim
#' @importFrom stringr str_sub
#' @importFrom purrr flatten map map_lgl pmap_chr imap reduce map_chr map_int map_dbl map_dfr
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @importFrom stats IQR median sd quantile
#' @importFrom dplyr summarize filter vars tally ungroup group_by mutate lag select bind_rows full_join add_tally distinct rowwise
#' @importFrom dplyr everything rename mutate_at mutate_all as_tibble
#' @importFrom tidyr complete nesting pivot_wider pivot_longer replace_na starts_with
#' @importFrom utils str head
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
NULL

#' A grammar of summary data for clinical reports
#'
#' 'Tplyr' is a package dedicated to simplifying the data manipulation necessary to create clinical reports. Clinical data summaries can
#' often be broken down into two factors - counting discrete variables (or counting shifts in state), and descriptive statistics around
#' continuous variable. Many of the reports that go into a clinical report are simply made up of these two scenarios. By abstracting
#' this process away, 'Tplyr' allows you to rapidly build these tables without worrying about the underlying data manipulation
#'
#' 'Tplyr' takes this process a few steps further by abstracting away all of the programming that goes into proper presentation, which
#' is where even more of the time is spent when programming these reports. For example, 'Tplyr' allows you to easily control:
#'
#' \describe{
#' \item{\strong{Denominators}}{n (\%) counts often vary based on the summary being performed. 'Tplyr' allows you to easily control what denominators
#' are used based on a few common scenarios, or additionally you can provide a custom dataset used in calculation
#' \strong{NOTE:} This feature is still being developed and is not yet implemented}
#' \item{\strong{String formatting}}{Different reports warrant different presentation of your strings. Programming this can get tedious, as you always
#' want to make sure that your decimals properly align. 'Tplyr' abstracts this process away and provides you with a simple format to specify
#' how you want your data presented}
#' \item{\strong{Treatment groups}}{Need a total column? Need to group summaries of multiple treatments? 'Tplyr' makes this simple to add additional
#' treatment groups into your report}
#' }
#'
#' Furthermore, 'Tplyr' was built to be extensible. A common pitfall in creating these solutions is over-automation. Some utilities just do
#' to much, but don't solve all of the problems. We know we won't handle every scenario - and therefore we've left channels open for you to
#' add your own touches where necessary.
#'
#' @docType package
#' @name tplyr
"_PACKAGE"

# Default options ----
tplyr_default_options <- list(
  tplyr.debug = FALSE
)

# Carry out process on load ----
.onLoad <- function(libname, pkgname) {
  # store existing options
  op <- options()

  # Set any options that haven't been set
  toset <- !(names(tplyr_default_options) %in% names(op))
  if(any(toset)) options(tplyr_default_options[toset])

  invisible()
}

target_var <- NULL
target <- NULL
where <- NULL
sort_vars <- NULL
layers <- NULL
layer_output <- NULL
mask <- NULL
row_label <- NULL
value <- NULL
cols <- NULL
empty <- NULL
pct <- NULL
numeric_data <- NULL
format_strings <- NULL
max_layer_length <- NULL
max_n_width <- NULL
Total <- NULL
display_string <- NULL
built_target <- NULL
table_where <- NULL
distinct_by <- NULL
summary_vars <- NULL
trans_vars <- NULL
stat <- NULL
summary_var <- NULL
spanner_locs <- NULL
spanned_sects <- NULL
op <- NULL
cl <- NULL
q1 <- NULL
q3 <- NULL
