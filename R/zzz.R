#' @importFrom rlang env enquo enquos caller_env abort inform is_quosure quo_get_expr quo_is_null env_get env_bind env_has quo_is_missing
#' @importFrom rlang call_modify call_standardise call_name call_args as_label is_call as_label current_env quo_name trace_back is_function
#' @importFrom rlang expr exprs enexprs enexpr is_named env_parent env_label is_logical is_empty is_quosures quo_is_symbol sym := as_name
#' @importFrom rlang quos quo env_names env_bind_active
#' @importFrom stringr str_split str_extract_all regex str_detect str_replace_all str_replace str_locate_all fixed str_count str_trim
#' @importFrom purrr flatten map map_lgl pmap_chr imap reduce map_chr map_int map_dbl map_dfr pmap_dfr walk2 map2 map2_dfr
#' @importFrom stringr str_sub str_extract
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom magrittr %>% %<>%
#' @importFrom assertthat assert_that
#' @importFrom stats IQR median sd quantile var
#' @importFrom dplyr n summarize filter vars tally ungroup group_by mutate lag select bind_rows full_join add_tally distinct rowwise
#' @importFrom dplyr everything rename mutate_at mutate_all as_tibble bind_cols do case_when arrange left_join
#' @importFrom tidyr complete nesting pivot_wider pivot_longer replace_na starts_with
#' @importFrom utils str head tail
#' @importFrom tidyselect all_of vars_select
#' @importFrom tibble tibble rownames_to_column
#' @importFrom lifecycle deprecate_soft
#' @importFrom stats var
NULL

#' A grammar of summary data for clinical reports
#'
#' \lifecycle{experimental}
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
total <- NULL
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
.var <- NULL
total_stat <- NULL
distinct_n <- NULL
summary_stat <- NULL
total_row_label <- NULL
distinct_total <- NULL
.distinct_total <- NULL
count_row_prefix <- NULL
inner_count_layer_prefix <- NULL
ordering_cols <- NULL
order_count_method <- NULL
result_order_var <- NULL
nest_sort_index <- NULL
.data <- NULL
. <- NULL
built_pop_data <- NULL
factor_index <- NULL
formatted_data <- NULL
dots <- NULL
stats <- NULL
nest_count <- NULL
dif <- NULL
comparisons <- NULL
n <- NULL
low <- NULL
high <- NULL
row_label1 <- NULL
comp_numeric_data <- NULL
indentation <- NULL
max_int <- NULL
max_dec <- NULL
need_prec_table <- NULL
precision_by <- NULL
precision_on <- NULL
cap <- NULL
