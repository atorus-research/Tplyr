#' @importFrom rlang env enquo enquos caller_env abort inform is_quosure quo_get_expr quo_is_null env_get env_bind env_has quo_is_missing
#' @importFrom rlang call_modify call_standardise call_name call_args is_call current_env quo_name trace_back is_function
#' @importFrom rlang expr exprs enexprs enexpr is_named env_parent env_label is_logical is_empty is_quosures quo_is_symbol sym := as_name
#' @importFrom rlang quos quo env_names env_bind_active as_label eval_tidy
#' @importFrom stringr str_split str_extract_all regex str_detect str_replace_all str_replace str_locate_all fixed str_count str_trim
#' @importFrom purrr flatten map map_lgl pmap_chr imap reduce map_chr map_int map_dbl map_dfr pmap_dfr walk2 map2 map2_dfr walk
#' @importFrom stringr str_sub str_extract str_pad str_starts
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom magrittr %>% extract
#' @importFrom assertthat assert_that
#' @importFrom stats IQR median sd quantile var
#' @importFrom dplyr n summarize filter vars tally ungroup group_by mutate lag select bind_rows full_join add_tally distinct rowwise
#' @importFrom dplyr everything rename mutate_at mutate_all as_tibble bind_cols do case_when arrange left_join row_number between mutate_if
#' @importFrom tidyr complete nesting pivot_wider pivot_longer replace_na starts_with
#' @importFrom utils str head tail
#' @importFrom tidyselect all_of vars_select
#' @importFrom tibble tibble rownames_to_column add_column
#' @importFrom lifecycle deprecate_soft deprecate_stop
#' @importFrom stats var
#' @importFrom forcats fct_expand fct_collapse fct_explicit_na
NULL

#' A grammar of summary data for clinical reports
#'
#' `r lifecycle::badge("experimental")`
#'
#' 'Tplyr' is a package dedicated to simplifying the data manipulation necessary
#' to create clinical reports. Clinical data summaries can often be broken down
#' into two factors - counting discrete variables (or counting shifts in state),
#' and descriptive statistics around a continuous variable. Many of the reports
#' that go into a clinical report are made up of these two scenarios. By
#' abstracting this process away, 'Tplyr' allows you to rapidly build these
#' tables without worrying about the underlying data manipulation.
#'
#' 'Tplyr' takes this process a few steps further by abstracting away most of
#' the programming that goes into proper presentation, which is where a great
#' deal of programming time is spent. For example, 'Tplyr' allows you to easily
#' control:
#'
#' \describe{ \item{\strong{String formatting}}{Different reports warrant
#' different presentation of your strings. Programming this can get tedious, as
#' you typically want to make sure that your decimals properly align. 'Tplyr'
#' abstracts this process away and provides you with a simple interface to
#' specify how you want your data presented} \item{\strong{Treatment
#' groups}}{Need a total column? Need to group summaries of multiple treatments?
#' 'Tplyr' makes it simple to add additional treatment groups into your report}
#' \item{\strong{Denominators}}{n (\%) counts often vary based on the summary
#' being performed. 'Tplyr' allows you to easily control what denominators are
#' used based on a few common scenarios} \item{\strong{Sorting}}{Summarizing
#' data is one thing, but ordering it for presentation. Tplyr automatically
#' derives sorting variable to give you the data you need to order your table
#' properly. This process is flexible so you can easily get what you want by
#' leveraging your data or characteristics of R.} }
#'
#' Another powerful aspect of 'Tplyr' are the objects themselves. 'Tplyr' does
#' more than format your data. Metadata about your table is kept under the hood,
#' and functions allow you to access information that you need. For example,
#' 'Tplyr' allows you to calculate and access the raw numeric data of
#' calculations as well, and easily pick out just the pieces of information that
#' you need.
#'
#' Lastly, 'Tplyr' was built to be flexible, yet intuitive. A common pitfall of
#' building tools like this is over automation. By doing to much, you end up not
#' doing enough. 'Tplyr' aims to hit the sweet spot in between. Additionally, we
#' designed our function interfaces to be clean. Modifier functions offer you
#' flexibility when you need it, but defaults can be set to keep the code
#' concise. This allows you to quickly assemble your table, and easily make
#' changes where necessary.
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' # Use just the defaults
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(mpg, by=cyl)
#'   ) %>%
#'   add_layer(
#'     group_count(carb, by=cyl)
#'   ) %>%
#'   build()
#'
#' # Customize and modify
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(mpg, by=cyl) %>%
#'       set_format_strings(
#'         "n"         = f_str("xx", n),
#'         "Mean (SD)" = f_str("a.a+1 (a.a+2)", mean, sd, empty='NA'),
#'         "Median"    = f_str("a.a+1", median),
#'         "Q1, Q3"    = f_str("a, a", q1, q3, empty=c(.overall='NA')),
#'         "Min, Max"  = f_str("a, a", min, max),
#'         "Missing"   = f_str("xx", missing)
#'       )
#'   ) %>%
#'   add_layer(
#'     group_count(carb, by=cyl) %>%
#'       add_risk_diff(
#'         c('5', '3'),
#'         c('4', '3')
#'       ) %>%
#'       set_format_strings(
#'         n_counts = f_str('xx (xx%)', n, pct),
#'         riskdiff = f_str('xx.xxx (xx.xxx, xx.xxx)', dif, low, high)
#'       ) %>%
#'       set_order_count_method("bycount") %>%
#'       set_ordering_cols('4') %>%
#'       set_result_order_var(pct)
#'   ) %>%
#'   build()
#'
#' # A Shift Table
#' tplyr_table(mtcars, am) %>%
#'   add_layer(
#'     group_shift(vars(row=gear, column=carb), by=cyl) %>%
#'     set_format_strings(f_str("xxx (xx.xx%)", n, pct))
#'   ) %>%
#'   build()
#'
#' @docType package
#' @name Tplyr
"_PACKAGE"

# Default options ----
tplyr_default_options <- list(

  # Count layer defaults
  tplyr.count_layer_default_formats =
    list(n_counts = f_str("a (xxx.x%)", distinct_n, distinct_pct),
         riskdiff = f_str('xx.xxx (xx.xxx, xx.xxx)', dif, low, high)
         ),

  # Desc layer defaults
  tplyr.desc_layer_default_formats =
    list("n"        = f_str("xxx", n),
         "Mean (SD)"= f_str("a.a+1 (a.a+2)", mean, sd),
         "Median"   = f_str("a.a+1", median),
         "Q1, Q3"   = f_str("a.a+1, a.a+1", q1, q3),
         "Min, Max" = f_str("a.a, a.a", min, max),
         "Missing"  = f_str("xxx", missing)
         ),

  # Shift layer defaults
  tplyr.shift_layer_default_formats = list(f_str("a", n)),

  # Precision caps for decimal and integer precision
  tplyr.precision_cap = c('int' = 99, 'dec'=99),

  # Custom summaries
  tplyr.custom_summaries = NULL,

  # Set to avoid printing in scientific notation
  tplyr.scipen = 1000,

  # Quantile algorithm setting
  tplyr.quantile_type = 7
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
pop_where <- NULL
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
distinct_stat <- NULL
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
..index <- NULL
denoms_by <- NULL
.distinct_n <- NULL
count_layer_formats <- NULL
desc_layer_formats <- NULL
denoms_df <- NULL
is_built_nest <- FALSE
by_saved <- NULL
target_var_saved <- NULL
indentation_length <- NULL
missing_count_string <- NULL
missing_string <- NULL
denom_ignore <- NULL
denoms_distinct_df <- NULL
outer_inf <- NULL
shift_layer_formats <- NULL
.tmp_name <- NULL
include_total_row <- NULL
ord_layer_index <- NULL
ord_break <- NULL
missing_vars_ord <- NULL
total_vars_ord <- NULL
string_m <- NULL
string_t <- NULL
total_count_format <- NULL
missing_count_list <- NULL
total_denom_ignore <- NULL
total_row_sort_value <- NULL
missing_sort_value <- NULL
missing_index <- NULL
total_index <- NULL
process_distinct_total <- FALSE
total_stat_denom <- NULL
denom_where <- NULL
built_target_pre_where <- NULL
count_fmt <- NULL
count_missings <- NULL
has_missing_count <- FALSE
