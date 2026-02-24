# Package index

## Build

High-level functions to create and build a table

- [`build()`](https://atorus-research.github.io/Tplyr/reference/build.md)
  :

  Trigger the execution of the `tplyr_table`

- [`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
  : Create a Tplyr table object

- [`pop_data()`](https://atorus-research.github.io/Tplyr/reference/pop_data.md)
  [`` `pop_data<-`() ``](https://atorus-research.github.io/Tplyr/reference/pop_data.md)
  [`set_pop_data()`](https://atorus-research.github.io/Tplyr/reference/pop_data.md)
  : Return or set population data bindings

- [`pop_treat_var()`](https://atorus-research.github.io/Tplyr/reference/pop_treat_var.md)
  [`set_pop_treat_var()`](https://atorus-research.github.io/Tplyr/reference/pop_treat_var.md)
  : Return or set pop_treat_var binding

## Layers and layering

Creating layers and adding them to the table

- [`add_layer()`](https://atorus-research.github.io/Tplyr/reference/layer_attachment.md)
  [`add_layers()`](https://atorus-research.github.io/Tplyr/reference/layer_attachment.md)
  :

  Attach a layer to a `tplyr_table` object

- [`tplyr_layer()`](https://atorus-research.github.io/Tplyr/reference/tplyr_layer.md)
  :

  Create a `tplyr_layer` object

- [`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
  [`group_desc()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
  [`group_shift()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
  :

  Create a `count`, `desc`, or `shift` layer for discrete count based
  summaries, descriptive statistics summaries, or shift count summaries

## Formatting

Customizing the display as a table

- [`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
  :

  Create a `f_str` object

- [`get_desc_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
  [`set_desc_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
  [`get_count_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
  [`set_count_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
  [`get_shift_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
  [`set_shift_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
  : Get or set the default format strings for descriptive statistics
  layers

- [`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
  : Set the format strings and associated summaries to be performed in a
  layer

- [`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md)
  : Set the display for missing strings

## Sorting

Sorting and customizing table order

- [`set_order_count_method()`](https://atorus-research.github.io/Tplyr/reference/ordering.md)
  [`set_ordering_cols()`](https://atorus-research.github.io/Tplyr/reference/ordering.md)
  [`set_result_order_var()`](https://atorus-research.github.io/Tplyr/reference/ordering.md)
  : Set the ordering logic for the count layer
- [`set_outer_sort_position()`](https://atorus-research.github.io/Tplyr/reference/set_outer_sort_position.md)
  : Set the value of a outer nested count layer to Inf or -Inf

## Adding Groups and Stats

Adding treatment groups, total rows, and risk difference

- [`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
  : Add a Total row into a count summary.
- [`add_treat_grps()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
  [`add_total_group()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
  [`treat_grps()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
  : Combine existing treatment groups for summary
- [`add_risk_diff()`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md)
  : Add risk difference to a count layer
- [`add_missing_subjects_row()`](https://atorus-research.github.io/Tplyr/reference/add_missing_subjects_row.md)
  : Add a missing subject row into a count summary.
- [`set_total_row_label()`](https://atorus-research.github.io/Tplyr/reference/set_total_row_label.md)
  : Set the label for the total row
- [`set_missing_subjects_row_label()`](https://atorus-research.github.io/Tplyr/reference/set_missing_subjects_row_label.md)
  : Set the label for the missing subjects row

## Descriptive Statistics Layer Functions

Descriptive statistics layer helper functions

- [`set_custom_summaries()`](https://atorus-research.github.io/Tplyr/reference/set_custom_summaries.md)
  : Set custom summaries to be performed within a descriptive statistics
  layer
- [`set_precision_data()`](https://atorus-research.github.io/Tplyr/reference/set_precision_data.md)
  : Set precision data
- [`get_precision_by()`](https://atorus-research.github.io/Tplyr/reference/precision_by.md)
  [`set_precision_by()`](https://atorus-research.github.io/Tplyr/reference/precision_by.md)
  : Set or return precision_by layer binding
- [`get_precision_on()`](https://atorus-research.github.io/Tplyr/reference/precision_on.md)
  [`set_precision_on()`](https://atorus-research.github.io/Tplyr/reference/precision_on.md)
  : Set or return precision_on layer binding
- [`set_stats_as_columns()`](https://atorus-research.github.io/Tplyr/reference/set_stats_as_columns.md)
  : Set descriptive statistics as columns

## Counting functions

Count layer helper functions

- [`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md)
  : Set variables used in pct denominator calculation
- [`set_distinct_by()`](https://atorus-research.github.io/Tplyr/reference/set_distinct_by.md)
  : Set counts to be distinct by some grouping variable.
- [`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md)
  : Set Logic for denominator subsetting
- [`set_nest_count()`](https://atorus-research.github.io/Tplyr/reference/set_nest_count.md)
  : Set the option to nest count layers
- [`set_outer_sort_position()`](https://atorus-research.github.io/Tplyr/reference/set_outer_sort_position.md)
  : Set the value of a outer nested count layer to Inf or -Inf
- [`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md)
  : Set the display for missing strings
- [`keep_levels()`](https://atorus-research.github.io/Tplyr/reference/keep_levels.md)
  : Select levels to keep in a count layer
- [`set_denom_ignore()`](https://atorus-research.github.io/Tplyr/reference/set_denom_ignore.md)
  : Set values the denominator calculation will ignore
- [`set_indentation()`](https://atorus-research.github.io/Tplyr/reference/set_indentation.md)
  : Set the option to prefix the row_labels in the inner count_layer
- [`set_numeric_threshold()`](https://atorus-research.github.io/Tplyr/reference/set_numeric_threshold.md)
  **\[experimental\]** : Set a numeric cutoff
- [`set_limit_data_by()`](https://atorus-research.github.io/Tplyr/reference/set_limit_data_by.md)
  : Set variables to limit reported data values only to those that exist
  rather than fully completing all possible levels

## Column Headers

Column header helpers

- [`add_column_headers()`](https://atorus-research.github.io/Tplyr/reference/add_column_headers.md)
  : Attach column headers to a Tplyr output
- [`header_n()`](https://atorus-research.github.io/Tplyr/reference/header_n.md)
  [`` `header_n<-`() ``](https://atorus-research.github.io/Tplyr/reference/header_n.md)
  [`set_header_n()`](https://atorus-research.github.io/Tplyr/reference/header_n.md)
  : Return or set header_n binding

## Metadata Functions

Tplyr metadata functions

- [`tplyr_meta()`](https://atorus-research.github.io/Tplyr/reference/tplyr_meta.md)
  : Tplyr Metadata Object
- [`add_variables()`](https://atorus-research.github.io/Tplyr/reference/metadata_additions.md)
  [`add_filters()`](https://atorus-research.github.io/Tplyr/reference/metadata_additions.md)
  : Add variables to a tplyr_meta object
- [`add_anti_join()`](https://atorus-research.github.io/Tplyr/reference/add_anti_join.md)
  : Add an anti-join onto a tplyr_meta object
- [`get_metadata()`](https://atorus-research.github.io/Tplyr/reference/get_metadata.md)
  : Get the metadata dataframe from a tplyr_table
- [`append_metadata()`](https://atorus-research.github.io/Tplyr/reference/append_metadata.md)
  : Append the Tplyr table metadata dataframe
- [`get_meta_result()`](https://atorus-research.github.io/Tplyr/reference/get_meta_result.md)
  : Extract the result metadata of a Tplyr table
- [`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md)
  : Extract the subset of data based on result metadata

## Templates

Layer Templates

- [`new_layer_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
  [`remove_layer_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
  [`get_layer_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
  [`get_layer_templates()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
  [`use_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
  : Create, view, extract, remove, and use Tplyr layer templates

## Post-pocessing

Post-pocessing functions

- [`apply_conditional_format()`](https://atorus-research.github.io/Tplyr/reference/apply_conditional_format.md)
  : Conditional reformatting of a pre-populated string of numbers
- [`apply_formats()`](https://atorus-research.github.io/Tplyr/reference/apply_formats.md)
  : Apply Format Strings outside of a Tplyr table
- [`apply_row_masks()`](https://atorus-research.github.io/Tplyr/reference/apply_row_masks.md)
  : Replace repeating row label variables with blanks in preparation for
  display.
- [`collapse_row_labels()`](https://atorus-research.github.io/Tplyr/reference/collapse_row_labels.md)
  : Collapse row labels into a single column
- [`replace_leading_whitespace()`](https://atorus-research.github.io/Tplyr/reference/replace_leading_whitespace.md)
  : Reformat strings with leading whitespace for HTML
- [`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md)
  [`str_extract_num()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md)
  : Extract format group strings or numbers
- [`str_indent_wrap()`](https://atorus-research.github.io/Tplyr/reference/str_indent_wrap.md)
  : Wrap strings to a specific width with hyphenation while preserving
  indentation

## Helper functions

General helper functions

- [`get_numeric_data()`](https://atorus-research.github.io/Tplyr/reference/get_numeric_data.md)
  : Retrieve the numeric data from a tplyr objects
- [`get_stats_data()`](https://atorus-research.github.io/Tplyr/reference/get_stats_data.md)
  : Get statistics data
- [`get_by()`](https://atorus-research.github.io/Tplyr/reference/by.md)
  [`set_by()`](https://atorus-research.github.io/Tplyr/reference/by.md)
  : Set or return by layer binding
- [`get_target_var()`](https://atorus-research.github.io/Tplyr/reference/target_var.md)
  [`set_target_var()`](https://atorus-research.github.io/Tplyr/reference/target_var.md)
  : Set or return treat_var binding
- [`treat_var()`](https://atorus-research.github.io/Tplyr/reference/treat_var.md)
  [`set_treat_var()`](https://atorus-research.github.io/Tplyr/reference/treat_var.md)
  : Return or set the treatment variable binding
- [`get_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
  [`set_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
  [`set_pop_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
  [`get_pop_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
  : Set or return where binding for layer or table
- [`Tplyr-package`](https://atorus-research.github.io/Tplyr/reference/Tplyr.md)
  [`Tplyr`](https://atorus-research.github.io/Tplyr/reference/Tplyr.md)
  : A grammar of summary data for clinical reports
- [`get_tplyr_regex()`](https://atorus-research.github.io/Tplyr/reference/get_tplyr_regex.md)
  : Retrieve one of Tplyr's regular expressions

## Data

Tplyr Built-in Datasets

- [`tplyr_adae`](https://atorus-research.github.io/Tplyr/reference/tplyr_adae.md)
  : ADAE Data
- [`tplyr_adas`](https://atorus-research.github.io/Tplyr/reference/tplyr_adas.md)
  : ADAS Data
- [`tplyr_adlb`](https://atorus-research.github.io/Tplyr/reference/tplyr_adlb.md)
  : ADLB Data
- [`tplyr_adsl`](https://atorus-research.github.io/Tplyr/reference/tplyr_adsl.md)
  : ADSL Data
- [`tplyr_adpe`](https://atorus-research.github.io/Tplyr/reference/tplyr_adpe.md)
  : ADPE Data
- [`get_data_labels()`](https://atorus-research.github.io/Tplyr/reference/get_data_labels.md)
  : Get Data Labels
