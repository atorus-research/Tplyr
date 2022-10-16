# Tplyr 1.0.1
- Bug fixes
  - Resolve issue where `modify_nested_call()` fails if Tplyr is not loaded (#95)

# Tplyr 1.0.0

- New features
  - Introduction of traceability metadata framework #32
  - Framework for creating re-usable layer templates #66
  - Native pipe compatibility #33
  - Automatically load magrittr pipe #22
  - Refactor of count layer programming #28
  - Allow external precision data for desc layers #27
  - Allow denominators within count layers as formattable values #11
  - Descriptive statistics layers allow stats as columns #37
  - New experimental function `set_numeric_threshold()`
  - Apply f_str's outside of a Tplyr table with new function `apply_formats()` #57
  - New post processing function helper `str_indent_wrap()` for hyphen-enabled string wrapping #59
- Bug fixes
  - Fix errors in partially provided precision caps #20

# Tplyr 0.4.4

- Added new functionality per issue #10. Adds 'Both' an option for sorting outer layers of nested count.


# Tplyr 0.4.3

- Fixes bug #12 where posix class columns would cause the `all.equal` check between
target and pop_data to error.
- Incorporates pull request #13 for change to tidyr.

# Tplyr 0.4.2

- No functionality updates. Tests updated to pass rlang 1.0.0.

# Tplyr 0.4.1

- Bug Fixes
  - Nested count layers with character values in the first position could error
  if multiple risk differences were added.
  - Improved handling of factors in the treatment variable.

# Tplyr 0.4.0

- Enhancements
  - In certain cases when creating a count layer, you may only want to keep
  certain factors from your target dataset. Tplyr now has this functionality
  built in! With the `keep_levels()` you can define what factors you want to
  keep in your count layers without having to recode/drop factors at the table
  level.
  - Tplyr would normally use the R native rounding method and that is the method
  we recommend. However, in certain cases you may be trying to match your Tplyr
  output with SAS. You can set the 'tplyr.IBMRounding' option to TRUE, and Tplyr
  will simulate IBM rounding.
  - `set_denoms_by()` has been enhanced for nested count layers. You can now your
  nested count target variables as denominators.
  
- Bug fixes
  - `add_risk_difference()` would error out when you used it in a nested count
  layer that had a character value as the first variable.
  - Nested count layers could not be sorted `bycount` if the layer level where
  logic caused a value to be droped. This was fixed and tested for future
  development.
  

# Tplyr 0.3.1

- Enhancements
  - The process for determining `by` variable indicies was changed from <VAR>N -> factor -> alphabetical to factor -> <VAR>N -> alphabetical to allow users to override variables that have <VAR>N counterparts that might have additional values not present in the target.
  - You can now use text strings as the first variable in nested count.
  
- Bug Fixes
  - A bug fix where factors in by variables weren't indexed properly was resolved.
  - Several documentation updates for clarity and changed functionality.
  - Improved error messages and error handling in some places.

- Other changes
  - Event counts are now noted as 'distinct_n' instead of 'distinct' in count format strings. 'distinct' may still be used but results in a warning that it should no longer be used. Using both 'distinct' and 'distinct_n' results in an error.
  - Updated for changes in how tibble uses attributes. 

# Tplyr 0.2.2

- Enhancements
  - `set_missing_count()` interface was made more intuitive. A new argument `denom_ignore` was added.
  - `set_denom_ignore()` is now defunct. It was replaced with the `denom_ignore` argument in `set_missing_count()`
  - `add_total_row()` now uses the `count_missing` argument and will no longer have any side effects on the denominators.
  - `set_denom_where()` is now usable by shift layers.

# Tplyr 0.2.1

- Bug Fixes
  - Fixed a bug caused by an update to `tibble` 3.0.4 that caused factors to be displayed incorrectly in row labels and sorting columns to populate incorrectly.
  - A bug where the factors used in the shift layers wouldn't be reflected in the ordering columns.

- Enhancements
  - The `add_total_row()` interface has been updated. It now takes an f_str object can be formatted differently than the rest of the table. A parameter was also added note if total rows should include missing counts.
  - The `set_missing_count()` interface was updated. The 'string' parameter was removed and replaced with the ellipsis. Instead of passing a vector, a user would pass any number of character vectors that are named.
  - Build will error if `denom_ignore` is used but no missing count strings are specified.
  - A new function, `set_denom_where()` was added to allow a user to change how the denominators are filtered when calculating percentages.
 
- Other changes
  - The version of dplyr that gets imported was updated to 1.0.0. The version of tidyselect imported was updated to 1.1.0. This was updated to remove warnings in the count layer build process.

# Tplyr 0.1.4

- Bug Fixes
 - Fixes a bug where "Totals" in numeric data may not take into account the where logic at the layer level and thus give inaccurate percentages

- Other Changes
 - `add_total_row()` function is more intuitive. It now uses the `denoms_by` variables to determine how to calculate the totals.


# Tplyr 0.1.3

- Bug Fixes
  - Fixes a bug where 'N' counts in column headers would display as 0 when a distinct_by and custom groupings were used.
  
- Other Changes
  - Ordering layer columns are now unnamed vectors. For varn and factor ordering columns they could previously be named which could be unexpected.
  - The names of the data.frames used in target and pop_data are now attributes of the tplyr table object and not the data.frames themselves.
  - The UAT application now gives a warning if an error happened during validation, or confirms that all tests pass.

# Tplyr 0.1.2

- Bug Fixes
  - Fixes a bug where percentages in count layers would appear as 'Inf' when a distinct_by variable and custom groupings were used. GitHub Issue #8

# Tplyr 0.1.1

Initial release onto CRAN.

- Bug Fixes/Enhancements
  - Count layers were re-factored to improve the execution efficiency
  - Auto-precision now works without a `by` variable
  - Several new assertions have been added to give clearer error messages
  - Treatment groups within the population data will produce columns in the resulting build, even if no records exist for that treatment group in the target dataset
  - Risk difference variable names will now populate properly when a `cols` argument is used
  - Data frame attributes are cleaned prior to processing to prevent any merge/bind warnings during processing
  - Total values within count layers are properly filled when the resulting count is 0 (largely impacts risk-difference calculations)
- Feature additions
  - Shift layers are here! 
  - Flexibility when filling missing values has been enhanced for descriptive statistic layers
  - Layers can now be named, and those names can be used in `get_numeric_data` and the new function `get_statistics_data` to get risk difference raw numbers. Data may also be filtered directly from both functions. 
  - Default formats can now be set via options or at the table level, which allows you to eliminate a great deal of redundant code

# Tplyr 0.1.0

Beta release for Tplyr with introduction of numerous new features:

General updates:
  - Calculate your header N counts based on
the population dataset or the target dataset. The alpha release had an
option to set the population data but this wasn’t actually used anywhere
in the internals.
  - Use these header N counts as token replacements when
using the `add_column_headers` function.
  - Order variables are now added
to the built dataset to allow you to sort the output dataset as you wish
with numeric variables.
- Count layer updates:
  - Optionally use the
population data N counts as denominators for percent calculation.
  - For multi-level count summaries, nest the row label columns together to
present both row labels in a single column
  - You can now present both distinct and non-distinct counts instead of one or
the other
  - Sorting options allow you to order results from the target variable values or
from derived counts within a specified column
  - Risk difference calculations can now be added as additional columns, with
flexible options for presentation
- Descriptive statistics layer updates:
  - The custom summary functionality has
been updated to apply to multi-variable summaries, which results in an interface
change
  - Automatic decimal precision has been added to allow you to base the
presentation on the precision of the data as collected

# Tplyr 0.1.0.9999

Initial alpha release of Tplyr

