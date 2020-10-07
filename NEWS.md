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

