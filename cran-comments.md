## Submission 1.4.0

This is a minor release (current CRAN version 1.3.3). It adds one backwards
compatible feature and fixes two bugs:

* New `max_int` and `max_dec` arguments to `set_format_strings()`, along with a
  corresponding `tplyr.max_precision` option, allowing an overall maximum
  precision to be applied after auto-precision and any '+' modifier are
  resolved (#219). The defaults reproduce the previous behaviour exactly.
* Fix table level defaults from `set_shift_layer_formats()` being silently
  ignored by shift layers that do not set their own format strings (#216).
* Fix incorrect `add_missing_subjects_row()` counts on nested count layers,
  where `set_distinct_by()` was not applied to the inner layer (#217).

## Test Environments

* Local macOS (aarch64-apple-darwin20), R 4.5.1
* GitHub Actions:
  * windows-latest (R release)
  * macOS-latest (R release)
  * ubuntu-latest and ubuntu-22.04 (R release and R devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Tplyr has no strong reverse dependencies (Depends/Imports/LinkingTo). Two
packages list it under Suggests (clinify, logrx); the new arguments and option
are additive with defaults that preserve existing output, so they are
unaffected.
