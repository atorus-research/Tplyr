## Submission 1.4.1

This is a patch release (current CRAN version 1.4.0). It fixes a single bug,
with no user-facing API change:

* Fix an unnamed `empty` value in `f_str()` being ignored for descriptive
  statistics rows where every summarized value is missing, which left the cell
  blank instead of filling the format string (#215). This was a regression
  introduced in 1.3.2.

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
packages list it under Suggests (clinify, logrx); this patch restores
previously documented formatting behaviour and makes no API change, so they
are unaffected.
