## Submission 1.3.3

This is a patch release (current CRAN version 1.3.2). It fixes a single bug, with
no user-facing API change:

* Resolve a spurious "no non-missing arguments to max" warning and an invalid
  `-Inf` sort value produced when a count layer targets an all-missing (`NA`)
  variable (#213).

## Test Environments

* Local macOS (aarch64-apple-darwin20), R 4.5.1
* GitHub Actions:
  * windows-latest (R release)
  * macOS-latest (R release)
  * ubuntu-latest and ubuntu-22.04 (R release and R devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

This NOTE reflects the local machine's inability to reach a time server and is
unrelated to the package.

## Reverse dependencies

Tplyr has no strong reverse dependencies (Depends/Imports/LinkingTo). Two
packages list it under Suggests (clinify, logrx); this patch makes no
user-facing API change and does not affect them.
