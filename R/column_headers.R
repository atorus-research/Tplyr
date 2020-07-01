string <- "row label 1 {test}| row label 2 | Spanning header {Inside header1 | Inside header 2} | Spanning header 2 {Inside header1 | Inside header 2}"


# Find the inside groups
op <- str_locate_all(string, fixed("{"))[[1]][,1]
cl <- str_locate_all(string, fixed("}"))[[1]][,1]

# Make sure that brackets are all balanced
if (length(op) != length(cl)) {
  abort("Unmatched brackets for spanning headers")
}

## Get the spanned text ----
# Initialize vector for the spanned text
spanned <- character(length(op))


for (i in seq_along(op)) {
  # Check that the next opening bracket isn't before the current closing (i.e. no nesting)
  if (i+1 <= length(op) && (cl[i] > op[i + 1])) {
    abort("Nested spanning headers are not yet supported")
  }

  # Grab the spanned section
  spanned[[i]] <- str_sub(string, start=op[i], end=cl[i])
}

## Build the spanner row ----
## Get the spanner headers
spanner_locs <- str_locate_all(string, "[^{\\|]*\\{")[[1]]

# Initialize output vector
spanning_row <- character(nrow(spanner_locs))
for (i in 1:nrow(spanner_locs)) {
  # Pick out the location info
  locs <- spanner_locs[i, ]

  # Grab the text of the spanner header
  txt <- str_trim(str_sub(string, locs['start'], locs['end']-1))

  # Prefix with | to pad how many cells are before the text and commit into spanning vector
  spanning_row[[i]] <- paste(c(rep("|", str_count(str_sub(string, 1, locs['end']), fixed("|"))), txt), collapse="")

}

# [^\\{\\|]*\\{[^\\}]*\\}


# Process the outside groups
