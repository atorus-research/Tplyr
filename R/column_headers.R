string <- "row label 1 {test | test2}| row label 2 | Spanning header {Inside header1 | Inside header 2} | Spanning header 2 {Inside header1 | Inside header 2}"


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
spanned_sects <- str_locate_all(string,"[^\\{\\|]*\\{[^\\}]*\\}")[[1]]

# Initialize output vector
spanning_row <- character(nrow(spanner_locs) + 1)
bottom_row <- string

for (i in 1:nrow(spanner_locs)) {
  # Pick out the location info
  top_locs <-spanner_locs[i, ]
  sect_locs <- spanned_sects[i ,]


  # Grab part of the string to count the | from (i.e. the end of the last section)
  sect_start <- ifelse(i == 1, 1, spanner_locs[i-1, ]['end'])

  # Grab the text of the spanner header
  top_txt <- str_trim(str_sub(string, top_locs['start'], top_locs['end']-1))
  sect_txt <- str_trim(str_sub(string, sect_locs['start'], sect_locs['end']-1))

  # Count the bars that need to be prefixed to this part of the spanner to split up cells
  bars <- str_count(str_sub(string, sect_start, top_locs['end']), fixed("|"))

  # Prefix with | to pad how many cells are before the text and commit into spanning vector
  spanning_row[[i]] <- paste(c(rep("|", bars), top_txt), collapse="")
}

# Add in the final number of bars
bars <- str_count(str_sub(string, top_locs['end']), fixed("|"))
spanning_row[[i + 1]] <- paste(c(rep("|", bars)), collapse="")

spanner_row_text <- paste(spanning_row, collapse="")
spanner_row_text

#


# Process the outside groups
