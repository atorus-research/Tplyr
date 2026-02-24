# Replace repeating row label variables with blanks in preparation for display.

Depending on the display package being used, row label values may need
to be blanked out if they are repeating. This gives the data frame
supporting the table the appearance of the grouping variables being
grouped together in blocks. `apply_row_masks` does this work by blanking
out the value of any row_label variable where the current value is equal
to the value before it. Note - `apply_row_masks` assumes that the data
frame has already be sorted and therefore should only be applied once
the data frame is in its final sort sequence.

## Usage

``` r
apply_row_masks(dat, row_breaks = FALSE, ...)
```

## Arguments

- dat:

  Data.frame / tibble to mask repeating row_labels

- row_breaks:

  Boolean - set to TRUE to insert row breaks

- ...:

  Variable used to determine where row-breaks should be inserted. Breaks
  will be inserted when this group of variables changes values. This is
  determined by dataset order, so sorting should be done prior to using
  `apply_row_masks`. If left empty, `ord_layer_index` will be used.

## Value

tibble with blanked out rows where values are repeating

## Details

Additionally, `apply_row_masks` can add row breaks for you between each
layer. Row breaks are inserted as blank rows. This relies on the "break
by" variables (submitted via `...`) constructed in `build` still being
attached to the dataset. An additional order variable is attached named
`ord_break`, but the output dataset is sorted to properly insert the row
breaks between layers.
