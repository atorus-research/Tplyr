
## Make sure calls to add_layer all work correctly ----
test_that("Layers are attached correctly in add_layers", {

  # Basic call
  t1 <- tplyr_table(mtcars, cyl) %>%
    add_layer(
      group_desc(mpg)
    )


  # With piping
  t2 <- tplyr_table(mtcars, cyl) %>%
    add_layer(
      group_desc(mpg) %>%
        set_format_strings('Mean (SD)' = f_str('xxx (xxx)', mean, sd))
    )


  # Native piping
  t3 <- tplyr_table(mtcars, cyl) %>%
    add_layer(
      set_format_strings(group_desc(mpg), 'Mean (SD)' = f_str('xxx (xxx)', mean, sd))
    )

  expect_identical(parent.env(t1$layers[[1]]), t1)
  expect_identical(parent.env(t2$layers[[1]]), t2)
  expect_identical(parent.env(t3$layers[[1]]), t3)
})

test_that("add_layer only accepts Tplyr functions", {
  expect_snapshot_error(
    tplyr_table(mtcars, cyl) %>%
      add_layer(
        mean(c(1,2,3))
      )
  )

  expect_snapshot_error(
    tplyr_table(mtcars, cyl) %>%
      add_layer(
        group_desc(mpg) %>%
          set_format_strings('Mean (SD)' = f_str('xxx (xxx)', mean, sd)) %>%
          mean()
      )
  )

  expect_snapshot_error(
    tplyr_table(mtcars, cyl) %>%
      add_layer(
        set_format_strings(mean(group_desc(mpg)), 'Mean (SD)' = f_str('xxx (xxx)', mean, sd))
      )
  )
})

## apply_row_masks tests ----
test_that("Apply row masks errors trigger properly", {

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(am)
    ) %>%
    build()

  # Non-variable names
  expect_snapshot_error(apply_row_masks(t, row_breaks=TRUE, x+y))
  expect_snapshot_error(apply_row_masks(t, row_breaks=TRUE, "hello"))
  # Variable not included
  expect_snapshot_error(apply_row_masks(t, row_breaks=TRUE, ord_bad_name))
  expect_snapshot_error(apply_row_masks(t, row_breaks=TRUE, ord_bad_name, ord_other_bad_name))
  # Variables submitted must be ord variables in the build dataset
  expect_snapshot_error(apply_row_masks(t, row_breaks=TRUE, row_label1))
  expect_snapshot_error(apply_row_masks(t, row_breaks=TRUE, row_label1, var1_3))

})

test_that("apply_row_masks masks repeating row labels", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(am)
    ) %>%
    build()

  masked <- apply_row_masks(t)

  # Row labels should be character
  expect_type(masked$row_label1, "character")
})

test_that("apply_row_masks inserts row breaks", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(am)
    ) %>%
    add_layer(
      group_count(vs)
    ) %>%
    build()

  result <- apply_row_masks(t, row_breaks = TRUE)

  # Row breaks should add rows
  expect_gt(nrow(result), nrow(t))
  # ord_break column should exist
  expect_true("ord_break" %in% names(result))
})

test_that("apply_row_masks with row_breaks does not mask across layers", {
  # Reprex from GitHub issue #183
  df <- data.frame(
    "Subject" = c("A001", "A002", "B001", "B002", "B003"),
    "Cohort" = c("A", "A", "B", "B", "B"),
    "AE_related" = c("No", "No", "No", "No", "No"),
    "DLT" = c("No", "No", "No", "No", "No"),
    "SAE" = c("Yes", "No", "No", "No", "No")
  )

  result <- tplyr_table(df, Cohort) %>%
    add_layer(
      group_count(AE_related, by = "AE_related") %>%
        set_distinct_by(Subject) %>%
        set_format_strings(f_str("xxx (xx.x%); xxx", distinct_n, distinct_pct, n))
    ) %>%
    add_layer(
      group_count(DLT, by = "DLT") %>%
        set_distinct_by(Subject) %>%
        set_format_strings(f_str("xxx (xx.x%); xxx", distinct_n, distinct_pct, n))
    ) %>%
    add_layer(
      group_count(SAE, by = "SAE") %>%
        set_distinct_by(Subject) %>%
        set_format_strings(f_str("xxx (xx.x%); xxx", distinct_n, distinct_pct, n))
    ) %>%
    build() %>%
    apply_row_masks(row_breaks = TRUE)

  # Get only data rows (not break rows)
  data_rows <- result[result$ord_break == 1, ]

  # Each layer's first data row has row_label2 "No" - these should NOT be
  # blanked out across layers since row breaks separate them
  layer_first_rows <- data_rows[!duplicated(data_rows$ord_layer_index), ]
  expect_true(all(layer_first_rows$row_label2 == "No"))
})

