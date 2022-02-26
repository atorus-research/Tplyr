
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

