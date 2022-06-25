t <- tplyr_table(mtcars, cyl) %>%
  add_layer(
    group_desc(hp)
  )

dat <- t %>% build(metadata = TRUE)

m <- t$metadata

test_that("Metadata extractors error properly", {
  # Invalid row ID
  expect_snapshot_error(get_meta_result(t, "bad", 'var1_4'))

  # row_id not string
  expect_snapshot_error(get_meta_result(t, 2, 'var1_4'))

  # Not a present column
  expect_snapshot_error(get_meta_result(t, 'd1_1', "bad"))

  # Not a string
  expect_snapshot_error(get_meta_result(t, 'd1_1', 2))

  # Not a result column
  expect_snapshot_error(get_meta_result(t, 'd1_1', 'row_label1'))

  # add_cols not vars
  expect_snapshot_error(get_meta_subset(t, 'd1_1', 'var1_4', add_cols = "bad"))

  # get_meta_subset needs target
  expect_snapshot_error(get_meta_subset(m, 'd1_1', 'var1_4'))
})
