

t_ <- tplyr_table(mtcars, gear, cols = vs) %>%
  add_total_group() %>%
  add_layer(
    group_count(cyl, by = am) %>%
      set_distinct_by(cyl) %>%
      set_format_strings(f_str("a (xx.xx%) [xxx] [xx.xx%]", distinct_n, distinct_pct, n, pct))
  )


test_that("tplyr_table is printed as expected", {

  expect_snapshot_output(print(t_))

  expect_snapshot_output(str(t_))

  build(t_)

  expect_snapshot_output(print(t_))

  expect_snapshot_output(str(t_))

})

test_that("tplyr layers are printed as expected", {
  expect_snapshot_output(print(t_$layers[[1]], print_env = FALSE))

  expect_snapshot_output(str(t_$layers[[1]], print_env = FALSE))
})

test_that("f_str objects are printed as expected", {
  expect_snapshot_output(print(t_$layers[[1]]$format_strings))

  expect_snapshot_output(str(t_$layers[[1]]$format_strings))
})
