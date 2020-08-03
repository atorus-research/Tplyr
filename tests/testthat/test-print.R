

t_ <- tplyr_table(mtcars, gear, cols = vs) %>%
  add_total_group() %>%
  add_layer(
    group_count(cyl, by = am) %>%
      set_distinct_by(cyl) %>%
      set_format_strings(f_str("a (xx.xx%) [xxx] [xx.xx%]", distinct, distinct_pct, n, pct))
  )


test_that("tplyr_table is printed as expected", {

  expect_known_output(print(t_), test_path("table_print.txt"))

  expect_known_output(str(t_), test_path("table_str.txt"))

  build(t_)

  expect_known_output(print(t_), test_path("table_built_print.txt"))

  expect_known_output(str(t_), test_path("table_built_str.txt"))

})

test_that("tplyr layers are printed as expected", {
  expect_output_file(print(t_$layers[[1]], print_env = FALSE), test_path("layer_print.txt"))

  expect_known_output(str(t_$layers[[1]], print_env = FALSE), test_path("layer_str.txt"))
})

test_that("f_str objects are printed as expected", {
  expect_known_output(print(t_$layers[[1]]$format_strings), test_path("fstr_print.txt"))

  expect_known_output(str(t_$layers[[1]]$format_strings), test_path("fstr_str.txt"))
})
