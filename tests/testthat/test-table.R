test_that("tplyr_table returns an empty envrionment of class 'tplyr_table' when passed no arguemnts", {
  st <- tplyr_table()
  expect_true(is.environment(st))
  expect_equal(length(rlang::env_names(st)), 0)
})

test_that("tplyr_table returns a class of tplyr_table and environment", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)
  expect_s3_class(tab, "tplyr_table")
  expect_s3_class(tab, "environment")
  expect_setequal(env_names(tab), c("target", "layers", "pop_data",
                                    "treat_var", "pop_treat_var", "header_n",
                                    "header"))
})

test_that("tplyr_table comes with empty list binded on 'layers'", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)
  expect_equal(tab$layers, structure(list(), class = c("tplyr_layer_container", "list")))
})

### Errors ###
test_that("tplyr_table throws error when passed a bad table argument", {
  expect_error(tplyr_table(matrix(a = 1:10, b = 11:20), a))
  expect_silent(tplyr_table(data.frame(a = 1:10, b = 11:20), a))
})
