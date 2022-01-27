test_that("tplyr_table returns an empty envrionment of class 'tplyr_table' when passed no arguemnts", {
  st <- tplyr_table()
  expect_true(is.environment(st))
  expect_equal(length(rlang::env_names(st)), 0)
})

test_that("tplyr_table returns a class of tplyr_table and environment", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)
  expect_s3_class(tab, "tplyr_table")
  expect_s3_class(tab, "environment")
  expect_setequal(env_names(tab), c("shift_layer_formats", "target", "count_layer_formats", "pop_data", "pop_where", "cols",
                                    "table_where", "desc_layer_formats", "treat_var", "pop_treat_var",
                                    "layers", "treat_grps"))

})

test_that("tplyr_table comes with empty list binded on 'layers'", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)
  expect_equal(tab$layers, structure(list(), class = c("tplyr_layer_container", "list")))
})

### Errors ###
test_that("tplyr_table throws error when passed a bad table argument", {
  expect_snapshot_error(tplyr_table(matrix(a = 1:10, b = 11:20), a))
  expect_silent(tplyr_table(data.frame(a = 1:10, b = 11:20), a))
})

test_that("Table level where clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear, where = bad == code) %>%
    add_layer(
      group_desc(drat)
    )

  expect_snapshot_error(build(t))
})

test_that("Population data where clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear) %>%
    set_pop_data(mtcars) %>%
    set_pop_where(bad == code) %>%
    add_layer(
      group_desc(drat)
    )

  expect_snapshot_error(build(t))
})
