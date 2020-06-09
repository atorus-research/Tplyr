iris_b <- iris
iris_b$Species <- as.character(iris_b$Species)
tab <- tplyr_table(iris_b, Species) %>%
  add_layer(
    group_count(Species)
  ) %>%
  add_total_group()

test_that("build.tplyr_table preprocesses data appropriately", {
  expect_named(tab, c("layer_output", "target", "pop_data", "header", "treat_var", "header_n", "pop_treat_var",
                      "layers",  "treat_grps"))
  expect_true(all(tab$target$`.tplyr-Total`))
})
