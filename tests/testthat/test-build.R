
tab <- tplyr_table(iris, Species) %>%
  add_layer(
    group_count(Petal.Width)
  ) %>%
  add_total_group() %>%
  add_treat_grps("V Species" = c("versicolor", "virginica"))
build(tab)

test_that("build.tplyr_table preprocesses data appropriately", {
  expect_named(tab, c("shift_layer_formats", "target", "count_layer_formats", "pop_data", "pop_where", "cols",
  "max_n_width", "table_where", "desc_layer_formats", "max_layer_length", "treat_var",
  "built_target", "built_pop_data", "header_n", "pop_treat_var", "layers", "treat_grps"))
  expect_equal(nrow(tab$built_target[tab$built_target$Species == "Total", ]), 150)
  expect_equal(nrow(tab$built_target[tab$built_target$Species == "V Species", ]), 100)
})
