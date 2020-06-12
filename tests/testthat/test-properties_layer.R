##### treat_var tests #####
iris_a <- iris
iris_a['Species2'] <- iris_a$Species

test_that("target_var layer bindings attaches properly", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(get_target_var(tab), quos(Species))

  set_target_var(tab, Species2)
  expect_equal(get_target_var(tab), quos(Species2))
})

test_that("target_var errors raise appropriately", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_target_var(tab, "Species2"), "Invalid input to `target_var`")
  expect_error(set_target_var(tab, quos(filter = Species2)), "Invalid input to `target_var`")
  expect_silent(set_target_var(tab, Species2))
})
##### by tests #####
test_that("by binds as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_true(quo_is_null(get_by(tab)[[1]]))

  set_by(tab, "aString")
  expect_equal(get_by(tab)[[1]], quo("aString"))

  set_by(tab, Species2)
  expect_equal(get_by(tab), quos(Species2))

  set_by(tab, vars(Species2, Sepal.Width))
  expect_equal(get_by(tab), vars(Species2, Sepal.Width))
})

test_that("by raises expected errors", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  msg = "Invalid input to `by`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`."
  expect_error(set_by(tab, list(Species)), msg)
  expect_error(set_by(tab, vars(Species, list())), msg)
  expect_error(set_by(tab, vars(Species, 2)), msg)
})

##### where tests #####
test_that("where binds where as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_true(quo_get_expr(get_where(tab)))

  set_where(tab, Petal.Length > 3)
  expect_equal(get_where(tab), quo(Petal.Length > 3))
})

test_that("where throws errors as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_where(tab, "aString"), "The `where` parameter must contain subsetting logic")
  expect_error(set_where(tab, Species), "The `where` parameter must contain subsetting logic")
  expect_silent(set_where(tab, quo(Petal.Length > 3)))
})

##### sort_vars tests #####
test_that("sort_vars binds sort_var as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(get_sort_vars(tab)[[1]], quo(Species))

  set_sort_vars(tab, Species2)

  expect_equal(get_sort_vars(tab), quos(Species2))

  set_sort_vars(tab, vars(Species, Species2))
  expect_equal(get_sort_vars(tab), quos(Species, Species2))
})

test_that("sort_vars throws expected errors", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_sort_vars(tab, c(1, 2)), "Invalid input to `sort_vars`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`.")
})

##### sort tests #####
test_that("sort sets bindings as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(get_layer_sort(tab), "ascending")

  set_layer_sort(tab, "desc")
  expect_equal(get_layer_sort(tab), "desc")
})

test_that("sort_throws errors as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_sort(tab$layer[[1]], "other"), "sort must be 'ascending', 'desc'")
})

##### formetter tests #####
test_that("formatter layer sets bindings as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(get_layer_formatter(tab), as.character)

  set_layer_formatter(tab, is.character)
  expect_equal(get_layer_formatter(tab), is.character)

  set_layer_formatter(tab, function(x) "abc")
  expect_equal(get_layer_formatter(tab), function(x) "abc")
})

test_that("formatter raises errors as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_formatter(tab, "string"), "formatter must be a function")
})
