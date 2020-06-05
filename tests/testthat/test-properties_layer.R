##### treat_var tests #####
test_that("target_var layer bindings attaches properly", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(target_var(tab), quo(Species))

  set_target_var(tab, Species2)
  expect_equal(target_var(tab), quo(Species2))
})

test_that("target_var errors raise appropriately", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_target_var(tab, "Species2"), "target_var must be a variable name")
  expect_error(set_target_var(tab, quo(filter = Species2)), "target_var must be a variable name")
  expect_silent(set_target_var(tab, Species2))
})
##### by tests #####
test_that("tplyr_by binds as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_true(quo_is_null(tplyr_by(tab)[[1]]))

  set_tplyr_by(tab, "aString")
  expect_equal(tplyr_by(tab)[[1]], quo("aString"))

  set_tplyr_by(tab, Species2)
  expect_equal(tplyr_by(tab), quos(Species2))

  set_tplyr_by(tab, vars(Species2, Sepal.Width))
  expect_equal(tplyr_by(tab), vars(Species2, Sepal.Width))
})

test_that("tplyr_by raises expected errors", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(stop("I'm having trouble figuring out how to test these errors."))
})

##### where tests #####
test_that("tplyr_where binds where as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_true(quo_is_null(tplyr_where(tab)))

  set_tplyr_where(tab, Petal.Length > 3)
  expect_equal(tplyr_where(tab), quo(Petal.Length > 3))

  set_tplyr_where(tab, NULL)
  expect_true(quo_is_null(tplyr_where(tab)))
})

test_that("tplyr_where throws errors as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_tplyr_where(tab, "aString"), "The `where` parameter must contain subsetting logic")
  expect_error(set_tplyr_where(tab, Species), "The `where` parameter must contain subsetting logic")
  expect_silent(set_tplyr_where(tab, quo(Petal.Length > 3)))
})

##### sort_vars tests #####
test_that("sort_vars binds sort_var as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(sort_vars(tab), quo(Species))

  set_sort_vars(tab, c("Sepal.Length", "Sepal.Width"))

  expect_equal(sort_vars(tab), c("Sepal.Length", "Sepal.Width"))

  set_sort_vars(tab, NA_character_)
  expect_equal(sort_vars(tab), NA_character_)
})

test_that("sort_vars throws expected errors", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_sort_vars(tab, c(1, 2)), "sort_vars must be a character vector")
})

##### sort tests #####
test_that("sort sets bindings as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(layer_sort(tab), "ascending")

  set_layer_sort(tab, "desc")
  expect_equal(layer_sort(tab), "desc")
})

test_that("sort_throws errors as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_sort(tab$layer[[1]], "other"), "sort must be 'ascending', 'desc'")
})

##### formetter tests #####
test_that("formatter layer sets bindings as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(layer_formatter(tab), as.character)

  set_layer_formatter(tab, is.character)
  expect_equal(layer_formatter(tab), is.character)

  set_layer_formatter(tab, function(x) "abc")
  expect_equal(layer_formatter(tab), function(x) "abc")
})

test_that("formatter raises errors as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_formatter(tab, "string"), "formatter must be a function")
})
