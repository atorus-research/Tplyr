##### treat_var tests #####
test_that("treat_var layer bindings attaches properly", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(layer_treat_var(tab$layers[[1]]), quo(Species))

  set_layer_treat_var(tab$layers[[1]], Species2)
  expect_equal(layer_treat_var(tab$layers[[1]]), Species2)
})

test_that("treat_var errors raise appropriately", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_treat_var(tab$layers[[1]], "Species2"), "treat_var must be a Symbol")
  expect_error(set_layer_treat_var(tab$layers[[1]], filter = Species2), "treat_var must be a variable name")
  expect_silent(set_layer_treat_var(tab$layers[[1]], Species2))
})
##### by tests #####
test_that("tplyr_by binds as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_true(is.null(tplyr_by(tab$layer[[1]])))

  set_tplyr_by(tab$layers[[1]], "aString")
  expect_equal(tplyr_by(tab$layers[[1]]), "aString")

  set_tplyr_by(tab$layers[[1]], Species2)
  expect_equal(tplyr_by(tab$layers[[1]]), quo(Species2))

  set_tplyr_by(tab$layers[[1]], vars(Species2, Sepal.Width))
  expect_equal(tplyr_by(tab$layers[[1]]), vars(Species2, Sepal.Width))
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

  expect_true(is.null(tplyr_where(tab$layers[[1]])))

  set_tplyr_where(tab$layer[[1]], Petal.Length > 3)
  expect_true(tplyr_where(tab$layers[[1]]), quo(Petal.Length > 3))

  set_tplyr_where(tab$layer[[1]], NULL)
  expect_true(is.null(tplyr_where(tab$layers[[1]])))
})

test_that("tplyr_where throws errors as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_tplyr_where(tab$layers[[1]], "aString"), "The `where` parameter must contain subsetting logic")
  expect_error(set_tplyr_where(tab$layers[[1]], Species), "The `where` parameter must contain subsetting logic")
  expect_silent(set_tplyr_where(tab$layers[[1]], quo(Petal.Length > 3)))
})

##### sort_vars tests #####
test_that("sort_vars binds sort_var as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(sort_vars(tab$layers[[1]]), quo(Species))

  set_sort_vars(tab$layers[[1]], c("Sepal.Length", "Sepal.Width"))
  expect_equal(sort_vars(tab$layers[[1]]), c("Sepal.Length", "Sepal.Width"))

  set_sort_vars(tab$layers[[1]], NA_character_)
  expect_equal(sort_vars(tab$layers[[1]]), NA_character_)

  set_sort_vars(tab$layers[[1]], Species2)
  expect_equal(tab$layers[[1]], quo(Species2))
})

test_that("sort_vars throws expected errors", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_sort_vars(tab$layer[[1]], c(1, 2)), "sort_vars must be a character vector or variable name")
  expect_error(set_sort_vars(tab$layer[[1]], Petal.Width == 1), "sort_vars must be a character vector or variable name")
  expect_silent(set_sort_vars(tab$layers[[1]], NA_character_))
})

##### sort tests #####
test_that("sort sets bindings as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(layer_sort(tab$layers[[1]]), "asc")

  set_layer_sort(tab$layers[[1]], "desc")
  expect_equal(layer_sort(tab$layers[[1]]), "desc")
})

test_that("sort_throws errors as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_sort(tab$layer[[1]], "other"), "sort must be 'asc', 'desc'")
})

##### formetter tests #####
test_that("formatter layer sets bindings as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_true(is.null(layer_formatter(tab$layer[[1]])))

  set_layer_formatter(tab$layer[[1]], is.character)
  expect_equal(layer_formatter(tab$layer[[1]]), is.character)

  set_layer_formatter(tab$layer[[1]], function(x) "abc")
  expect_equal(layer_formatter(tab$layer[[1]]), function(x) "abc")
})

test_that("formatter raises errors as expected", {
  iris_a <- iris
  iris_a$Species2 <- iris_a$Species2
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_error(set_layer_formatter(tab$layer[[1]], "string"), "formatter must be a function")
})
