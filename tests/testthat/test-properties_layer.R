##### treat_var tests #####
iris_a <- iris
iris_a['Species2'] <- iris_a$Species

test_that("target_var layer bindings attaches properly", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(unname(map_chr(get_target_var(tab), as_name)), "Species")

  set_target_var(tab, Species2)
  expect_equal(get_target_var(tab), quos(Species2))
})

test_that("target_var errors raise appropriately", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_snapshot_error(set_target_var(tab, "Species2"))
  expect_snapshot_error(set_target_var(tab, quos(filter = Species2)))
  expect_silent(set_target_var(tab, Species2))
})
##### by tests #####
test_that("by binds as expected", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_equal(get_by(tab), quos())

  set_by(tab, "aString")
  expect_equal(get_by(tab)[[1]], quo("aString"))

  set_by(tab, Species2)
  expect_equal(get_by(tab), quos(Species2))

  set_by(tab, vars(Species2, Sepal.Width))
  expect_equal(unname(map_chr(get_by(tab), as_name)), c("Species2", "Sepal.Width"))
})

test_that("by raises expected errors", {
  tab <- tplyr_table(iris_a, Species) %>%
    group_count(Species)

  expect_snapshot_error(set_by(tab, list(Species)))
  expect_snapshot_error(set_by(tab, vars(Species, list())))
  expect_snapshot_error(set_by(tab, vars(Species, 2)))
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

  expect_snapshot_error(set_where(tab, "aString"))
  expect_snapshot_error(set_where(tab, Species))
  expect_silent(set_where(tab, quo(Petal.Length > 3)))
})

