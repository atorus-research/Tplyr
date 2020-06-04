context("layer.R")

tplyr_debug(FALSE)

## Check empty return ----
test_that("`tplyr_layer` errors when no arguments provided", {
  expect_error(tplyr_layer(),  "The `parent` argument must be provided.")

})

## Check that the object classes returned are appropriate ----
test_that("tplyr_layer returns a class of `tplyr_layer` and environment when `tplyr_table` is parent", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_s3_class(l, "tplyr_layer")
  expect_s3_class(l, "environment")
})

test_that("tplyr_layer returns a class of `tplyr_subgroup_layer`, `tplyr_layer` and environment when `tplyr_layer` is parent", {
  t <- tplyr_table(iris, Sepal.Width)
  l1 <- tplyr_layer(t, target_var=Species, type='count')
  l2 <- tplyr_layer(l1, target_var=Species, type='count')
  expect_s3_class(l2, "tplyr_layer")
  expect_s3_class(l2, "tplyr_subgroup_layer")
  expect_s3_class(l2, "environment")
})

test_that("tplyr_layer returns a class of `tplyr_subgroup_layer`, `tplyr_layer` and environment when `tplyr_subgroup_layer` is parent", {
  t <- tplyr_table(iris, Sepal.Width)
  l1 <- tplyr_layer(t, target_var=Species, type='count')
  l2 <- tplyr_layer(l1, target_var=Species, type='count')
  l3 <- tplyr_layer(l2, target_var=Species, type='count')
  expect_s3_class(l3, "tplyr_layer")
  expect_s3_class(l3, "tplyr_subgroup_layer")
  expect_s3_class(l3, "environment")
})

## Environment checks from a proper call ----

test_that("Environment contains proper bindings when call is proper", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_equal(sort(env_names(l)), c("by", "formatter", "layers", "sort", "sort_var", "target_var", "where"))
})

test_that("`Type` attribute is set properly", {
  t <- tplyr_table(iris, Sepal.Width)
  l1 <- tplyr_layer(t, target_var=Species, type='count')
  expect_s3_class(l1, 'count_layer')
  l2 <- tplyr_layer(t, target_var=Species, type='desc')
  expect_s3_class(l2, 'desc_layer')
  l3 <- tplyr_layer(t, target_var=Species, type='shift')
  expect_s3_class(l3, 'shift_layer')
})

## Error checks ----
test_that("type field can only contain one of 'count', 'desc', or 'shift'", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_silent(tplyr_layer(t, target_var=Species, type='count'))
  expect_silent(tplyr_layer(t, target_var=Species, type='desc'))
  expect_silent(tplyr_layer(t, target_var=Species, type='shift'))
  expect_error(tplyr_layer(t, target_var=Species, type=c('shift', 'desc')),
                '`type` must be one of "count", "desc", or "shift"')
  expect_error(tplyr_layer(t, target_var=Species, type=c('count', 'desc')),
               '`type` must be one of "count", "desc", or "shift"')
  expect_error(tplyr_layer(t, target_var=Species, type=c('count', 'desc', 'shift')),
               '`type` must be one of "count", "desc", or "shift"')
  expect_error(tplyr_layer(t, target_var=Species, type="bad"),
               '`type` must be one of "count", "desc", or "shift"')
})

test_that("Parent must be a `tplyr_table`, `tplyr_layer`, or `tplyr_subgroup_layer`", {
  expect_error(tplyr_layer(env(), type='count'))
})

test_that("`by` must me a string, a variable name, or multiple variables submitted using `dplyr::vars`", {
  t <- tplyr_table(iris, Sepal.Width)
  # Safe checks
  expect_silent(tplyr_layer(t, target_var=Species, type='count', by="character"))
  expect_silent(tplyr_layer(t, target_var=Species, type='count', by=Petal.Width))
  expect_silent(tplyr_layer(t, target_var=Species, type='count', by=vars('character', Petal.Width)))
  # Error checks
  err = "Invalid input to `by`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`."
  expect_error(tplyr_layer(t, target_var=Species, type='count', by=1), err)
  expect_error(tplyr_layer(t, target_var=Species, type='count', by=list('a', 'b')), err)
  expect_error(tplyr_layer(t, target_var=Species, type='count', by=c('a', 'b')), err)
  expect_error(tplyr_layer(t, target_var=Species, type='count', by=vars('character', Petal.Width, 1)), err)
  expect_error(tplyr_layer(t, target_var=Species, type='count', by=vars('character', Petal.Width, x+y)),
               "Arguments to `by` must")
})

test_that("`target_var` must be a variable name", {
  t <- tplyr_table(iris, Sepal.Width)
  # Variable exists
  expect_error(tplyr_layer(t, target_var="Species", type='count'))
})


test_that("`target_var` must exist in target dataset", {
  t <- tplyr_table(iris, Sepal.Width)
  # Variable exists
  expect_silent(tplyr_layer(t, target_var=Species, type='count'))
  # Variable does not
  expect_error(tplyr_layer(t, target_var=BadVar, type='count'), "`target_var` value BadVar does not exist in target data frame.")
})

test_that("`by` varaibles must exist in the target dataset", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_error(tplyr_layer(t, target_var=Species, by=BadVars, type='count'),
               "By variable `BadVars` does not exist in target dataset")
  expect_error(tplyr_layer(t, target_var=Species, by=vars(Species, BadVars), type='count'),
               "By variable `BadVars` does not exist in target dataset")
})

test_that("`where` must be programming logic (quosure of class 'call')", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_silent(tplyr_layer(t, target_var=Species, type="count", where=a == b))
  expect_error(tplyr_layer(t, target_var=Species, type="count", where=VARAIBLE),
               "The `where` parameter")
})

## Coded defaults ----
test_that("`sort` defaults to 'ascending'", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_equal(l$sort, 'ascending')
})

test_that("`sort_var` defaults to `target_var`", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_true(identical(l$sort_var, l$target_var))
})

test_that("`formatter` defaults to `as.character`", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_true(identical(l$formatter, as.character))
})

test_that("`layers` defaults to an empty list with a class of `tplyr_layer_container`", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_s3_class(l$layers, 'tplyr_layer_container')
  expect_s3_class(l$layers, 'list')
  expect_equal(length(l$layers), 0)
})

## Environment checks ----
test_that("Parent of layer is appropraitely parent environment", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- tplyr_layer(t, target_var=Species, type='count')
  expect_true(identical(env_parent(l), t))
})

test_that("Objects submitted through ellipsis argument appear in environment", {
  t <- tplyr_table(iris, Sepal.Width)
  dat <- data.frame(var = c(1,2,3))
  l <- tplyr_layer(t, target_var=Species, type='count', dat=dat, a=1, z='c')
  expect_equal(env_get(l, 'dat'), dat)
  expect_equal(env_get(l, 'a'), 1)
  expect_equal(env_get(l, 'z'), 'c')
})


