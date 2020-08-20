context("layer.R")

## Check empty return ----
test_that("`tplyr_layer` errors when no arguments provided", {
  expect_error(tplyr_layer(),  "The `parent` argument must be provided.")

})

## Check that the object classes returned are appropriate ----
test_that("tplyr_layer returns a class of `tplyr_layer` and environment when `tplyr_table` is parent", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- group_count(t, target_var=Species)
  expect_s3_class(l, "tplyr_layer")
  expect_s3_class(l, "environment")
})

test_that("tplyr_layer returns a class of `tplyr_subgroup_layer`, `tplyr_layer` and environment when `tplyr_layer` is parent", {
  t <- tplyr_table(iris, Sepal.Width)
  l1 <- group_count(t, target_var=Species)
  l2 <- group_count(l1, target_var=Species)
  expect_s3_class(l2, "tplyr_layer")
  expect_s3_class(l2, "tplyr_subgroup_layer")
  expect_s3_class(l2, "environment")
})

test_that("tplyr_layer returns a class of `tplyr_subgroup_layer`, `tplyr_layer` and environment when `tplyr_subgroup_layer` is parent", {
  t <- tplyr_table(iris, Sepal.Width)
  l1 <- group_count(t, target_var=Species)
  l2 <- group_count(l1, target_var=Species)
  l3 <- group_count(l2, target_var=Species)
  expect_s3_class(l3, "tplyr_layer")
  expect_s3_class(l3, "tplyr_subgroup_layer")
  expect_s3_class(l3, "environment")
})

## Environment checks from a proper call ----

test_that("`Type` attribute is set properly", {
  t <- tplyr_table(iris, Sepal.Width)
  l1 <- group_count(t, target_var=Species)
  expect_s3_class(l1, 'count_layer')
  l2 <- group_desc(t, target_var=Sepal.Length)
  expect_s3_class(l2, 'desc_layer')
  l3 <- group_shift(t, target_var=Species)
  expect_s3_class(l3, 'shift_layer')
})

## Error checks ----
test_that("type field can only contain one of 'count', 'desc', or 'shift'", {
  t <- tplyr_table(iris, Sepal.Width)
  # In order to test type parameter have to test direct access to tplyr_layer
  expect_silent(tplyr_layer(t, target_var=quos(Species), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type='count'))
  expect_silent(tplyr_layer(t, target_var=quos(Sepal.Length), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type='desc'))
  expect_silent(tplyr_layer(t, target_var=quos(Species), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type='shift'))
  expect_error(tplyr_layer(t, target_var=quos(Species), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type=c('shift', 'desc')),
                '`type` must be one of "count", "desc", or "shift"')
  expect_error(tplyr_layer(t, target_var=quos(Species), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type=c('count', 'desc')),
               '`type` must be one of "count", "desc", or "shift"')
  expect_error(tplyr_layer(t, target_var=quos(Species), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type=c('count', 'desc', 'shift')),
               '`type` must be one of "count", "desc", or "shift"')
  expect_error(tplyr_layer(t, target_var=quos(Species), by=quos(NULL), cols=quos(NULL), where=quo(TRUE), type="bad"),
               '`type` must be one of "count", "desc", or "shift"')
})

test_that("Parent must be a `tplyr_table`, `tplyr_layer`, or `tplyr_subgroup_layer`", {
  expect_error(group_count(env()), "Must provide `tplyr_table`, `tplyr_layer`, or `tplyr_subgroup_layer` object from the `tplyr` package.")
})

test_that("`by` must me a string, a variable name, or multiple variables submitted using `dplyr::vars`", {
  t <- tplyr_table(iris, Sepal.Width)
  # Safe checks
  expect_silent(group_count(t, target_var=Species, by="character"))
  expect_silent(group_count(t, target_var=Species, by=Petal.Width))
  expect_silent(group_count(t, target_var=Species, by=vars('character', Petal.Width)))
  # Error checks
  err = "Submit either a string, a variable name, or multiple variable names using `dplyr::vars`."
  expect_error(group_count(t, target_var=Species, by=1), err)
  expect_error(group_count(t, target_var=Species, by=list('a', 'b')), err)
  expect_error(group_count(t, target_var=Species, by=c('a', 'b')), err)
  expect_error(group_count(t, target_var=Species, by=vars('character', Petal.Width, 1)), err)
  expect_error(group_count(t, target_var=Species, by=vars('character', Petal.Width, x+y)), err)
})

test_that("`target_var` must me a string, a variable name, or multiple variables submitted using `dplyr::vars`", {
  t <- tplyr_table(iris, Sepal.Width)
  # Safe checks
  expect_silent(group_count(t, target_var=Species))
  expect_silent(group_count(t, target_var=vars(Petal.Width, Petal.Length)))
  # Error checks
  err = "Submit either a string, a variable name, or multiple variable names using `dplyr::vars`."
  expect_error(group_count(t, target_var=1), err)
  expect_error(group_count(t, target_var=list('a', 'b')), err)
  expect_error(group_count(t, target_var=c('a', 'b')), err)
  expect_error(group_count(t, target_var=vars('character', Petal.Width, 1)), err)
  expect_error(group_count(t, target_var=vars('character', Petal.Width, x+y)), err)
})


test_that("`target_var` must exist in target dataset", {
  t <- tplyr_table(iris, Sepal.Width)
  # Variable exists
  expect_silent(group_count(t, target_var=Species))
  # Variable does not
  expect_error(group_count(t, target_var=BadVar), "`target_var` variable `BadVar` does not exist in target dataset")
  expect_error(group_count(t, target_var=vars(Species, BadVar)), "`target_var` variable `BadVar` does not exist in target dataset")
})

test_that("`by` varaibles must exist in the target dataset", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_error(group_count(t, target_var=Species, by=BadVars),
               "`by` variable `BadVars` does not exist in target dataset")
  expect_error(group_count(t, target_var=Species, by=vars(Species, BadVars)),
               "`by` variable `BadVars` does not exist in target dataset")
})

test_that("`where` must be programming logic (quosure of class 'call')", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_silent(group_count(t, target_var=Species, where=a == b))
  expect_error(group_count(t, target_var=Species, where=VARAIBLE),
               "The `where` parameter")
})

## Coded defaults ----

test_that("`layers` defaults to an empty list with a class of `tplyr_layer_container`", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- group_count(t, target_var=Species)
  expect_s3_class(l$layers, 'tplyr_layer_container')
  expect_s3_class(l$layers, 'list')
  expect_equal(length(l$layers), 0)
})

## Environment checks ----
test_that("Parent of layer is appropraitely parent environment", {
  t <- tplyr_table(iris, Sepal.Width)
  l <- group_count(t, target_var=Species)
  expect_true(identical(env_parent(l), t))
})

# There's some nuance here that makes this tricky so leaving the tests out for now.
# Not much practical use currently anyway.
# test_that("Objects submitted through ellipsis argument appear in environment", {
#   t <- tplyr_table(iris, Sepal.Width)
#   dat <- data.frame(var = c(1,2,3))
#   l <- group_count(t, target_var=Species, dat=dat, a=1, z='c')
#   expect_equal(env_get(l, 'dat'), dat)
#   expect_equal(env_get(l, 'a'), 1)
#   expect_equal(env_get(l, 'z'), 'c')
# })

test_that("Desc layers only accept numeric variables", {

  expect_error({tplyr_table(ToothGrowth, dose) %>%
    add_layer(
      group_desc(supp)
    )
  }, regexp = "Target variables must be numeric for desc layers\\.")

  expect_error({tplyr_table(ToothGrowth, dose) %>%
      add_layer(
        group_desc(vars(len, supp))
      )
  }, regexp = "Target variables must be numeric for desc layers\\.")

  expect_error({tplyr_table(ToothGrowth, dose) %>%
      add_layer(
        group_desc(vars(supp, len))
      )
  }, regexp = "Target variables must be numeric for desc layers\\.")

})

