context("layering.R")

tplyr_debug(FALSE)

## group_<tpye> family of functions ----

test_that("`group_<type>` functions output layers of appropriate type", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_s3_class(group_count(t, target_var=Species), 'count_layer')
  expect_s3_class(group_desc(t, target_var=Species), 'desc_layer')
  expect_s3_class(group_shift(t, target_var=Species), 'shift_layer')
})

test_that("`group_<type>` function pass parameters through appropriately", {
  t <- tplyr_table(iris, Sepal.Width)
  # Define layers with parameter
  l1 <- group_count(t, target_var=Species, by=Sepal.Width, where=Species == 'something')
  l2 <- group_desc(t, target_var=Species, by=Sepal.Width, where=Species == 'something')
  l3 <- group_shift(t, target_var=Species, by=Sepal.Width, where=Species == 'something')

  # Check values of `by`
  expect_equal(as_label(quo_get_expr(l1$by[[1]])), "Sepal.Width")
  expect_equal(as_label(quo_get_expr(l2$by[[1]])), "Sepal.Width")
  expect_equal(as_label(quo_get_expr(l3$by[[1]])), "Sepal.Width")

  # Check values of `where`
  expect_equal(as_label(quo_get_expr(l1$where)), "Species == \"something\"")
  expect_equal(as_label(quo_get_expr(l2$where)), "Species == \"something\"")
  expect_equal(as_label(quo_get_expr(l3$where)), "Species == \"something\"")
})

## `add_layer` error testing
test_that("All parameters must be provided", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_error(add_layer())
  expect_error(add_layer(t))
  expect_silent(add_layer(t, group_desc(target_var=Species)))
})

test_that("Parent argument is a valid class", {
  expect_error(add_layer(iris, group_desc(target_var=Species)))
})

test_that("Only `Tplyr` methods are allowed in the `layer` parameter", {
  expect_silent({
    t <- tplyr_table(iris, Sepal.Width) %>%
      add_layer(
        group_desc(target_var=Species)
      )
  })

  expect_error({
    t <- tplyr_table(iris, Sepal.Width) %>%
      add_layer(
        group_desc(target_var=Species) %>%
        print()
      )
  })
})

## `add_layer` functionality testing
test_that("`add_layer` attaches layer object into parent", {
  t <- tplyr_table(iris, Sepal.Width) %>%
    add_layer(
      group_desc(target_var=Species)
    )

  expect_true(length(t$layers) == 1)
  expect_s3_class(t$layers[[1]], 'tplyr_layer')
  expect_true(as_label(t$layers[[1]]$target_var) == "Species")
})


test_that("Sequences of add_layer append appropriately into parent", {
  t <- tplyr_table(iris, Sepal.Width) %>%
    add_layer(
      # Adding index to check that order is appropriate.
      # See test-layer.R tests to see that ellipsis arguments are funneled into layer environment as bindings
      group_desc(target_var=Species, index=1)
    ) %>%
    add_layer(
      group_desc(target_var=Species, index=2)
    )

    expect_true(length(t$layers) == 2)
    expect_true(t$layers[[1]]$index == 1)
    expect_true(t$layers[[2]]$index == 2)
})

test_that("Using `add_layer` within `add_layer` adds child layers into a layer object", {
  # Make a layer with a subgroup
  t <- tplyr_table(iris, Sepal.Width) %>%
    add_layer(
      group_desc(target_var=Species) %>%
        add_layer(
          group_desc(target_var=Species)
        )
    )

  # Extract the parent layer, and the child layer from the parent
  parent_layer <- t$layers[[1]]
  child_layer <- parent_layer$layers[[1]]

  expect_equal(env_parent(parent_layer), t)
  expect_s3_class(parent_layer, 'tplyr_layer')
  expect_equal(env_parent(child_layer), parent_layer)
  expect_s3_class(child_layer, 'tplyr_subgroup_layer')
  expect_s3_class(child_layer, 'tplyr_layer')
})




