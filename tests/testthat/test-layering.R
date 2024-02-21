
## group_<type> family of functions ----

test_that("`group_<type>` functions output layers of appropriate type", {
  t <- tplyr_table(iris, Sepal.Width)
  expect_s3_class(group_count(t, target_var=Species), 'count_layer')
  expect_s3_class(group_desc(t, target_var=Sepal.Length), 'desc_layer')
  expect_s3_class(group_shift(t, target_var=Species), 'shift_layer')
})

test_that("`group_<type>` function pass parameters through appropriately", {
  t <- tplyr_table(iris, Sepal.Width)
  # Define layers with parameter
  l1 <- group_count(t, target_var=Species, by=Sepal.Width, where=Species == 'something')
  l2 <- group_desc(t, target_var=Sepal.Length, by=Sepal.Width, where=Species == 'something')
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
  expect_snapshot_error(add_layer())
  expect_snapshot_error(add_layer(t))
  expect_silent(add_layer(t, group_desc(target_var=Sepal.Length)))
})

test_that("Parent argument is a valid class (pass through to `tplyr_layer`)", {
  expect_snapshot_error(add_layer(iris, group_desc(target_var=Sepal.Length)))
})

test_that("Only `Tplyr` methods are allowed in the `layer` parameter", {
  expect_silent({
    t <- tplyr_table(iris, Sepal.Width) %>%
      add_layer(
        group_desc(target_var=Sepal.Length)
      )
  })

  expect_snapshot_error({
    t <- tplyr_table(iris, Sepal.Width) %>%
      add_layer(
        group_desc(target_var=Sepal.Length) %>%
        print()
      )
  })
})

## `add_layer` functionality testing
test_that("`add_layer` attaches layer object into parent", {
  t <- tplyr_table(iris, Sepal.Width) %>%
    add_layer(
      group_desc(target_var=Sepal.Length)
    )

  expect_true(length(t$layers) == 1)
  expect_s3_class(t$layers[[1]], 'tplyr_layer')
  expect_equal(unname(map_chr(t$layers[[1]]$target_var, as_name)), "Sepal.Length")
})

test_that("Using `add_layer` within `add_layer` adds child layers into a layer object", {
  # Make a layer with a subgroup
  t <- tplyr_table(iris, Sepal.Width) %>%
    add_layer(
      group_desc(target_var=Sepal.Length) %>%
        add_layer(
          group_desc(target_var=Sepal.Length)
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

test_that("Layers accept names when specified", {

  # Using add_layer - multiple call types
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(name = "Test",
              group_desc(drat)
    )

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat) %>%
        set_format_strings('n'=f_str('a', n)),
      name="Test"
    )

  t3 <- t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      layer = group_desc(drat) %>%
        set_format_strings('n'=f_str('a', n)),
      name="Test"
    )

  expect_equal(names(t1$layers), 'Test')
  expect_equal(names(t2$layers), 'Test')
  expect_equal(names(t3$layers), 'Test')


  # Using add_layers
  t <- tplyr_table(mtcars, gear)
  l <- group_desc(t, drat)
  t <- add_layers(t, 'Test' = l)
  expect_equal(names(t$layers), "Test")

})

test_that("add_layer can see calling environment objects", {
  tfunc <- function(){

    prec <- tibble::tribble(
      ~vs, ~max_int, ~max_dec,
      0,        1,        1,
      1,        2,        2
    )

    tplyr_table(mtcars, gear) %>%
      add_layer(
        group_desc(wt, by = vs) %>%
          set_precision_data(prec)
      )
  }

  expect_silent(tfunc())
})

