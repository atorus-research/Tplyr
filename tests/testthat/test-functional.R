

##### T1 Simple count layer #####
t1 <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(carb)
  )

##### T2 Simple count layer w/table options #####
t2 <- tplyr_table(mtcars, gear, cols = am) %>%
  set_where(vs == 1) %>%
  add_layer(
    group_count(carb)
  )

##### T3 Simple desc layer #####
t3 <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(mpg) %>%
      set_format_strings(
        "n"        = f_str("xx", n),
        "Mean (SD)"= f_str("xx.x", mean),
        "SD" = f_str("xx.xx", sd),
        "Median"   = f_str("xx.x", median),
        "Q1, Q3"   = f_str("xx, xx", q1, q3),
        "Min, Max" = f_str("xx, xx", min, max),
        "Missing"  = f_str("xx", missing)
      )
  )

##### T4 Complex desc layer #####
t4 <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(mpg, by = vars("am", am)) %>%
      set_format_strings(
        "n"        = f_str("xx", n),
        "Mean (SD)"= f_str("xx.x", mean),
        "SD" = f_str("xx.xx", sd),
        "Median"   = f_str("xx.x", median),
        "Q1, Q3"   = f_str("xx, xx", q1, q3),
        "Min, Max" = f_str("xx, xx", min, max),
        "Missing"  = f_str("xx", missing)
      )
  )

  ##### T5 Complex table, cols, where, groups #####
t5 <- tplyr_table(mtcars, gear, cols = vs) %>%
  set_where(mpg > 15) %>%
  add_total_group() %>%
  add_treat_grps("Cyls 8" = c(8))
t5 <- add_layers(t5,
                 group_count(t5, carb, by = am),
                 group_count(t5, "All", by = am) %>%
                   set_distinct_by(carb) %>%
                   set_format_strings(f_str("xxx", n)),
                 group_desc(t5, mpg) %>%
                   set_format_strings(
                     "n"        = f_str("xx", n),
                     "Mean (SD)"= f_str("xx.x", mean),
                     "SD" = f_str("xx.xx", sd),
                     "Median"   = f_str("xx.x", median),
                     "Q1, Q3"   = f_str("xx, xx", q1, q3),
                     "Min, Max" = f_str("xx, xx", min, max),
                     "Missing"  = f_str("xx", missing)
                   )
)

##### T6 Simple table complex count layer #####
t6 <- tplyr_table(mtcars, gear, col = am) %>%
  add_total_group() %>%
  set_where(mpg > 15) %>%
  add_layer(
    group_count(carb, by = vars("Carb count", vs))
  ) %>%
  add_layer(
    group_count(cyl, by = vars(vs, carb)) %>%
      set_format_strings(f_str("xxx", n))
  ) %>%
  add_layer(
    group_count("All VS") %>%
      set_distinct_by(vs)
  ) %>%
  add_layer(
    group_desc(mpg) %>%
      set_format_strings(
        "n"        = f_str("xx", n),
        "Mean (SD)"= f_str("xx.x", mean),
        "SD" = f_str("xx.xx", sd),
        "Median"   = f_str("xx.x", median),
        "Q1, Q3"   = f_str("xx, xx", q1, q3),
        "Min, Max" = f_str("xx, xx", min, max),
        "Missing"  = f_str("xx", missing)
      )
  )

##### T7 Table with pop_data, distinct_by, and treatment groups #####
mtcars2 <- mtcars
mtcars2$col_i <- rep(c("a", "b", "c", "d"), 8)
t7 <- tplyr_table(mtcars2, gear, col = col_i) %>%
  set_pop_data(mtcars) %>%
  add_total_group() %>%
  add_layer(
    group_count(cyl) %>%
      set_distinct_by(vs)
  )

##### Tests #####

test_that("all test tables can be built without errors or warnings", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
  expect_silent(suppressWarnings(build(t6))) # This seems to be a bug https://github.com/tidyverse/dplyr/issues/5149
  expect_snapshot_error(build(t7))
})

test_that("all tables have the expected dimentions", {
  b_t1 <- build(t1)
  b_t2 <- build(t2)
  b_t3 <- build(t3)
  b_t4 <- build(t4)
  b_t5 <- build(t5)
  b_t6 <- suppressWarnings(build(t6))

  expect_equal(dim(b_t1), c(6, 6))
  expect_equal(dim(b_t2), c(3, 9))
  expect_equal(dim(b_t3), c(7, 6))
  expect_equal(dim(b_t4), c(14, 10))
  expect_equal(dim(b_t5), c(19, 13))
  expect_equal(dim(b_t6), c(48, 15))
})

## Table with extra treat var factor
test_that("Extra factors on the treat var come through on the table", {
  mtcars2 <- mtcars
  mtcars2$gear <- factor(mtcars2$gear, c(3, 4, 5, 10))

  t1 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl)
    ) %>%
    add_layer(
      group_desc(mpg)
    ) %>%
    build()

  expect_equal(dim(t1), c(9,7))
})

test_that("Treatment groups and columns persist even when taken out by where logic", {
  mtcars2 <- mtcars
  mtcars2$gear <- factor(mtcars2$gear, c(3, 4, 5, 10))
  mtcars2$am <- factor(mtcars2$am, c(0, 1, 2))

  t1 <- tplyr_table(mtcars2, gear, cols = am, where = (gear != 4 & am != 1)) %>%
    add_layer(group_count(cyl)) %>%
    build()

  expect_equal(dim(t1), c(3, 15))

})
