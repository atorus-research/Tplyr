# Desc Layer
mtcars_long <- mtcars %>%
  rownames_to_column(var = "model") %>%
  pivot_longer(cols = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'))

t1 <- tplyr_table(mtcars, gear)
t2 <- tplyr_table(mtcars, gear)
t3 <- tplyr_table(mtcars, gear)
t4 <- tplyr_table(mtcars, gear)
t5 <- tplyr_table(mtcars, gear)
t6 <- tplyr_table(mtcars, gear)
t7 <- tplyr_table(mtcars, gear, cols=vs)
t8 <- tplyr_table(mtcars, gear, cols=vs)

d1 <- group_desc(t1, mpg)
d2 <- group_desc(t2, mpg, by = am)
d3 <- group_desc(t3, mpg, by = vars(am, vs))
d4 <- group_desc(t4, mpg) %>%
  set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
  set_format_strings(
    "Mean Squared" = f_str("xx.xx", mean_squared)
  )
d5 <- group_desc(t5, vars(mpg, wt))
# Update for custom summaries - two target variables
d6 <- group_desc(t6, vars(mpg, wt)) %>%
  set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
  set_format_strings(
    "Mean Squared" = f_str("xx.xx", mean_squared)
  )
d7 <- group_desc(t7, mpg)
d8 <- group_desc(t7, mpg, by = carb)

t1 <- add_layers(t1, d1)
t2 <- add_layers(t2, d2)
t3 <- add_layers(t3, d3)
t4 <- add_layers(t4, d4)
t5 <- add_layers(t5, d5)
t6 <- add_layers(t6, d6)
t7 <- add_layers(t7, d7)
t8 <- add_layers(t8, d8)

test_that("Group_desc can be created without warnings and errors", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
  expect_silent(build(t6))
  expect_silent(build(t7))
  expect_silent(build(t8))
})

test_that("Auto precision builds correctly", {

  t_uncap <- tplyr_table(mtcars_long, gear) %>%
    add_layer(
      group_desc(value, by = name) %>%
        set_format_strings(
          'n' = f_str('xxx', n),
          'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
          'Variance' = f_str('a.a+2', var),
          'Q1, Median, Q3' = f_str('a.a, a.a, a.a', q1, median, q3),
          'IQR' = f_str('a', iqr),
          'Missing' = f_str('xxx', missing)
        )
    ) %>%
    build()

  t_cap <- tplyr_table(mtcars_long, gear) %>%
    add_layer(
      group_desc(value, by = name) %>%
        set_format_strings(
          'n' = f_str('xxx', n),
          'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
          'Variance' = f_str('a.a+2', var),
          'Q1, Median, Q3' = f_str('a.a, a.a, a.a', q1, median, q3),
          'IQR' = f_str('a', iqr),
          'Missing' = f_str('xxx', missing),
          cap = c('int'=3, 'dec'=2)
        )
    ) %>%
    build()

  load(test_path("t_uncap_comp.Rdata"))
  load(test_path("t_cap_comp.Rdata"))

  expect_equal(mutate_all(t_uncap, as.character),
               mutate_all(t_uncap_comp, as.character), ignore_attr = TRUE)
  expect_equal(mutate_all(t_cap, as.character),
               mutate_all(t_cap_comp, as.character), ignore_attr = TRUE)

})

test_that("Desc layer clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat, where=bad == code)
    )

  expect_snapshot_error(build(t))
})

test_that("Stats as columns properly transposes the built data", {

  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(vars(wt, drat)) %>%
        set_format_strings(
          "n"        = f_str("xx", n),
          "sd"       = f_str("xx.x", sd, empty = c(.overall = "BLAH"))
        ) %>%
        set_stats_as_columns()
    )

  expect_silent(build(t1))

  d1 <- build(t1)

  # Make sure the names are as expected
  t1_exp_names <- c("row_label1", "var1_n", "var1_sd", "var2_n", "var2_sd", "ord_layer_index", "ord_layer_1")
  expect_equal(names(d1), t1_exp_names)
  expect_snapshot_output(d1)

  # Check that cols evaluate properly as well
  t2 <- tplyr_table(mtcars, gear, cols=am) %>%
    add_layer(
      group_desc(vars(wt, drat)) %>%
        set_format_strings(
          "n"        = f_str("xx", n),
          "sd"       = f_str("xx.x", sd, empty = c(.overall = "BLAH"))
        ) %>%
        set_stats_as_columns()
    )

  expect_silent(d2 <- build(t2))

  t2_exp_names <- c('row_label1', 'var1_n_0', 'var1_sd_0', 'var1_n_1', 'var1_sd_1', 'var2_n_0',
                    'var2_sd_0', 'var2_n_1', 'var2_sd_1', 'ord_layer_index', 'ord_layer_1')

  expect_equal(names(d2), t2_exp_names)
  expect_snapshot(as.data.frame(d2))

})

test_that("Infinites aren't produced from min/max", {
  dat <- tibble::tribble(
    ~x1, ~x2,
    'a',   1,
    'a',   2,
    'b',  NA,
  )

  t <- tplyr_table(dat, x1) %>%
    add_layer(
      group_desc(x2) %>%
        set_format_strings(
          "Min, Max" = f_str("xx, xx", min, max)
        )
    )

  x <- suppressWarnings(build(t))

  expect_equal(x$var1_b, "")
})
