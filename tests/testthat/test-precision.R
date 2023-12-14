
mtcars_long <- mtcars %>%
  rownames_to_column(var = "model") %>%
  pivot_longer(cols = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'))

# Tplyr:::make_prec_data(mtcars_long, quos(name), quo(value), cap=c('int'=99, 'dec'=99)) %>%
#   arrange(name)

test_that('Precision data calculates correctly', {
  # No by
  prec0 <- Tplyr:::make_prec_data(mtcars_long,
                                  quos(),
                                  quo(value),
                                  cap=c('int'=99, 'dec'=99)
  ) %>%
  as.data.frame()

  comp0 <- data.frame(max_int = c(3),
                      max_dec = c(3),
                      precision_on = 'value',
                      stringsAsFactors = FALSE)

  expect_equal(prec0, comp0)


  # One by
  prec1 <- Tplyr:::make_prec_data(mtcars_long,
                                  quos(name),
                                  quo(value),
                                  cap=c('int'=99, 'dec'=99)
                                  ) %>%
    arrange(name) %>%
    as.data.frame()

  comp1 <- data.frame(name =
                        c('cyl', 'disp', 'drat', 'hp', 'mpg', 'qsec', 'wt'),
                      max_int = c(1, 3, 1, 3, 2, 2, 1),
                      max_dec = c(0, 1, 2, 0, 1, 2, 3),
                      precision_on = rep('value', 7),
                      stringsAsFactors = FALSE)

  expect_equal(prec1, comp1)

  # Two by
  prec2 <- Tplyr:::make_prec_data(mtcars_long,
                                  quos(gear, name),
                                  quo(value),
                                  cap=c('int'=99, 'dec'=99)
  ) %>%
    arrange(name) %>%
    as.data.frame()

  comp2 <- data.frame(gear = rep(c(3, 4, 5), 7),
                      name =
                        c(rep('cyl',3), rep('disp',3), rep('drat', 3), rep('hp',3), rep('mpg',3), rep('qsec',3), rep('wt',3)),
                      max_int = c(rep(1, 3), rep(3,3), rep(1, 3), rep(3, 3), rep(2, 3), rep(2, 3), rep(1, 3)),
                      max_dec = c(rep(0, 3), rep(1, 3), rep(2, 3), rep(0, 3), rep(1, 3), rep(2, 2), 1, rep(3, 3)),
                      precision_on = rep('value', 21),
                      stringsAsFactors = FALSE)

  expect_equal(prec2, comp2)

})

test_that('Caps work correctly', {
  # No by
  prec0 <- Tplyr:::make_prec_data(mtcars_long,
                                  quos(),
                                  quo(value),
                                  cap=c('int'=2, 'dec'=1)
  ) %>%
    as.data.frame()

  comp0 <- data.frame(max_int = c(2),
                      max_dec = c(1),
                      precision_on = 'value',
                      stringsAsFactors = FALSE)

  expect_equal(prec0, comp0)

  # One by
  prec1 <- Tplyr:::make_prec_data(mtcars_long,
                                  quos(name),
                                  quo(value),
                                  cap=c('int'=2, 'dec'=1)
  ) %>%
    arrange(name) %>%
    as.data.frame()

  comp1 <- data.frame(name =
                        c('cyl', 'disp', 'drat', 'hp', 'mpg', 'qsec', 'wt'),
                      max_int = c(1, 2, 1, 2, 2, 2, 1),
                      max_dec = c(0, 1, 1, 0, 1, 1, 1),
                      precision_on = rep('value', 7),
                      stringsAsFactors = FALSE)

  expect_equal(prec1, comp1)

  # Two by
  prec2 <- Tplyr:::make_prec_data(mtcars_long,
                                  quos(gear, name),
                                  quo(value),
                                  cap=c('int'=2, 'dec'=1)
  ) %>%
    arrange(name) %>%
    as.data.frame()

  comp2 <- data.frame(gear = rep(c(3, 4, 5), 7),
                      name =
                        c(rep('cyl',3), rep('disp',3), rep('drat', 3), rep('hp',3), rep('mpg',3), rep('qsec',3), rep('wt',3)),
                      max_int = c(rep(1, 3), rep(2,3), rep(1, 3), rep(2, 3), rep(2, 3), rep(2, 3), rep(1, 3)),
                      max_dec = c(rep(0, 3), rep(1, 3), rep(1, 3), rep(0, 3), rep(1, 3), rep(1, 2), 1, rep(1, 3)),
                      precision_on = rep('value', 21),
                      stringsAsFactors = FALSE)

  expect_equal(prec2, comp2)

})

test_that("Precision data can be provided externally", {
  # Mock up a precision data set
  prec <- tibble::tribble(
    ~vs, ~max_int, ~max_dec,
    0,        1,        1,
    1,        2,        2
  )

  t <- tplyr_table(mtcars, gear)
  l <- group_desc(t, wt, by = vs) %>%
         set_precision_data(prec)

  t <-add_layers(t, l)

  # Proper data builds without error
  expect_silent(build(t))
})

test_that("Missing by variables are handled as specified in precision data",{

  # Mock up a precision data set
  prec2 <- tibble::tribble(
    ~vs, ~max_int, ~max_dec,
    0,        1,        1
  )

  expect_snapshot_error({
    t <- tplyr_table(mtcars, gear)
    l <- group_desc(t, wt, by = vs) %>%
          set_precision_data(prec2)
    t <- add_layers(t, l)
    build(t)
  })

  expect_snapshot_error({
    t <- tplyr_table(mtcars, gear)
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(prec2, default="error")
    t <- add_layers(t, l)
    build(t)
  })

  expect_snapshot_error({
    t <- tplyr_table(mtcars, gear)
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(prec2, default="blah")
    t <- add_layers(t, l)
    build(t)
  })

  expect_snapshot({
    t <- tplyr_table(mtcars, gear)
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(prec2, default="auto")
    t <- add_layers(t, l)
    as.data.frame(build(t))
  })

})

test_that("Data validation for external precision data works effectively", {
  # Mock up a precision data set
  prec <- tibble::tribble(
    ~vs, ~max_int, ~max_dec,
    0,        1,        1,
    1,        2,        2
  )

  # max_int and max_dec must exist
  p1 <- select(prec, -max_dec)
  p2 <- select(prec, -max_int)

  t <- tplyr_table(mtcars, gear)

  expect_snapshot_error({
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(p1)
  })

  expect_snapshot_error({
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(p2)
  })

  # max_int and max_dec must be valid integers
  p3 <- prec %>% mutate(max_int = max_int + .1)
  p4 <- prec %>% mutate(max_dec = max_dec + .1)

  expect_snapshot_error({
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(p3)
  })

  expect_snapshot_error({
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(p4)
  })

  # by variable types match
  p5 <- prec %>% mutate(vs = as.character(vs))

  expect_snapshot_error({
    l <- group_desc(t, wt, by = vs) %>%
      set_precision_data(p5)
    t <- add_layers(t, l)
    build(t)
  })
})


test_that("Partially provided decimal precision caps populate correctly", {

  load(test_path('adlb.Rdata'))

  t <- tplyr_table(adlb, TRTA, where = PARAMCD == 'URATE') %>%
    add_layer(
      group_desc(AVAL) %>%
        set_format_strings("Mean (SD)" = f_str("a.a (a.a)", mean, sd), cap = c(dec = 1))
    ) %>%
    add_layer(
      group_desc(AVAL) %>%
        set_format_strings("Mean (SD)" = f_str("a.a (a.a)", mean, sd), cap = c(int = 1))
    ) %>%
    add_layer(
      group_desc(AVAL) %>%
        set_format_strings("Mean (SD)" = f_str("a.a (a.a)", mean, sd), cap = c(int = 1, dec = 1))
    )

  # In bug #20 this caused an error so expect build to complete correctly
  expect_silent(d <- build(t))

  # Manually verified these results look appropriate
  expect_snapshot(as.data.frame(d %>% select(starts_with('var1'))))
})
