
mtcars_long <- mtcars %>%
  rownames_to_column(var = "model") %>%
  pivot_longer(cols = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'))

Tplyr:::make_prec_data(mtcars_long, quos(name), quo(value), cap=c('int'=99, 'dec'=99)) %>%
  arrange(name)

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

