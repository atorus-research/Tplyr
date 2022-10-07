
## Initial set-up and framework exists
test_that("A container named `stats` exists in a new layer", {
  t <- tplyr_table(mtcars, gear)
  l1 <- group_count(t, carb)

  expect_equal(l1$stats, list())
})

## Everything goes in correctly.
test_that("`add_risk_diff` adds an element of the correct type to the `stats` container", {
  t <- tplyr_table(mtcars, gear)
  l1 <- group_count(t, carb) %>%
    add_risk_diff(c('4', '3'))

  # 1 container added with proper class
  expect_equal(length(l1$stats), 1)
  expect_s3_class(l1$stats[[1]], 'tplyr_statistic')
  expect_s3_class(l1$stats[[1]], 'tplyr_riskdiff')

  s <- l1$stats[[1]]
  # Contents of the class are accurate
  expect_equal(s$comparisons, list(c('4', '3')))
  expect_equal(s$args, list())
})

## Can't be applied to a wrong layer type
test_that("`add_risk_diff` can't be applied to a non-count layer", {


  expect_snapshot_error({
    t <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_desc(mpg) %>%
          add_risk_diff(c('5', '3'))
      )
  })

})

## Parameters are checked and errors are triggered properly.
test_that("Improper parameter entry is handled correctly", {
  t <- tplyr_table(mtcars, gear)
  l1 <- group_count(t, carb)

  # Not character
  expect_snapshot_error({
    l1 %>% add_risk_diff(c(1,2))
  })

  # Not two elements
  expect_snapshot_error({
    l1 %>% add_risk_diff(c('1', '2', '3'))
  })

  # Invalid arguments to prop.test
  expect_snapshot_error({
    l1 %>% add_risk_diff(c('5', '4'), args=list(badname = 2))
  })

})

## Risk difference processes properly with defaults
test_that("Default processing happens correctly", {
  ## Two group comparisons with default options applied
  t <- tplyr_table(mtcars, gear)

  # Basic risk diff for two groups, using defaults
  l1 <- group_count(t, carb) %>%
    # Compare 4 vs. 3, 5 vs. 3
    add_risk_diff(
      c('4', '3')
    )

  # Build and show output
  dat <- suppressWarnings(add_layers(t, l1) %>% build())

  # 5 columns
  expect_equal(ncol(dat), 7)

  # 1 rdiff column (check if rdiff in the names of dat and sum the logicals)
  expect_equal(sum(grepl("rdiff", names(dat), fixed=TRUE)), 1)

  # Correct number of rows (unique values in carb)
  expect_equal(length(unique(mtcars$carb)), nrow(dat))

  # 1st row value is accurate
  # Yes this is a lame test - but it's manually verified. Shut it.
  expect_equal(dat$rdiff_4_3[[1]], " 0.133 (-0.277,  0.543)")

})

## Risk difference processes properly with defaults
test_that("Multiple comparisons properly populate", {
  ## Two group comparisons with default options applied
  t <- tplyr_table(mtcars, gear)

  # Basic risk diff for two groups, using defaults
  l1 <- group_count(t, carb) %>%
    # Compare 4 vs. 3, 5 vs. 3
    add_risk_diff(
      c('4', '3'),
      c('5', '3')
    )

  # Build and show output
  dat <- suppressWarnings(add_layers(t, l1) %>% build())

  # 5 columns
  expect_equal(ncol(dat), 8)

  # 1 rdiff column (check if rdiff in the names of dat and sum the logicals)
  expect_equal(sum(grepl("rdiff", names(dat), fixed=TRUE)), 2)

  # Correct number of rows (unique values in carb)
  expect_equal(length(unique(mtcars$carb)), nrow(dat))

  # 1st row value is accurate
  # Yes this is a lame test - but it's manually verified. Shut it.
  expect_equal(dat$rdiff_5_3[[2]], " 0.133 (-0.484,  0.751)")

})

## Format strings are applied correctly.
test_that("Passing arguments into prop.test update values correctly", {
  ## Two group comparisons with default options applied
  t <- tplyr_table(mtcars, gear)

  # Basic risk diff for two groups, using defaults
  l1 <- group_count(t, carb) %>%
    # Compare 4 vs. 3, 5 vs. 3
    add_risk_diff(
      c('4', '3'),
      args = list(conf.level=.9, correct=FALSE, alternative="less")
    )

  dat <- suppressWarnings(add_layers(t, l1) %>% build())

  expect_equal(dat$rdiff_4_3[[1]], " 0.133 (-1.000,  0.352)")

})

test_that("Invalid name to format string call errors properly", {
  ## Two group comparisons with default options applied
  t <- tplyr_table(mtcars, gear)

  # Basic risk diff for two groups, using defaults
  expect_snapshot_error({
    l1 <- group_count(t, carb) %>%
      # Compare 4 vs. 3, 5 vs. 3
      add_risk_diff(
        c('4', '3')
      ) %>%
      set_format_strings(badname = f_str('xx.xxx', dif))
  })

})

## Format strings are applied correctly.
test_that("Format strings are applied correctly", {
  ## Two group comparisons with default options applied
  t <- tplyr_table(mtcars, gear)

  # Basic risk diff for two groups, using defaults
  l1 <- group_count(t, carb) %>%
    # Compare 4 vs. 3, 5 vs. 3
    add_risk_diff(
      c('4', '3')
    ) %>%
    set_format_strings(
      riskdiff = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', ref, comp, dif, low, high)
      )

  dat <- suppressWarnings(add_layers(t, l1) %>% build())

  expect_equal(dat$rdiff_4_3[[1]], " 0.200,  0.333,  0.133, -0.277,  0.543")

})

## Format strings are applied correctly.
test_that("Make sure display values accurately reflect prop.test results", {
  ## Two group comparisons with default options applied
  t <- tplyr_table(mtcars, gear)

  # Basic risk diff for two groups, using defaults
  l1 <- group_count(t, carb) %>%
    # Compare 4 vs. 3, 5 vs. 3
    add_risk_diff(
      c('4', '3')
    ) %>%
    set_format_strings(
      riskdiff = f_str('xx.xxxxxx, xx.xxxxxx, xx.xxxxxx, xx.xxxxxx, xx.xxxxxx', comp, ref, dif, low, high)
    )

  # Build the table
  dat <- suppressWarnings(add_layers(t, l1) %>% build())

  # Pick out the available results
  results <- dat$rdiff_4_3[1:4]
  results <- results[results != '']
  results <- map(results, ~ as.numeric(str_split(.x, ", ")[[1]]))

  # Run a manual prop test from the manually checked values
  carb_1 <- suppressWarnings(prop.test(c(4, 3), c(12, 15)))
  carb_2 <- suppressWarnings(prop.test(c(4, 4), c(12, 15)))
  carb_3 <- suppressWarnings(prop.test(c(0, 3), c(12, 15)))
  carb_4 <- suppressWarnings(prop.test(c(4, 5), c(12, 15)))

  # Get the values for carb == 1
  carb_1_res <- unname(
    c(carb_1$estimate[1], carb_1$estimate[2], carb_1$estimate[1] - carb_1$estimate[2], carb_1$conf.int[1], carb_1$conf.int[2])
    )

  # Get the values for carb == 2
  carb_2_res <- unname(
    c(carb_2$estimate[1], carb_2$estimate[2], carb_2$estimate[1] - carb_2$estimate[2], carb_2$conf.int[1], carb_2$conf.int[2])
  )

  # Get the values for carb == 3
  carb_3_res <- unname(
    c(carb_3$estimate[1], carb_3$estimate[2], carb_3$estimate[1] - carb_3$estimate[2], carb_3$conf.int[1], carb_3$conf.int[2])
  )

  # Get the values for carb == 4
  carb_4_res <- unname(
    c(carb_4$estimate[1], carb_4$estimate[2], carb_4$estimate[1] - carb_4$estimate[2], carb_4$conf.int[1], carb_4$conf.int[2])
  )

  expect_equal(results[[2]], carb_2_res, tolerance = .00001)
  expect_equal(results[[3]], carb_3_res, tolerance = .00001)
  expect_equal(results[[4]], carb_4_res, tolerance = .00001)
  expect_equal(results[[1]], carb_1_res, tolerance = .00001)
})

test_that("Distinct or non-distinct values are chosen properly", {
  load(file='adae.Rdata')
  # load(file='vignettes/adae.Rdata')

  ## Two group comparisons with default options applied
  t1 <- tplyr_table(adae, TRTA)
  t2 <- tplyr_table(adae, TRTA)
  t3 <- tplyr_table(adae, TRTA)

  # No distinct variables
  l1 <- group_count(t1, AEBODSYS) %>%
    add_risk_diff(c('Xanomeline High Dose', 'Placebo'))

  # Distinct variables - and use them
  l2 <- group_count(t2, AEBODSYS) %>%
    add_risk_diff(c('Xanomeline High Dose', 'Placebo')) %>%
    set_distinct_by(USUBJID)

  # Distinct variables, don't use them
  l3 <- group_count(t3, AEBODSYS) %>%
    add_risk_diff(c('Xanomeline High Dose', 'Placebo'), distinct=FALSE) %>%
    set_distinct_by(USUBJID)

  dat1 <- suppressWarnings(add_layers(t1, l1) %>% build())
  dat2 <- suppressWarnings(add_layers(t2, l2) %>% build())
  dat3 <- suppressWarnings(add_layers(t3, l3) %>% build())

  expect_true(all(dat1$`rdiff_Xanomeline High Dose_Placebo` == dat3$`rdiff_Xanomeline High Dose_Placebo`))
  expect_true(!all(dat1$`rdiff_Xanomeline High Dose_Placebo` == dat2$`rdiff_Xanomeline High Dose_Placebo`))
  expect_true(!all(dat2$`rdiff_Xanomeline High Dose_Placebo` == dat3$`rdiff_Xanomeline High Dose_Placebo`))

})

test_that("Error generates when duplicating riskdiff comparison values", {

  expect_snapshot_error(
      tplyr_table(mtcars, gear) %>%
        add_layer(
          group_count(cyl) %>%
            add_risk_diff(
              c("4", "4")
            )
        )
  )

})
