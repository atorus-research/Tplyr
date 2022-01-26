
# Store the default options

op <- options()
## Count layer default ----

## Default options pass through
test_that('Default count level options pass forward', {

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3'))
    )

  dat <- suppressWarnings(build(t))

  expected_cnt <- c('3 ( 20.0%)','4 ( 26.7%)','3 ( 20.0%)','5 ( 33.3%)','0 (  0.0%)','0 (  0.0%)')
  expected_rdiff <- c(" 0.133 (-0.277,  0.543)", " 0.067 (-0.348,  0.481)", "-0.200 (-0.477,  0.077)", " 0.000 (-0.358,  0.358)", " 0.000 ( 0.000,  0.000)", " 0.000 ( 0.000,  0.000)")

  expect_equal(dat$var1_3, expected_cnt)
  expect_equal(dat$rdiff_4_3, expected_rdiff)

})

options(op)

## Changing options are picked up
test_that('Default count level options pass forward', {

  op <- options()

  options('tplyr.count_layer_default_formats' = list(
            'n_counts' = f_str('   x', n),
            'riskdiff' = f_str('xx.xxx', dif)
          ))

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3'))
    )

  dat <- suppressWarnings(build(t))

  expected_cnt <- c('   3','   4','   3','   5','   0','   0')
  expected_rdiff <- c(" 0.133", " 0.067", "-0.200", " 0.000", " 0.000", " 0.000")

  expect_equal(dat$var1_3, expected_cnt)
  expect_equal(dat$rdiff_4_3, expected_rdiff)

  options(op)

})

options(op)

## table level overrides work
test_that('Table level overrides superseed option defaults', {

  # Also testing that the formats are merged

  # build the tables
  # Count only
  t_cnt <- tplyr_table(mtcars, gear) %>%
    set_count_layer_formats(
      n_counts = f_str('   x', n)
    ) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3'))
    )

  # Riskdiff only
  t_rsk <- tplyr_table(mtcars, gear) %>%
      set_count_layer_formats(
        riskdiff = f_str('xx.xxx', dif)
      ) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3'))
    )

  # Both
  t_bth <- tplyr_table(mtcars, gear) %>%
    set_count_layer_formats(
      n_counts = f_str('   x', n),
      riskdiff = f_str('xx.xxx', dif)
    ) %>%
  add_layer(
    group_count(carb) %>%
      add_risk_diff(c('4', '3'))
  )

  # Build all
  t_cnt_dat <- suppressWarnings(build(t_cnt))
  t_rsk_dat <- suppressWarnings(build(t_rsk))
  t_bth_dat <- suppressWarnings(build(t_bth))

  # Default values
  def_expected_cnt <- c('3 ( 20.0%)','4 ( 26.7%)','3 ( 20.0%)','5 ( 33.3%)','0 (  0.0%)','0 (  0.0%)')
  def_expected_rdiff <- c(" 0.133 (-0.277,  0.543)", " 0.067 (-0.348,  0.481)", "-0.200 (-0.477,  0.077)", " 0.000 (-0.358,  0.358)", " 0.000 ( 0.000,  0.000)", " 0.000 ( 0.000,  0.000)")

  # Table override values
  tbl_expected_cnt <- c('   3','   4','   3','   5','   0','   0')
  tbl_expected_rdiff <- c(" 0.133", " 0.067", "-0.200", " 0.000", " 0.000", " 0.000")

  # Override count, not risk
  expect_equal(t_cnt_dat$var1_3, tbl_expected_cnt)
  expect_equal(t_cnt_dat$rdiff_4_3, def_expected_rdiff)

  # Override risk, not count
  expect_equal(t_rsk_dat$var1_3, def_expected_cnt)
  expect_equal(t_rsk_dat$rdiff_4_3, tbl_expected_rdiff)

  # Override both
  expect_equal(t_bth_dat$var1_3, tbl_expected_cnt)
  expect_equal(t_bth_dat$rdiff_4_3, tbl_expected_rdiff)

})

options(op)

## Layer level overrides work
test_that('Table level overrides superseed option defaults', {

  # Also testing that the formats are merged

  # These first two test that the option is overridden
  # Override n_counts
  t_rsk <- tplyr_table(mtcars, gear) %>%
    set_count_layer_formats(
      n_counts = f_str('   x', n)
    ) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3')) %>%
        set_format_strings(
          riskdiff = f_str('xx.xxx, xx.xxx', low, high)
        )
    )

  # Riskdiff only
  t_cnt <- tplyr_table(mtcars, gear) %>%
    set_count_layer_formats(
      riskdiff = f_str('xx.xxx', dif)
    ) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3')) %>%
        set_format_strings(
          f_str('(xx%)', pct) # Not using name intentionally to test
                              # that the default assumption that no
                              # name = n_counts works
        )
    )

  # This tests that table level specs are overwritten
  t_bth <- tplyr_table(mtcars, gear) %>%
    set_count_layer_formats(
      n_counts = f_str('   x', n),
      riskdiff = f_str('xx.xxx', dif)
    ) %>%
    add_layer(
      group_count(carb) %>%
        add_risk_diff(c('4', '3')) %>%
        set_format_strings( # Test override
          n_counts = f_str('(xx%)', pct),
          riskdiff = f_str('xx.xxx, xx.xxx', low, high)
        )
    )

  # Build
  t_cnt_dat <- suppressWarnings(build(t_cnt))
  t_rsk_dat <- suppressWarnings(build(t_rsk))
  t_bth_dat <- suppressWarnings(build(t_bth))

  # Table override values
  tbl_expected_cnt <- c('   3','   4','   3','   5','   0','   0')
  tbl_expected_rdiff <- c(" 0.133", " 0.067", "-0.200", " 0.000", " 0.000", " 0.000")

  lay_expected_cnt <- c("(20%)", "(27%)", "(20%)", "(33%)", "( 0%)", "( 0%)")
  lay_expected_rdiff <- c("-0.277,  0.543", "-0.348,  0.481", "-0.477,  0.077", "-0.358,  0.358", " 0.000,  0.000", " 0.000,  0.000")

  # Override count, not risk
  expect_equal(t_cnt_dat$var1_3, lay_expected_cnt)
  expect_equal(t_cnt_dat$rdiff_4_3, tbl_expected_rdiff)

  # Override risk, not count
  expect_equal(t_rsk_dat$var1_3, tbl_expected_cnt)
  expect_equal(t_rsk_dat$rdiff_4_3, lay_expected_rdiff)

  # Override both
  expect_equal(t_bth_dat$var1_3, lay_expected_cnt)
  expect_equal(t_bth_dat$rdiff_4_3, lay_expected_rdiff)

})

options(op)

## Scipen override works
test_that('Scientific notation overrides function properly', {

 op <- options()
 # Using default
 def <- tplyr_table(mtcars, gear) %>%
   add_layer(
     group_count(cyl) %>%
       add_risk_diff(c('4', '3'))
   )

 def <- suppressWarnings(build(def))

 # Using changed
 options('tplyr.scipen' = -4)
 chg <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        add_risk_diff(c('4', '3'))
    )

 chg <- suppressWarnings(build(chg))

 # Make sure scipen outside of tplyr is unchanged
 expect_equal(getOption('scipen'), 0)

 expected_def <- c(" 0.600 ( 0.230,  0.970)"," 0.200 (-0.192,  0.592)", "-0.800 (-1.000, -0.523)")
 expected_chg <- c(" 6e-01 (2.3e-01, 9.7e-01)", " 2e-01 (-1.92e-01, 5.92e-01)", "-8e-01 (-1.000, -5.23e-01)")

 expect_equal(def$rdiff_4_3, expected_def)
 expect_equal(chg$rdiff_4_3, expected_chg)

 options(op)

})

options(op)

## Desc layer defaults ----

test_that('Default count level options pass forward', {

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat)
    )

  dat <- suppressWarnings(build(t))

  expected <- c(" 15", "3.133 (0.2737)", "3.080", "3.035, 3.180", "2.76, 3.73", "  0")

  expect_equal(dat$var1_3, expected)

})

options(op)

## Changing options are picked up
test_that('Default count level options pass forward', {

  op <- options()

  options('tplyr.desc_layer_default_formats' = list(
    'n' = f_str('xx', n)
  ))

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat)
    )

  dat <- suppressWarnings(build(t))

  expected = c("15")

  expect_equal(dat$var1_3, expected)

  options(op)

})

options(op)

## table level overrides work
test_that("Table level overrides work on desc layers", {

  t <- tplyr_table(mtcars, gear) %>%
    set_desc_layer_formats(
      'n' = f_str('xx', n)
    ) %>%
    add_layer(
      group_desc(drat)
    )

    dat <- suppressWarnings(build(t))

    expected = c("15")

    expect_equal(dat$var1_3, expected)

})

options(op)

## Layer level overrides work
test_that("Table level overrides work on desc layers", {

  # Default options are overriden
  t_opt <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat) %>%
        set_format_strings(
          'mean' = f_str('xx.xx', mean)
        )
    )

  # Table level overrides and overriden by layer
  t_tbl <- tplyr_table(mtcars, gear) %>%
    set_desc_layer_formats(
      'n' = f_str('xx', n)
    ) %>%
  add_layer(
    group_desc(drat) %>%
      set_format_strings(
        'mean' = f_str('xx.xx', mean)
      )
  )

  # Build
  t_opt_dat <- suppressWarnings(build(t_opt))
  t_tbl_dat <- suppressWarnings(build(t_tbl))

  # Should always pick up layer level
  expect_equal(t_opt_dat$var1_3, c(" 3.13"))
  expect_equal(t_tbl_dat$var1_3, c(" 3.13"))

})

options(op)

## Precision cap is effective
test_that('Precision cap override is picked up from option', {

  op <- options()
  # Default without resetting option
  t_def <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat, by=am) %>%
        set_format_strings(
          'mean' = f_str('a.a', mean)
        )
    )

  t_def_dat <- suppressWarnings(build(t_def))

  # Reset option
  options('tplyr.precision_cap' = c('int'=1, 'dec'=1))
  # Create table again
  t_opt <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat, by=am) %>%
        set_format_strings(
          'mean' = f_str('a.a', mean)
        )
    )

  # Build
  t_opt_dat <- suppressWarnings(build(t_opt))

  # Test
  expect_equal(t_def_dat$var1_3, c("3.13", ""))
  expect_equal(t_opt_dat$var1_3, c("3.1", ""))

  options(op)
})

options(op)

## Custom summaries pass through from options
test_that('Custom summaries set within options are available for build', {

  op <- options()

  options(tplyr.custom_summaries =
            quos(geometric_mean = exp(sum(log(.var[.var > 0]), na.rm=TRUE) / length(.var)))
          )

  # Default without resetting option
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat, by=am) %>%
        set_format_strings(
          'Geometric Mean' = f_str('a.a', geometric_mean)
        )
    )

  dat <- build(t)

  # Test
  expect_equal(dat$var1_3, c("3.12", ""))

  options(op)
})

test_that("Quantile switch works properly", {

  op <- options()
  expect_equal(getOption('tplyr.quantile_type'), 7)

  dat1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(disp) %>%
        set_format_strings(x = f_str('xx, xx, xx', iqr, q1, q3))
    ) %>%
    get_numeric_data(layer=1)

  res1 <- c(104.200, 275.800, 380.000,  81.075,  78.925, 160.000, 180.700, 120.300, 301.000)
  expect_equal(dat1$value, res1)

  options('tplyr.quantile_type' = 3)

  dat2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(disp) %>%
        set_format_strings(x = f_str('xx, xx, xx', iqr, q1, q3))
    ) %>%
    get_numeric_data(layer=1)

  res2 <- c(84.2, 275.8, 360.0,  81.3,  78.7, 160.0, 205.9,  95.1, 301.0)
  expect_equal(dat2$value, res2)

  expect_true(!all(dat1$value != dat2$value))

})

### Shift Layer Defaults ----
test_that("Shift layer defaults are created as expected", {
  t <- tplyr_table(mtcars, gear)

  s1 <- group_shift(t, vars(row = cyl, column = mpg))

  expect_equal(gather_defaults(s1)[[1]], f_str("a", n))
})

test_that("Shift layer defaults can be changed" ,{
  op <- options()

  options(tplyr.shift_layer_default_formats = list(
    f_str("xxxx (xxxx.xxx%) *****", n, pct)
  ))

  t <- tplyr_table(mtcars, gear)

  s1 <- group_shift(t, vars(row = cyl, column = mpg))

  expect_equal(gather_defaults(s1)[[1]], f_str("xxxx (xxxx.xxx%) *****", n, pct))

  options(op)

  expect_equal(gather_defaults(s1) [[1]], f_str("a", n))
})

test_that("Shift layer defaults can be overridden", {
  t <- tplyr_table(mtcars, gear)

  s1 <- group_shift(t, vars(row = cyl, column = mpg)) %>%
    set_format_strings(f_str("xx (xx.xx%)", n, pct))

  expect_equal(gather_defaults(s1)[[1]], f_str("a", n))
  expect_equal(s1$format_strings, f_str("xx (xx.xx%)", n, pct))
})

options(op)
