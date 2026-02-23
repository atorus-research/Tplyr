## Hug character formatting testing ----
load(test_path('adsl.Rdata'))
load(test_path('adlb.Rdata'))

y <- adlb %>%
  mutate(
    AVAL = if_else(
      AVISIT == "End of Treatment",
      85.301,
      AVAL
    )
  )

test_that("Hug character formatting applies properly for count layers", {
  # These tests are various applications of appropriate hug character formats

  # Single hug character, count layer, manual
  x <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE) %>%
        set_format_strings(f_str("a (XXX.x%)", n, pct))
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x, c(" 0   (0.0%)", " 8   (9.3%)", "78  (90.7%)"))

  # Multi hug character, count layer, manual, single hug auto
  x <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE) %>%
        set_format_strings(f_str("a {(XXX.x%)} [A]", distinct_n, distinct_pct, n))  %>%
        set_distinct_by(USUBJID)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x, c(" 0   {(0.0%)}  [0]", " 8   {(9.3%)}  [8]", "78  {(90.7%)} [78]"))

  # Single hug character, count layer, auto
  x <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE) %>%
        set_format_strings(f_str("a (XX.x%) [A]", distinct_n, distinct_pct, n)) %>%
        set_distinct_by(USUBJID)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x, c(" 0  (0.0%)  [0]", " 8  (9.3%)  [8]", "78 (90.7%) [78]"))

  # Multi hug character, count layer, auto
  x <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE) %>%
        set_format_strings(f_str("a {(XX.x%)} [[A]]", distinct_n, distinct_pct, n)) %>%
        set_distinct_by(USUBJID)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x, c(" 0  {(0.0%)}  [[0]]", " 8  {(9.3%)}  [[8]]", "78 {(90.7%)} [[78]]"))
})

test_that("Hug character formatting applies properly for desc layers", {

  # Single hug character, desc layer, manual
  x <- tplyr_table(y, TRTA) %>%
    add_layer(
      group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>%
        set_format_strings(
          TEST = f_str("xxx.x (XX.x)", mean, sd, empty="NA")
        ) %>%
        set_precision_by(PARAMCD)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x[1:3], c("279.6   (NA)", "323.4 (85.7)", " 85.3  (0.0)"))

  # Multi hug character, desc layer, manual
  x <- tplyr_table(y, TRTA) %>%
    add_layer(
      group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>%
        set_format_strings(
          TEST = f_str("xxx.x {(XX.x)}", mean, sd, empty="NA")
        ) %>%
        set_precision_by(PARAMCD)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x[1:3], c("279.6   {(NA)}", "323.4 {(85.7)}", " 85.3  {(0.0)}"))

  # Single hug character, desc layer, auto
  x <- tplyr_table(y, TRTA) %>%
    add_layer(
      group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>%
        set_format_strings(
          TEST = f_str("xxx.x (A.x)", mean, sd, empty="NA")
        ) %>%
        set_precision_by(PARAMCD)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x[1:3], c("279.6   (NA)", "323.4 (85.7)", " 85.3  (0.0)"))

  # Multi hug character, desc layer, auto
  x <- tplyr_table(y, TRTA) %>%
    add_layer(
      group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>%
        set_format_strings(
          TEST = f_str("xxx.x {(A.x)}", mean, sd, empty="NA")
        ) %>%
        set_precision_by(PARAMCD)
    ) %>%
    build() %>%
    pull(var1_Placebo)

  expect_equal(x[1:3], c("279.6   {(NA)}", "323.4 {(85.7)}", " 85.3  {(0.0)}"))
})

test_that("Hug character formatting applies properly for shift layers", {

  # Shift layer, single hug char, manual and auto
  x <- tplyr_table(adlb, TRTA, where=VISIT %in% c("SCREENING 1", "UNSCHEDULED 1.1")) %>%
    add_layer(
      group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT)) %>%
        set_format_strings(f_str("(A) (XXX.x%)", n, pct))
    ) %>%
    build() %>%
    pull(var1_Placebo_N)

  expect_equal(x, c(" (8) (100.0%)", " (0)   (0.0%)"))

  # Shift layer, multi hug char, manual and auto
  x <- tplyr_table(adlb, TRTA, where=VISIT %in% c("SCREENING 1", "UNSCHEDULED 1.1")) %>%
    add_layer(
      group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT)) %>%
        set_format_strings(f_str("((A)) {(XXX.x%)}", n, pct))
    ) %>%
    build() %>%
    pull(var1_Placebo_N)

  expect_equal(x, c(" ((8)) {(100.0%)}", " ((0))   {(0.0%)}"))
})

test_that("IBM rounding option works for count layers", {
  withr::with_options(list(tplyr.IBMRounding = TRUE), {
    x <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_count(cyl) %>%
          set_format_strings(f_str("a (xx.x%)", n, pct))
      ) %>%
      build()
    expect_true(is.data.frame(x))
    expect_true(nrow(x) > 0)
  })
})

test_that("IBM rounding option works for desc layers", {
  withr::with_options(list(tplyr.IBMRounding = TRUE), {
    x <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_desc(mpg) %>%
          set_format_strings("Mean" = f_str("xx.x", mean))
      ) %>%
      build()
    expect_true(is.data.frame(x))
    expect_true(nrow(x) > 0)
  })
})

test_that("Count layer with missing values and hug character formats correctly", {
  dat <- data.frame(
    trt = c("A", "A", "A", "B", "B"),
    val = c("x", "x", NA, "x", NA)
  )

  x <- tplyr_table(dat, trt) %>%
    add_layer(
      group_count(val) %>%
        set_format_strings(f_str("(A) (xxx.x%)", n, pct)) %>%
        set_missing_count(f_str("(A)", n), Missing = NA)
    ) %>%
    build()

  expect_true(is.data.frame(x))
  expect_true(nrow(x) > 0)
})
