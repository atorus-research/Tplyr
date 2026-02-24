test_that("Descriptive statistics data limiting works properly", {
  t1 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT))
    )

  x1 <- build(t1)

  cnts1 <- dplyr::count(x1, row_label1, row_label2)
  expect_equal(cnts1$n, c(18, 18, 18, 18))

  t2 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PARAM, AVISIT)
    )

  x2 <- build(t2)

  cnts2 <- dplyr::count(x2, row_label1, row_label2)
  expect_equal(cnts2$n, c(6, 18, 6, 18))


  t3 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PECAT, PARAM, AVISIT)
    )

  x3 <- build(t3)

  cnts3 <- dplyr::count(x3, row_label1, row_label2)
  expect_equal(cnts3$n, c(6, 18, 18))
})


test_that("Shift layers can also handle data limiting", {

  t1 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_shift(vars(row=BASEC, column=AVALC), by = vars(PECAT, PARAM, AVISIT))
    )

  x1 <- build(t1)

  cnts1 <- dplyr::count(x1, row_label1, row_label2)
  expect_equal(cnts1$n, c(9, 9, 9, 9))

  t2 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_shift(vars(row=BASEC, column=AVALC), by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PARAM, AVISIT)
    )

  x2 <- build(t2)

  cnts2 <- dplyr::count(x2, row_label1, row_label2)
  expect_equal(cnts2$n, c(3, 9, 3, 9))

  t3 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_shift(vars(row=BASEC, column=AVALC), by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PECAT, PARAM, AVISIT)
    )

  x3 <- build(t3)

  cnts3 <- dplyr::count(x3, row_label1, row_label2)
  expect_equal(cnts3$n, c(3, 9, 9))

})


test_that("Count data limiting works properly", {

  t1 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_count(AVALC, by = vars(PECAT, PARAM, AVISIT))
    )

  x1 <- build(t1)

  cnts1 <- dplyr::count(x1, row_label1, row_label2)
  expect_equal(cnts1$n, c(9, 9, 9, 9))

  t2 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PARAM, AVISIT)
    )

  x2 <- build(t2)

  cnts2 <- dplyr::count(x2, row_label1, row_label2)
  expect_equal(cnts2$n, c(3, 9, 3, 9))

  t3 <- tplyr_table(tplyr_adpe, TRT01A) %>%
    add_layer(
      group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PECAT, PARAM, AVISIT)
    )

  x3 <- build(t3)

  cnts3 <- dplyr::count(x3, row_label1, row_label2)
  expect_equal(cnts3$n, c(3, 9, 9))
})


test_that("Nested count layers limit data - set_limit_data_by removes invalid by x target combos", {
  # Create synthetic data with multiple body systems where AEDECOD is nested
  # within AEBODSYS. Using the pattern group_count(vars(AEDECOD, AESEV), by = vars(AEBODSYS))
  # so that AEBODSYS is a by variable. Without set_limit_data_by, complete() creates
  # all AEBODSYS x AEDECOD combinations including invalid ones.
  ae_data <- tibble::tibble(
    USUBJID = paste0("SUBJ-", 1:12),
    TRT = rep(c("Placebo", "Drug"), each = 6),
    AEBODSYS = c(
      "Cardiac", "Cardiac", "GI", "GI", "GI", "GI",
      "Cardiac", "Cardiac", "GI", "GI", "GI", "Cardiac"
    ),
    AEDECOD = c(
      "Palpitations", "Tachycardia", "Nausea", "Nausea", "Vomiting", "Vomiting",
      "Palpitations", "Palpitations", "Nausea", "Vomiting", "Nausea", "Tachycardia"
    ),
    AESEV = c(
      "MILD", "MODERATE", "MILD", "MODERATE", "MILD", "MILD",
      "MILD", "MODERATE", "MILD", "MILD", "MODERATE", "MILD"
    )
  )

  # Without limit_data_by: complete() creates full cartesian of AEBODSYS x AEDECOD
  res_no_limit <- tplyr_table(ae_data, TRT) %>%
    add_layer(
      group_count(vars(AEDECOD, AESEV), by = vars(AEBODSYS))
    ) %>%
    build()

  # With limit_data_by on AEBODSYS and AEDECOD
  res_limited <- tplyr_table(ae_data, TRT) %>%
    add_layer(
      group_count(vars(AEDECOD, AESEV), by = vars(AEBODSYS)) %>%
        set_limit_data_by(AEBODSYS, AEDECOD)
    ) %>%
    build()

  # The limited result should have FEWER rows than the unlimited result
  expect_lt(nrow(res_limited), nrow(res_no_limit))

  # Verify no invalid combinations: "Cardiac" should NOT have "Nausea" or "Vomiting"
  cardiac_rows <- res_limited %>%
    dplyr::filter(row_label1 == "Cardiac")
  cardiac_terms <- trimws(cardiac_rows$row_label2)
  expect_false(any(cardiac_terms %in% c("Nausea", "Vomiting")))

  # Verify no invalid combinations: "GI" should NOT have "Palpitations" or "Tachycardia"
  gi_rows <- res_limited %>%
    dplyr::filter(row_label1 == "GI")
  gi_terms <- trimws(gi_rows$row_label2)
  expect_false(any(gi_terms %in% c("Palpitations", "Tachycardia")))
})


test_that("Nested count layers limit data - original test with tplyr_adae", {
  # Original test structure preserved but with non-vacuous assertions
  t_ae1 <- tplyr_table(tplyr_adae, TRTA) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD), by = vars(AESEV, AEOUT)) %>%
        set_limit_data_by(AEOUT, AEDECOD)
    )

  t_ae_df1 <- t_ae1 %>%
    build() %>% select(-starts_with('ord'))

  t_ae2 <- tplyr_table(tplyr_adae, TRTA) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD), by = vars(AESEV, AEOUT)) %>%
        set_limit_data_by(AESEV, AEOUT, AEDECOD)
    )

  t_ae_df2 <- t_ae2 %>%
    build() %>% select(-starts_with('ord'))

  dropped_rows <- anti_join(
    t_ae_df1,
    t_ae_df2,
    by=names(t_ae_df1)
  )

  # The more restrictive limit should drop some rows
  # Verify this is non-vacuous (dropped_rows should have actual rows)
  expect_gt(nrow(dropped_rows), 0)

  # All dropped rows should be all-zero
  check <- c(dropped_rows$var1_Placebo, dropped_rows$`var1_Xanomeline High Dose`, dropped_rows$`var1_Xanomeline Low Dose`)
  expect_true(all(check == " 0 (  0.0%)"))
})
