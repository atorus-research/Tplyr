library(dplyr)

adpe <- tibble::tribble(~USUBJID,  ~AVISIT,     ~PECAT, ~PARAM,    ~TRT01A,   ~AVALC, ~AVAL, ~BASEC,
                "101-001", "Screening", "A",    "Head",    "TRT A",  "Normal", 1, "Abnormal",
                "101-001", "Screening", "A",    "Lungs",   "TRT A",  "Normal", 2, "Semi-Normal",
                "101-001", "Day -1",    "A",    "Lungs",   "TRT A",  "Normal", 3, "Normal",
                "101-001", "Day 5",     "A",    "Lungs",   "TRT A",  "Normal", 4, "Normal",
                "101-002", "Screening", "A",    "Head",    "TRT B",  "Semi-Normal", 5, "Normal",
                "101-002", "Screening", "A",    "Lungs",   "TRT B",  "Normal", 6, "Normal",
                "101-002", "Day -1",    "A",    "Lungs",   "TRT B",  "Normal", 7, "Normal",
                "101-002", "Day 5",     "A",    "Lungs",   "TRT B",  "Normal", 8, "Semi-Normal",
                "101-003", "Screening", "A",    "Head",    "TRT A",  "Normal", 9, "Normal",
                "101-003", "Screening", "A",    "Lungs",   "TRT A",  "Abnormal", 10, "Abnormal",
                "101-003", "Day -1",    "A",    "Lungs",   "TRT A",  "Normal", 11, "Abnormal",
                "101-003", "Day 5",     "A",    "Lungs",   "TRT A",  "Abnormal", 12, "Abnormal",

                "101-001", "Screening", "B",     "Lungs",   "TRT A",  "Normal", 13, "Normal",
                "101-001", "Day -1",    "B",     "Lungs",   "TRT A",  "Normal", 14, "Normal",
                "101-001", "Day 5",     "B",     "Lungs",   "TRT A",  "Normal", 15, "Semi-Normal",
                "101-002", "Screening", "B",     "Lungs",   "TRT B",  "Normal", 16, "Normal",
                "101-002", "Day -1",    "B",     "Lungs",   "TRT B",  "Normal", 17, "Abnormal",
                "101-002", "Day 5",     "B",     "Lungs",   "TRT B",  "Normal", 18, "Abnormal",
                "101-003", "Screening", "B",     "Lungs",   "TRT A",  "Abnormal", 19, "Normal",
                "101-003", "Day -1",    "B",    "Lungs",    "TRT A",   "Normal", 20, "Normal",
                "101-003", "Day 5",     "B",     "Lungs",   "TRT A",  "Abnormal", 21, "Normal"
)

adpe$AVALC <- factor(adpe$AVALC, levels = c("Normal", "Semi-Normal", "Abnormal"))
adpe$BASEC <- factor(adpe$BASEC, levels = c("Normal", "Semi-Normal", "Abnormal"))

test_that("Descriptive statistics data limiting works properly", {
  t1 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT))
    )

  x1 <- build(t1)

  cnts1 <- count(x1, row_label1, row_label2)
  expect_equal(cnts1$n, c(18, 18, 18, 18))

  t2 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PARAM, AVISIT)
    )

  x2 <- build(t2)

  cnts2 <- count(x2, row_label1, row_label2)
  expect_equal(cnts2$n, c(6, 18, 6, 18))


  t3 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PECAT, PARAM, AVISIT)
    )

  x3 <- build(t3)

  cnts3 <- count(x3, row_label1, row_label2)
  expect_equal(cnts3$n, c(6, 18, 18))
})


test_that("Shift layers can also handle data limiting", {

  t1 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_shift(vars(row=BASEC, column=AVALC), by = vars(PECAT, PARAM, AVISIT))
    )

  x1 <- build(t1)

  cnts1 <- count(x1, row_label1, row_label2)
  expect_equal(cnts1$n, c(9, 9, 9, 9))

  t2 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_shift(vars(row=BASEC, column=AVALC), by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PARAM, AVISIT)
    )

  x2 <- build(t2)

  cnts2 <- count(x2, row_label1, row_label2)
  expect_equal(cnts2$n, c(3, 9, 3, 9))

  t3 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_shift(vars(row=BASEC, column=AVALC), by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PECAT, PARAM, AVISIT)
    )

  x3 <- build(t3)

  cnts3 <- count(x3, row_label1, row_label2)
  expect_equal(cnts3$n, c(3, 9, 9))

})


test_that("Nested count layers limit data accurately", {

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

  check <- c(dropped_rows$var1_Placebo, dropped_rows$`var1_Xanomeline High Dose`, dropped_rows$`var1_Xanomeline Low Dose`)
  expect_true(all(check == " 0 (  0.0%)"))
})


test_that("Descriptive statistics data limiting works properly", {

  t1 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_count(AVALC, by = vars(PECAT, PARAM, AVISIT))
    )

  x1 <- build(t1)

  cnts1 <- count(x1, row_label1, row_label2)
  expect_equal(cnts1$n, c(9, 9, 9, 9))

  t2 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PARAM, AVISIT)
    )

  x2 <- build(t2)

  cnts2 <- count(x2, row_label1, row_label2)
  expect_equal(cnts2$n, c(3, 9, 3, 9))

  t3 <- tplyr_table(adpe, TRT01A) %>%
    add_layer(
      group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
        set_limit_data_by(PECAT, PARAM, AVISIT)
    )

  x3 <- build(t3)

  cnts3 <- count(x3, row_label1, row_label2)
  expect_equal(cnts3$n, c(3, 9, 9))
})


