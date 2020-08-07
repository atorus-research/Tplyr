context("Atorus Validation")

#' @title Test Cases Code
#' @section Last Updated By:
#' Nathan Kosiba
#' @section Last Update Date:
#' 8/07/2020

#insert any necessary libraries
library(Tplyr)
library(tidyverse)
library(testthat)
library(rlang)

#insert code applicable to all tests i.e. functions
adsl <- haven::read_xpt("~/Tplyr/uat/input/adsl.xpt")
adae <- haven::read_xpt("~/Tplyr/uat/input/adae.xpt")


#no updates needed - initializes vur which is used to determine which parts of code to execute during testing
vur <- NULL
if(file.exists("~/Tplyr/uat/references/output/vur_auto.Rds")) vur <- readRDS("~/Tplyr/uat/references/output/vur_auto.Rds")

#test 1
test_that('T1',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    test_1 <- tplyr_table(adsl, TRT01P)

    # output table to check attributes
    save(test_1, file = "~/Tplyr/uat/output/test_1.RData")

    #clean up working directory
    rm(test_1)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_1.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
    testthat::expect_equal(adsl, Tplyr::pop_data(test_1), label = "T1.1")
    testthat::expect_equal("TRT01P", Tplyr::treat_var(test_1), label = "T1.2")
  #manual check(s)

  #clean up working directory
  rm(test_1)
})

#test 2
test_that('T2',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    test_2 <- tplyr_table(adsl, TRT01P, where = (EFFFL == 'Y'))

    # output table to check attributes
    save(test_2, file = "~/Tplyr/uat/output/test_2.RData")

    #clean up working directory
    rm(test_2)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_2.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
    testthat::expect_equal(filter(adsl, EFFFL == 'Y'), filter(Tplyr::pop_data(test_2),!!Tplyr::get_where(test_2)), label = "T2.1")
  #manual check(s)

  #clean up working directory
  rm(test_2)
})

#test 3
test_that('T3',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_total_group() %>%
      add_treat_group('Total Xanomeline', c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
      add_layer(group_count(AGEGR1))
    build(t)
    test_3 <- header_n(t)

    # output table to check attributes
    save(test_3, file = "~/Tplyr/uat/output/test_3.RData")

    #clean up working directory
    rm(t)
    rm(test_3)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_3.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
    testthat::expect_equal(c("Placebo", "Total", "Total Xanomeline", "Xanomeline High Dose", "Xanomeline Low Dose"),
                           test_3[[1]], label = "T3.1")
    t3_2 <- c(nrow(filter(adsl, TRT01P == "Placebo")), nrow(adsl),
                nrow(filter(adsl, TRT01P == "Xanomeline High Dose" | TRT01P == "Xanomeline Low Dose")),
                nrow(filter(adsl, TRT01P == "Xanomeline High Dose")), nrow(filter(adsl, TRT01P == "Xanomeline Low Dose")))
    testthat::expect_equal(t3_2, test_3[[2]], label = "T3.2")
  #manual check(s)

  #clean up working directory
  rm(test_3)
  rm(t3_2)
})

#test 4
test_that('T4',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    test_4 <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE, by="Age (Groups)", where = SAFFL == "Y")
      )

    # output table to check attributes
    save(test_4, file = "~/Tplyr/uat/output/test_4.RData")

    #clean up working directory
    rm(test_4)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_4.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(adsl, Tplyr::pop_data(test_4), label = "T4.1")
  testthat::expect_equal("TRT01P", Tplyr::treat_var(test_4), label = "T4.2")
  #manual check(s)

  #clean up working directory
  rm(test_4)
})

#test 5
test_that('T5',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    test_5 <- tplyr_table(adae, TRTA) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_layer(
        group_desc(AEDECOD, by="Preferred Term", where = SAFFL == "Y")
      )

    # output table to check attributes
    save(test_5, file = "~/Tplyr/uat/output/test_5.RData")

    #clean up working directory
    rm(test_5)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_5.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(adsl, Tplyr::pop_data(test_5), label = "T5.1")
  testthat::expect_equal(adae, test_5$target, label = "T5.2")
  testthat::expect_equal("TRT01P", Tplyr::treat_var(test_5), label = "T5.3")
  testthat::expect_equal("TRTA", Tplyr::pop_treat_var(test_5), label = "T5.4")
  #manual check(s)

  #clean up working directory
  rm(test_5)
})

#test 6
test_that('T6',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    test_6 <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD, by="Preferred Term", where = SAFFL == "Y")
      )

    # output table to check attributes
    save(test_6, file = "~/Tplyr/uat/output/test_6.RData")

    #clean up working directory
    rm(test_6)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_6.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(filter(adae, SAFFL == 'Y'),
                         filter(Tplyr::pop_data(test_6),!!Tplyr::get_where(test_6)),
                         label = "T6.1")
  #manual check(s)

  #clean up working directory
  rm(test_6)
})

#test 7
test_that('T7',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD)
      ) %>%
      add_layer(
        group_count(AEDECOD) %>%
        set_distinct_by(USUBJID) %>%
        set_format_strings(f_str("xxx", distinct))
      )
    build(t)
    test_7 <- get_numeric_data(t)

    # output table to check attributes
    save(test_7, file = "~/Tplyr/uat/output/test_7.RData")

    #clean up working directory
    rm(t)
    rm(test_7)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_7.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t7_1 <- filter(adae, TRTA == "Placebo") %>%
    group_by(AEDECOD) %>%
    summarise(n=n())
  testthat::expect_equal(t7_1[[2]],
                         subset(test_7[[1]], TRTA == 'Placebo' & n != 0)[['n']],
                         label = "T7.1")
  t7_2 <- filter(adae, TRTA == "Placebo") %>%
    group_by(AEDECOD) %>%
    distinct(USUBJID, AEDECOD) %>%
    summarise(n=n())
  testthat::expect_equal(t7_2[[2]],
                         subset(test_7[[2]], TRTA == 'Placebo' & n != 0)[['distinct_n']],
                         label = "T7.2")
  #manual check(s)

  #clean up working directory
  rm(test_7)
  rm(t7_1)
  rm(t7_2)
})

#test 8
test_that('T8',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD, by=SEX)
      ) %>%
      add_layer(
        group_count(AEDECOD, by=SEX) %>%
          set_distinct_by(USUBJID) %>%
          set_format_strings(f_str("xxx", distinct))
      )
    build(t)
    test_8 <- get_numeric_data(t)

    # output table to check attributes
    save(test_8, file = "~/Tplyr/uat/output/test_8.RData")

    #clean up working directory
    rm(t)
    rm(test_8)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_8.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t8_1 <- filter(adae, TRTA == "Placebo") %>%
    group_by(SEX, AEDECOD) %>%
    summarise(n=n())
  testthat::expect_equal(t8_1[[3]],
                         subset(test_8[[1]], TRTA == 'Placebo' & n != 0)[['n']],
                         label = "T8.1")
  t8_2 <- filter(adae, TRTA == "Placebo") %>%
    group_by(SEX, AEDECOD) %>%
    distinct(USUBJID, SEX, AEDECOD) %>%
    summarise(n=n())
  testthat::expect_equal(t8_2[[3]],
                         subset(test_8[[2]], TRTA == 'Placebo' & n != 0)[['distinct_n']],
                         label = "T8.2")
  #manual check(s)

  #clean up working directory
  rm(test_8)
  rm(t8_1)
  rm(t8_2)
})

#test 9
test_that('T9',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE) %>%
        add_total_row() %>%
        set_total_row_label("TOTAL")
      )
    build(t)
    test_9 <- get_numeric_data(t)

    # output table to check attributes
    save(test_9, file = "~/Tplyr/uat/output/test_9.RData")

    #clean up working directory
    rm(t)
    rm(test_9)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_9.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(summarise(filter(adsl, TRT01P == 'Placebo'), n=n())[[1]],
                         subset(test_9[[1]], TRT01P == 'Placebo' & summary_var == 'TOTAL')[['n']],
                         label = "T9.1")
  #testthat::expect_equal(summarise(filter(adsl, TRT01P == 'Placebo'), n=n())[[1]],
  #                       subset(test_9[[1]], TRT01P == 'Placebo' & summary_var == 'TOTAL')[['n']],
  #                       label = "T9.2")
  #manual check(s)

  #clean up working directory
  rm(test_9)
})

#test 10
test_that('T10',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    adsl$RACE <- factor(adsl$RACE, c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"))
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE)
      )
    build(t)
    test_10 <- get_numeric_data(t)

    # output table to check attributes
    save(test_10, file = "~/Tplyr/uat/output/test_10.RData")

    #clean up working directory
    rm(t)
    rm(test_10)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_10.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"),
                         unique(test_10[[1]]$summary_var),
                         label = "T10.1")
  #clean up working directory
  rm(test_10)
})

#test 11
test_that('T11',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD) %>%
          set_format_strings(f_str("xxx (xx.x%)", n, pct))
      )%>%
      add_layer(
        group_count(AEDECOD) %>%
          set_distinct_by(USUBJID) %>%
          set_format_strings(f_str("xxx (xx.x%)", distinct, distinct_pct))
      )%>%
      add_layer(
        group_count(AEDECOD) %>%
          set_distinct_by(USUBJID) %>%
          set_format_strings(f_str("xxx (xx.x%) [xxx (xx.x%)]", n, pct, distinct, distinct_pct))
      )

    test_11 <- build(t)

    # output table to check attributes
    save(test_11, file = "~/Tplyr/uat/output/test_11.RData")

    #clean up working directory
    rm(t)
    rm(test_11)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_11.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(,label = "T11.1")
  testthat::expect_equal(,label = "T11.2")
  testthat::expect_equal(,label = "T11.3")
  #clean up working directory
  rm(test_11)
})

#test 12
test_that('T12',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE) %>%
          set_format_strings(f_str("xxx (xx.x%)", n, pct))
      )

    build(t)
    test_12 <- get_numeric_data(t)


    # output table to check attributes
    save(test_12, file = "~/Tplyr/uat/output/test_12.RData")

    #clean up working directory
    rm(t)
    rm(test_12)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_12.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t12_1 <- group_by(adsl, TRT01P) %>%
    summarise(n=n())
  testthat::expect_equal(,label = "T12.1")
  testthat::expect_equal(,label = "T12.2")
  #clean up working directory
  rm(test_12)
})

#test 13
test_that('T13',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      set_pop_where(SEX == "F") %>%
      add_layer(
        group_count(RACE) %>%
          set_format_strings(f_str("xxx (xx.x%)", n, pct))
      )

    build(t)
    test_13 <- get_numeric_data(t)


    # output table to check attributes
    save(test_13, file = "~/Tplyr/uat/output/test_13.RData")

    #clean up working directory
    rm(t)
    rm(test_13)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_13.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t13_1 <- filter(adsl, SEX == "F") %>%
    group_by(TRT01P) %>%
    summarise(n=n())
  testthat::expect_equal(,label = "T13.1")
  testthat::expect_equal(,label = "T13.2")
  #clean up working directory
  rm(t13_1)
  rm(test_13)
})

#test 14
test_that('T14',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder

    t <- tplyr_table(adae, TRTA) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_layer(
      group_count(AEDECOD) %>%
        set_format_strings(f_str("xxx (xx.x%)", n, pct))
    )

    build(t)
    test_14 <- get_numeric_data(t)

    # output table to check attributes
    save(test_14, file = "~/Tplyr/uat/output/test_14.RData")

    #clean up working directory
    rm(t)
    rm(test_14)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_14.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t14_1 <- group_by(adsl, TRT01P) %>%
    summarise(n=n())
  testthat::expect_equal(,label = "T14.1")
  testthat::expect_equal(,label = "T14.2")
  #clean up working directory
  rm(t14_1)
  rm(test_14)
})

#test 15
test_that('T15',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01A) %>%
      add_layer(
        group_count(RACE) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'))
      )

    build(t)
    test_15 <- t$layers[[1]]$stats[[1]]$stats_numeric_data


    # output table to check attributes
    save(test_15, file = "~/Tplyr/uat/output/test_15.RData")
    test_15 <- build(t)


    # output table to check attributes
    save(test_15, file = "~/Tplyr/uat/output/test_15.RData")

    #clean up working directory
    rm(t)
    rm(test_15)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_15.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  tot_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose"), n=n())[[1]]
  cnt_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose" & RACE == 'WHITE'), n=n())[[1]]
  tot_p <- summarise(filter(adsl, TRT01P == "Placebo"), n=n())[[1]]
  cnt_p <- summarise(filter(adsl, TRT01P == "Placebo" & RACE == 'WHITE'), n=n())[[1]]
  t15 <- prop.test(c(cnt_t, cnt_p), c(tot_t,tot_p))
  testthat::expect_equal(t15$estimate[[1]] - t15$estimate[[2]],
                         filter(test_15, summary_var == 'WHITE' & measure == 'dif')[[3]],
                         label = "T15.1")
  testthat::expect_equal(c(t15$conf.int[1], t15$conf.int[2]),
                         c(filter(test_15, summary_var == 'WHITE' & measure == 'low')[[3]],
                           filter(test_15, summary_var == 'WHITE' & measure == 'high')[[3]]),
                         label = "T15.2")

  #clean up working directory
  rm(tot_p)
  rm(cnt_p)
  rm(tot_t)
  rm(cnt_t)
  rm(t15)
  rm(test_15)
})


#test 16
test_that('T16',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            'n' = f_str('xx', n),
            'mean' = f_str('xx.x', mean),
            'median' = f_str('xx.x', median),
            'sd' = f_str('xx.xx', sd),
            'var' = f_str('xx.xx', var),
            'min' = f_str('xx', min),
            'max' = f_str('xx', max),
            'iqr' = f_str('xx.x', iqr),
            'q1' = f_str('xx.x', q1),
            'q3' = f_str('xx.x', q3),
            'missing' = f_str('xx', missing)
          )
      )
    built_cont <- t %>%
      build()

    test_16 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_16, file = "~/Tplyr/uat/output/test_16.RData")

    #clean up working directory
    rm(t)
    rm(test_16)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_16.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], n=n())[[1]],
                           subset(test_16, stat == 'n' & TRT01P == 'Placebo')[['value']],
                           label = "T16.1")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], mean=mean(AGE))[[1]],
                           subset(test_16, stat == 'mean' & TRT01P == 'Placebo')[['value']],
                           label = "T16.2")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], median=median(AGE))[[1]],
                           subset(test_16, stat == 'median' & TRT01P == 'Placebo')[['value']],
                           label = "T16.3")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], sd=sd(AGE))[[1]],
                           subset(test_16, stat == 'sd' & TRT01P == 'Placebo')[['value']],
                           label = "T16.4")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], var=var(AGE))[[1]],
                           subset(test_16, stat == 'var' & TRT01P == 'Placebo')[['value']],
                           label = "T16.5")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], min=min(AGE))[[1]],
                           subset(test_16, stat == 'min' & TRT01P == 'Placebo')[['value']],
                           label = "T16.6")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], max=max(AGE))[[1]],
                           subset(test_16, stat == 'max' & TRT01P == 'Placebo')[['value']],
                           label = "T16.7")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], iqr=IQR(AGE))[[1]],
                           subset(test_16, stat == 'iqr' & TRT01P == 'Placebo')[['value']],
                           label = "T16.8")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q1=quantile(AGE)[[2]])[[1]],
                           subset(test_16, stat == 'q1' & TRT01P == 'Placebo')[['value']],
                           label = "T16.9")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q3=quantile(AGE)[[4]])[[1]],
                           subset(test_16, stat == 'q3' & TRT01P == 'Placebo')[['value']],
                           label = "T16.10")
    testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo' & is.na(adsl$AGE),], n=n())[[1]],
                           subset(test_16, stat == 'missing' & TRT01P == 'Placebo')[['value']],
                           label = "T16.11")
  #manual check(s)


  #clean up working directory
  rm(test_16)
})

#format matching
#re_npe <- "\s?\d?\d\s\(\s?\d?\d?\d.\d%\)\s\[\s?\d?\d\]"
#re_np <- "\s?\d?\d\s\(\s?\d?\d?\d.\d%\)"


#clean up
rm(vur)
