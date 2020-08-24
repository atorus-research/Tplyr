context("Atorus Validation")

#' @title Test Cases Code
#' @section Last Updated By:
#' Nathan Kosiba
#' @section Last Update Date:
#' 8/24/2020

#setup ----
#insert any necessary libraries
library(Tplyr)
library(tidyverse)
library(testthat)
library(rlang)

#insert code applicable to all tests i.e. functions or data
adsl <- haven::read_xpt("~/Tplyr/uat/input/adsl.xpt")
adsl$RACE_FACTOR <- factor(adsl$RACE, c("WHITE", "BLACK OR AFRICAN AMERICAN",
                                        "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"))

adae <- haven::read_xpt("~/Tplyr/uat/input/adae.xpt")

advs <- haven::read_xpt("~/Tplyr/uat/input/advs.xpt")

adlb <- haven::read_xpt("~/Tplyr/uat/input/adlbc.xpt")
adlb$ANRIND_FACTOR <- factor(adlb$ANRIND, c("L","N","H"))
adlb$BNRIND_FACTOR <- factor(adlb$BNRIND, c("L","N","H"))

#no updates needed - initializes vur which is used to determine which parts of code to execute during testing
#vur <- NULL
vur <- TRUE
#if(file.exists("~/Tplyr/uat/references/output/vur_auto.Rds")) vur <- readRDS("~/Tplyr/uat/references/output/vur_auto.Rds")


#test 1 ----
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
  testthat::expect_equal(expr(TRT01P), quo_get_expr(test_1$treat_var), label = "T1.2")
  #manual check(s)

  #clean up working directory
  rm(test_1)
})

#test 2 ----
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

#test 3 ----
test_that('T3',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_total_group() %>%
      add_treat_grps('Total Xanomeline' = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
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
                         as.vector(test_3$TRT01P), label = "T3.1")
  t3_2 <- c(nrow(filter(adsl, TRT01P == "Placebo")), nrow(adsl),
              nrow(filter(adsl, TRT01P == "Xanomeline High Dose" | TRT01P == "Xanomeline Low Dose")),
              nrow(filter(adsl, TRT01P == "Xanomeline High Dose")), nrow(filter(adsl, TRT01P == "Xanomeline Low Dose")))
  testthat::expect_equal(t3_2, test_3[[2]], label = "T3.2")
  #manual check(s)

  #clean up working directory
  rm(t3_2)
  rm(test_3)
})

#test 4 ----
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
  testthat::expect_equal(expr(TRT01P), quo_get_expr(test_4$treat_var), label = "T4.2")
  #manual check(s)

  #clean up working directory
  rm(test_4)
})

#test 5 ----
test_that('T5',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    test_5 <- tplyr_table(adae, TRTA) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_layer(
        group_count(AEDECOD, by="Preferred Term", where = SAFFL == "Y")
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
  testthat::expect_equal(expr(TRT01P), quo_get_expr(test_5$pop_treat_var), label = "T5.3")
  testthat::expect_equal(expr(TRTA), quo_get_expr(test_5$treat_var), label = "T5.4")
  #manual check(s)

  #clean up working directory
  rm(test_5)
})

#test 6 ----
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

#test 7 ----
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
  rm(t7_1)
  rm(t7_2)
  rm(test_7)
})

#test 8 ----
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
  rm(t8_1)
  rm(t8_2)
  rm(test_8)
})

#test 9 ----
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
  #manual check(s)

  #clean up working directory
  rm(test_9)
})


#test 10 ----
test_that('T10',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(DCSREAS) %>%
          set_missing_count(f_str("xx", n), string = c(Missing = '')) %>%
          set_denom_ignore('')
      )

    test_10 <- build(t)

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
  t10_tots <- filter(adsl, DCSREAS != "") %>%
    group_by(TRT01P) %>%
    summarise(total = n())
  t10_1 <- group_by(adsl, TRT01P, DCSREAS) %>%
    summarise(n = n()) %>%
    left_join(t10_tots, by="TRT01P") %>%
    mutate(pct = n / total *100) %>%
    mutate(col = ifelse(DCSREAS == "", sprintf("%2s",n), paste0(sprintf("%2s",n),' (',sprintf("%5.1f",pct),"%)"))) %>%
    filter(TRT01P == "Placebo")
  testthat::expect_equal(t10_1$col,test_10$var1_Placebo,label = "T10.1")
  #clean up working directory
  rm(t10_1)
  rm(test_10)
})

#test 11 ----
test_that('T11',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE_FACTOR)
      )
    build(t)
    test_11 <- get_numeric_data(t)

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
  testthat::expect_equal(c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"),
                         unique(test_11[[1]]$summary_var),
                         label = "T11.1")
  #clean up working directory
  rm(test_11)
})

#test 12 ----
test_that('T12',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA, where=TRTA == 'Placebo') %>%
      add_layer(
        group_count(AEDECOD) %>%
          set_format_strings(f_str("xxx (xxx.x%)", n, pct))
      )%>%
      add_layer(
        group_count(AEDECOD) %>%
          set_distinct_by(USUBJID) %>%
          set_format_strings(f_str("xxx (xxx.x%)", distinct, distinct_pct))
      )%>%
      add_layer(
        group_count(AEDECOD) %>%
          set_distinct_by(USUBJID) %>%
          set_format_strings(f_str("xxx (xxx.x%) [xxx (xxx.x%)]", n, pct, distinct, distinct_pct))
      )

    test_12 <- build(t)

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
  t12_totals <- filter(adae) %>%
    group_by(TRTA) %>%
    summarize(total=n())
  t12_totals_distinct <- filter(adae) %>%
    distinct(USUBJID, TRTA) %>%
    group_by(TRTA) %>%
    summarize(distinct_total=n())

  t12_1 <- filter(adae, TRTA == 'Placebo') %>%
    group_by(AEDECOD, TRTA) %>%
    summarize(cnt=n()) %>%
    left_join(t12_totals,by="TRTA") %>%
    mutate(pct = sprintf("%5.1f", round(cnt/total*100,digits = 1))) %>%
    mutate(col = paste0(as.character(cnt),' (',pct,'%)'))

  t12_2 <- filter(adae, TRTA == 'Placebo') %>%
    distinct(USUBJID, TRTA, AEDECOD) %>%
    group_by(AEDECOD, TRTA) %>%
    summarize(cnt=n()) %>%
    left_join(t12_totals_distinct,by="TRTA") %>%
    mutate(pct = sprintf("%5.1f", round(cnt/distinct_total*100,digits = 1))) %>%
    mutate(distinct_col = paste0(as.character(cnt),' (',pct,'%)'))

  t12_3 <- select(t12_1,c("TRTA","AEDECOD","col")) %>%
    left_join(t12_2, by=c("TRTA","AEDECOD")) %>%
    mutate(col_combo = paste0(col, " [",sprintf("%12s",distinct_col),"]"))


  testthat::expect_equal(t12_1$col,
                         trimws(filter(test_12, ord_layer_index == 1)[["var1_Placebo"]]),
                         label = "T12.1")
  testthat::expect_equal(t12_2$distinct_col,
                         trimws(filter(test_12, ord_layer_index == 2)[["var1_Placebo"]]),
                         label = "T12.2")
  testthat::expect_equal(t12_3$col_combo,
                         trimws(filter(test_12, ord_layer_index == 3)[["var1_Placebo"]]),
                         label = "T12.3")
  #clean up working directory
  rm(t12_totals)
  rm(t12_totals_distinct)
  rm(t12_1)
  rm(t12_2)
  rm(t12_3)
  rm(test_12)
})

#test 13 ----
test_that('T13',{
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
    test_13 <- get_numeric_data(t)[[1]]

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
  t13_1 <- group_by(adsl, TRT01P) %>%
    summarise(total=n()) %>%
    mutate(total = as.integer(total))
  t13_2 <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    left_join(t13_1, by='TRT01P') %>%
    mutate(pct = round((n / total) * 100, digits = 1))

  testthat::expect_equal(t13_1$total,unique(test_13[c("TRT01P", "total")])$total,label = "T13.1")
  testthat::expect_equal(t13_2$pct,
                         mutate(filter(test_13, n != 0),pct = round((n / total) * 100, digits = 1))[['pct']],
                         label = "T13.2")
  #clean up working directory
  rm(t13_1)
  rm(t13_2)
  rm(test_13)
})

#test 14 ----
test_that('T14',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P, where=SEX == "F") %>%
      add_layer(
        group_count(RACE) %>%
          set_format_strings(f_str("xxx (xx.x%)", n, pct))
      )

    build(t)
    test_14 <- get_numeric_data(t)[[1]]


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
  t14_1 <- filter(adsl, SEX == "F") %>%
    group_by(TRT01P) %>%
    summarise(total=n())
  t14_2 <- filter(adsl, SEX == "F") %>%
    group_by(TRT01P, RACE) %>%
    summarise(n=n()) %>%
    left_join(t14_1, by='TRT01P') %>%
    mutate(pct = round((n / total) * 100, digits = 1))

  testthat::expect_equal(t14_1$total,unique(test_14[c("TRT01P", "total")])$total,label = "T14.1")
  testthat::expect_equal(t14_2$pct,
                         mutate(filter(test_14, n != 0),pct = round((n / total) * 100, digits = 1))[['pct']],
                         label = "T14.2")
  #clean up working directory
  rm(t14_1)
  rm(t14_2)
  rm(test_14)
})

#test 15 ----
test_that('T15',{
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
    test_15 <- merge(get_numeric_data(t)[[1]], rename(header_n(t), header_n=n),by.x = "TRTA", by.y = "TRT01P")

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
  t15_1 <- group_by(adsl, TRT01P) %>%
    summarise(total=n()) %>%
    mutate(total = as.numeric(total))
  t15_2 <- group_by(adae, TRTA, AEDECOD) %>%
    summarise(n=n()) %>%
    merge(t15_1, by.y='TRT01P', by.x = "TRTA") %>%
    mutate(pct = round((n / total) * 100, digits = 1))
  testthat::expect_equal(t15_1$total,unique(test_15[c("TRTA", "header_n")])$header_n,label = "T15.1")
  testthat::expect_equal(t15_2$pct,
                         mutate(filter(test_15, n != 0),pct = round((n / header_n) * 100, digits = 1))[['pct']],
                         label = "T15.2")
  #clean up working directory
  rm(t15_1)
  rm(t15_2)
  rm(test_15)
})

#test 16 ----
test_that('T16',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(ETHNIC, by=SEX) %>%
          set_denoms_by(TRT01P, SEX)
      )

    test_16 <- build(t)

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
  t16_tots <- group_by(adsl, TRT01P, SEX) %>%
    summarise(total=n()) %>%
    mutate(total = as.numeric(total))
  t16_1 <- group_by(adsl, TRT01P, SEX, ETHNIC) %>%
    summarise(n=n()) %>%
    left_join(t16_tots, by = c('TRT01P', "SEX")) %>%
    mutate(pct = round((n / total) * 100, digits = 1)) %>%
    mutate(col = paste0(sprintf("%2s",n),' (',sprintf("%5.1f",pct),'%)')) %>%
    filter(TRT01P == "Placebo")

  testthat::expect_equal(t16_1$col, test_16$var1_Placebo,label = "T16.1")
  #clean up working directory
  rm(t16_tots)
  rm(t16_1)
  rm(test_16)
})


#test 17 ----
test_that('T17',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder

    t <- tplyr_table(adsl, TRT01A) %>%
      add_layer(
        group_count(RACE) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'))
      )
    suppressWarnings( build(t))
    test_17 <- get_stats_data(t)[[1]]$riskdiff


    # output table to check attributes
    save(test_17, file = "~/Tplyr/uat/output/test_17.RData")

    #clean up working directory
    rm(t)
    rm(test_17)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_17.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  tot_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose"), n=n())[[1]]
  cnt_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose" & RACE == 'WHITE'), n=n())[[1]]
  tot_p <- summarise(filter(adsl, TRT01P == "Placebo"), n=n())[[1]]
  cnt_p <- summarise(filter(adsl, TRT01P == "Placebo" & RACE == 'WHITE'), n=n())[[1]]
  t17 <- prop.test(c(cnt_t, cnt_p), c(tot_t,tot_p))
  testthat::expect_equal(t17$estimate[[1]] - t17$estimate[[2]],
                         filter(test_17, summary_var == 'WHITE' & measure == 'dif')[[3]],
                         label = "T17.1")
  testthat::expect_equal(c(t17$conf.int[1], t17$conf.int[2]),
                         c(filter(test_17, summary_var == 'WHITE' & measure == 'low')[[3]],
                           filter(test_17, summary_var == 'WHITE' & measure == 'high')[[3]]),
                         label = "T17.2")

  #clean up working directory
  rm(tot_p)
  rm(cnt_p)
  rm(tot_t)
  rm(cnt_t)
  rm(t17)
  rm(test_17)
})


#test 18 ----
test_that('T18',{
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

    build(t)
    test_18 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_18, file = "~/Tplyr/uat/output/test_18.RData")

    #clean up working directory
    rm(t)
    rm(test_18)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_18.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], n=n())[[1]],
                         subset(test_18, stat == 'n' & TRT01P == 'Placebo')[['value']],
                         label = "T18.1")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], mean=mean(AGE))[[1]],
                         subset(test_18, stat == 'mean' & TRT01P == 'Placebo')[['value']],
                         label = "T18.2")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], median=median(AGE))[[1]],
                         subset(test_18, stat == 'median' & TRT01P == 'Placebo')[['value']],
                         label = "T18.3")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], sd=sd(AGE))[[1]],
                         subset(test_18, stat == 'sd' & TRT01P == 'Placebo')[['value']],
                         label = "T18.4")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], var=var(AGE))[[1]],
                         subset(test_18, stat == 'var' & TRT01P == 'Placebo')[['value']],
                         label = "T18.5")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], min=min(AGE))[[1]],
                         subset(test_18, stat == 'min' & TRT01P == 'Placebo')[['value']],
                         label = "T18.6")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], max=max(AGE))[[1]],
                         subset(test_18, stat == 'max' & TRT01P == 'Placebo')[['value']],
                         label = "T18.7")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], iqr=IQR(AGE))[[1]],
                         subset(test_18, stat == 'iqr' & TRT01P == 'Placebo')[['value']],
                         label = "T18.8")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q1=quantile(AGE)[[2]])[[1]],
                         subset(test_18, stat == 'q1' & TRT01P == 'Placebo')[['value']],
                         label = "T18.9")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q3=quantile(AGE)[[4]])[[1]],
                         subset(test_18, stat == 'q3' & TRT01P == 'Placebo')[['value']],
                         label = "T18.10")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo' & is.na(adsl$AGE),], n=n())[[1]],
                         subset(test_18, stat == 'missing' & TRT01P == 'Placebo')[['value']],
                         label = "T18.11")
  #manual check(s)


  #clean up working directory
  rm(test_18)
})


#test 19 ----
test_that('T19',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_custom_summaries(
            geometric_mean = exp(sum(log(.var[.var > 0]),
                                     na.rm=TRUE) / length(.var))
          ) %>%
          set_format_strings(
            'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
          )
      )

    build(t)
    test_19 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_19, file = "~/Tplyr/uat/output/test_19.RData")

    #clean up working directory
    rm(t)
    rm(test_19)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_19.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',],
                                   geometric_mean = exp(sum(log(AGE[AGE > 0]),na.rm=TRUE) / length(AGE)))[[1]],
                         subset(test_19, stat == 'geometric_mean' & TRT01P == 'Placebo')[['value']],
                         label = "T19.1")
  #manual check(s)


  #clean up working directory
  rm(test_19)
})


#test 20 ----
test_that('T20',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE, by=ETHNIC) %>%
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
            'q3' = f_str('xx.x', q3)
          )
      )

    build(t)
    test_20 <- filter(get_numeric_data(t)[[1]], TRT01P == 'Placebo')

    # output table to check attributes
    save(test_20, file = "~/Tplyr/uat/output/test_20.RData")

    #clean up working directory
    rm(t)
    rm(test_20)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_20.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t20_1 <- pivot_longer(data.frame(summarise(group_by(adsl[adsl$TRT01P == 'Placebo',],ETHNIC),
                                             n=n(),
                                             mean=mean(AGE),
                                             median=median(AGE),
                                             sd=sd(AGE),
                                             var=var(AGE),
                                             min=min(AGE),
                                             max=max(AGE),
                                             iqr=IQR(AGE),
                                             q1=quantile(AGE)[[2]],
                                             q3=quantile(AGE)[[4]]
                                             )
                                   ),
                        cols=c(n,mean,median,sd,var,min,max,iqr,q1,q3),names_to="STAT")

  testthat::expect_equal(t20_1$value,
                         test_20$value,
                         label = "T20.1")
  #manual check(s)


  #clean up working directory
  rm(t20_1)
  rm(test_20)
})


#test 21 ----
test_that('T21',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            'combo' = f_str('xx, xx.x, xx, x.xx, xx.xx, xx, xx, xx.x, xx.x, xx.x',
                            n,   mean, median, sd,  var,   min, max, iqr, q1,  q3)
          )
      )

    test_21 <- build(t)$var1_Placebo

    # output table to check attributes
    save(test_21, file = "~/Tplyr/uat/output/test_21.RData")

    #clean up working directory
    rm(t)
    rm(test_21)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_21.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)

  t21_1 <- paste(summarise(adsl[adsl$TRT01P == 'Placebo',],n=n())[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],mean=round(mean(AGE),1))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],median=median(AGE))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],sd=round(sd(AGE),2))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],var=round(var(AGE),2))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],min=min(AGE))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],max=max(AGE))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],iqr=round(IQR(AGE),1))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],q1=round(quantile(AGE)[[2]],1))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],q3=round(quantile(AGE)[[4]],1))[[1]],
                 sep=", ")

  testthat::expect_equal(t21_1,
                         test_21,
                         label = "T21.1")
  #manual check(s)


  #clean up working directory
  rm(t21_1)
  rm(test_21)
})

#test 22 ----
test_that('T22',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            'combo' = f_str('xx, xx.xx, xx.xx, xx.xxx, xx.xxx, xx, xx, xx.xx, xx.xx, xx.xx',
                             n,  mean, median, sd,     var,    min, max, iqr, q1,  q3)
          )
      )

    test_22 <- build(t)$var1_Placebo

    # output table to check attributes
    save(test_22, file = "~/Tplyr/uat/output/test_22.RData")

    #clean up working directory
    rm(t)
    rm(test_22)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_22.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)

  t22_1 <- paste(summarise(adsl[adsl$TRT01P == 'Placebo',],n=n())[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],mean=sprintf("%5.2f",round(mean(AGE),2)))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],median=sprintf("%5.2f",round(median(AGE),2)))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],sd=sprintf("%6.3f",round(sd(AGE),3)))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],var=sprintf("%6.3f",round(var(AGE),3)))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],min=min(AGE))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],max=max(AGE))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],iqr=sprintf("%5.2f",round(IQR(AGE),1)))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],q1=sprintf("%5.2f",round(quantile(AGE)[[2]],2)))[[1]],
                 summarise(adsl[adsl$TRT01P == 'Placebo',],q3=sprintf("%5.2f",round(quantile(AGE)[[4]],2)))[[1]],
                 sep=", ")

  testthat::expect_equal(t22_1,
                         test_22,
                         label = "T22.1")
  #manual check(s)


  #clean up working directory
  rm(t22_1)
  rm(test_22)
})

#test 23 ----
test_that('T23',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(advs, TRTA) %>%
      add_layer(
        group_desc(AVAL, by=PARAMCD) %>%
          set_format_strings(
            'combo' = f_str('xxxx, a.a+1, xx.a+1, a.a+2, xx.a+2, xxx, a, a.xx, xxx.xx, a.a+1',
                             n,    mean,  median, sd,    var,    min, max, iqr,   q1,    q3)
          )
      )

    test_23 <- build(t)

    # output table to check attributes
    save(test_23, file = "~/Tplyr/uat/output/test_23.RData")

    #clean up working directory
    rm(t)
    rm(test_23)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_23.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)

  t23_dat <- mutate(advs, avalc = as.character(AVAL)) %>%
    rowwise() %>%
    mutate(intlen = nchar(unlist(strsplit(avalc,'\\.'))[[1]])) %>%
    mutate(hasdec = as.numeric(grepl('\\.', avalc))) %>%
    mutate(declen = ifelse(hasdec > 0, nchar(unlist(strsplit(avalc,'\\.'))[[2]]), 0)) %>%
    ungroup() %>%
    group_by(PARAMCD) %>%
    mutate(intlen = max(intlen, na.rm=TRUE)) %>%
    mutate(hasdec = max(hasdec)) %>%
    mutate(declen = max(declen))

  t23_1 <- unique(t23_dat[,c("PARAMCD","intlen","declen","hasdec")]) %>%
    left_join(summarise(t23_dat[t23_dat$TRTA == 'Placebo',], n=n(), mean=mean(AVAL), median=median(AVAL), sd=sd(AVAL),
                        var=var(AVAL), min=min(AVAL), max=max(AVAL), iqr=IQR(AVAL),
                        q1=quantile(AVAL)[[2]], q3=quantile(AVAL)[[4]]), by="PARAMCD") %>%
    mutate(combo = paste(sprintf("%4s",n),
                         sprintf("%*s", (intlen + declen + 2),
                                 sprintf("%.*f",declen+1,
                                         round(mean,declen+1)
                                         )[[1]]),
                         sprintf("%*s", 2 + declen + 2,
                                 sprintf("%.*f",declen+1,
                                         round(median,declen+1)
                                         )[[1]]),
                         sprintf("%*s", intlen + declen + 3,
                                 sprintf("%.*f",declen+2,
                                         round(sd,declen+2)
                                         )[[1]]),
                         sprintf("%*s", 2 + declen + 3,
                                 sprintf("%.*f",declen+2,
                                         round(var,declen+2)
                                 )[[1]]),
                         sprintf("%*s", 3,
                                 sprintf("%.*f",0,
                                         round(min,0)
                                 )[[1]]),
                         sprintf("%*s", intlen,
                                 sprintf("%.*f",0,
                                         round(max,0)
                                 )[[1]]),
                         sprintf("%*s", intlen + 3,
                                 sprintf("%.*f",2,
                                         round(iqr,2)
                                 )[[1]]),
                         sprintf("%*s", 6,
                                 sprintf("%.*f",2,
                                         round(q1,2)
                                 )[[1]]),
                         sprintf("%*s", intlen + declen + 2,
                                 sprintf("%.*f",declen+1,
                                         round(q3,declen+1)
                                 )[[1]]),
                         sep = ", "))

  testthat::expect_equal(t23_1$combo,
                         test_23$var1_Placebo,
                         label = "T23.1")
  #manual check(s)


  #clean up working directory
  rm(t23_dat)
  rm(t23_1)
  rm(test_23)
})


#test 24 ----
test_that('T24',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            'combo' = f_str('xx, (xx.x), )xx(), x.xx%%, [xx.xx[], xx, xx, xx.x, {Q1 - xx.x}, Q3 - xx.x',
                            n,   mean, median, sd,  var,   min, max, iqr, q1,  q3)
          )
      )

    test_24 <- build(t)$var1_Placebo

    # output table to check attributes
    save(test_24, file = "~/Tplyr/uat/output/test_24.RData")

    #clean up working directory
    rm(t)
    rm(test_24)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_24.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)

  t24_1 <- paste0(summarise(adsl[adsl$TRT01P == 'Placebo',],n=n())[[1]],
                  ", (",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],mean=round(mean(AGE),1))[[1]],
                  "), )",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],median=median(AGE))[[1]],
                  "(), ",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],sd=round(sd(AGE),2))[[1]],
                  "%%, [",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],var=round(var(AGE),2))[[1]],
                  "[], ",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],min=min(AGE))[[1]],
                  ", ",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],max=max(AGE))[[1]],
                  ", ",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],iqr=round(IQR(AGE),1))[[1]],
                  ", {Q1 - ",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],q1=round(quantile(AGE)[[2]],1))[[1]],
                  "}, Q3 - ",
                  summarise(adsl[adsl$TRT01P == 'Placebo',],q3=round(quantile(AGE)[[4]],1))[[1]]
                  )

  testthat::expect_equal(t24_1,
                         test_24,
                         label = "T24.1")
  #manual check(s)


  #clean up working directory
  rm(t24_1)
  rm(test_24)
})

#test 25 ----
test_that('T25',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder

    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE, by = RACE_FACTOR) %>%
          set_format_strings(
            'n' = f_str('xx', n, empty = "NA"),
            'mean' = f_str('xx.x', mean, empty = "N/A")
          )
      )

    build(t)
    test_25 <- build(t)

    # output table to check attributes
    save(test_25, file = "~/Tplyr/uat/output/test_25.RData")

    #clean up working directory
    rm(t)
    rm(test_25)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_25.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t25_1 <- group_by(adsl, TRT01P, RACE_FACTOR) %>%
    summarise(n=n(), mean = round(mean(AGE),1)) %>%
    ungroup() %>%
    complete(TRT01P, RACE_FACTOR, fill=list(n="NA",mean="N/A")) %>%
    filter(TRT01P == "Placebo") %>%
    pivot_longer(cols=c(n,mean))
  testthat::expect_equal(t25_1$value, trimws(test_25$var1_Placebo),label = "T25.1")
  #manual check(s)

  #clean up working directory
  rm(t25_1)
  rm(test_25)
})

#test 26 ----
test_that('T26',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND))
      )
    build(t)
    test_26 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_26, file = "~/Tplyr/uat/output/test_26.RData")

    #clean up working directory
    rm(t)
    rm(test_26)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_26.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t26_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0))
  testthat::expect_equal(t26_1$n,test_26$n,label = "T26.1")
  #manual check(s)

  #clean up working directory
  rm(t26_1)
  rm(test_26)
})


#test 27 ----
test_that('T27',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND), by=SEX)
      )
    build(t)
    test_27 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_27, file = "~/Tplyr/uat/output/test_27.RData")

    #clean up working directory
    rm(t)
    rm(test_27)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_27.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t27_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, SEX, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, SEX, ANRIND, BNRIND, fill=list(n = 0))
  testthat::expect_equal(t27_1$n,test_27$n,label = "T27.1")
  #manual check(s)

  #clean up working directory
  rm(t27_1)
  rm(test_27)
})

#test 28 ----
test_that('T28',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND), by=vars(RACE, SEX))
      )
    build(t)
    test_28 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_28, file = "~/Tplyr/uat/output/test_28.RData")

    #clean up working directory
    rm(t)
    rm(test_28)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_28.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t28_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, RACE, SEX, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, RACE, SEX, ANRIND, BNRIND, fill=list(n = 0))
  testthat::expect_equal(t28_1$n,test_28$n,label = "T27.1")
  #manual check(s)

  #clean up working directory
  rm(t28_1)
  rm(test_28)
})

#test 29 ----
test_that('T29',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND_FACTOR, column=BNRIND_FACTOR))
      )
    build(t)
    test_29 <- get_numeric_data(t)[[1]]

    # output table to check attributes
    save(test_29, file = "~/Tplyr/uat/output/test_29.RData")

    #clean up working directory
    rm(t)
    rm(test_29)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_29.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t29_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND_FACTOR, BNRIND_FACTOR) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND_FACTOR, BNRIND_FACTOR, fill=list(n = 0))
  testthat::expect_equal(t29_1$n,test_29$n,label = "T29.1")
  #manual check(s)

  #clean up working directory
  rm(t29_1)
  rm(test_29)
})

#test 30 ----
test_that('T30',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND)) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct))
      )

    test_30 <- build(t)

    # output table to check attributes
    save(test_30, file = "~/Tplyr/uat/output/test_30.RData")

    #clean up working directory
    rm(t)
    rm(test_30)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_30.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t30_tots <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, BNRIND) %>%
    summarise(total=n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, fill=list(total = 0))
  t30_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t30_tots, by=c("TRTA","BNRIND")) %>%
    mutate(pct = ifelse(total > 0, n / total * 100,0)) %>%
    mutate(col =paste0(sprintf("%3s",n),' (',sprintf("%5.1f",pct),'%)')) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t30_1$col,test_30$var1_Placebo_N,label = "T30.1")
  #manual check(s)

  #clean up working directory
  rm(t30_tots)
  rm(t30_1)
  rm(test_30)
})


#test 31 ----
test_that('T31',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND)) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct))
      )

    test_31 <- build(t)

    # output table to check attributes
    save(test_31, file = "~/Tplyr/uat/output/test_31.RData")

    #clean up working directory
    rm(t)
    rm(test_31)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_31.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t31_tots <- filter(adlb) %>%
    group_by(TRTA) %>%
    summarise(total=n())
  t31_1 <- filter(adlb) %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t31_tots, by="TRTA") %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%3s",n)," (",sprintf("%5.1f",pct),"%)")) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t31_1$col,test_31$var1_Placebo_N,label = "T31.1")
  #manual check(s)

  #clean up working directory
  rm(t31_tots)
  rm(t31_1)
  rm(test_31)
})


#test 32 ----
test_that('T32',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND)) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct))
      )

    test_32 <- build(t)

    # output table to check attributes
    save(test_32, file = "~/Tplyr/uat/output/test_32.RData")

    #clean up working directory
    rm(t)
    rm(test_32)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_32.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t32_tots <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA) %>%
    summarise(total=n())
  t32_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t32_tots, by="TRTA") %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%3s",n)," (",sprintf("%5.1f",pct),"%)")) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t32_1$col,test_32$var1_Placebo_N,label = "T32.1")
  #manual check(s)

  #clean up working directory
  rm(t32_tots)
  rm(t32_1)
  rm(test_32)
})


#test 33 ----
test_that('T33',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND),
                    where=(PARAMCD == "BILI" & AVISIT == "Week 2")) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct))
      )

    test_33 <- build(t)

    # output table to check attributes
    save(test_33, file = "~/Tplyr/uat/output/test_33.RData")

    #clean up working directory
    rm(t)
    rm(test_33)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_33.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t33_tots <- group_by(adsl, TRT01P)%>%
    summarise(total=n())
  t33_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    merge(t33_tots, by.x="TRTA", by.y="TRT01P") %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%3s",n)," (",sprintf("%5.1f",pct),"%)")) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t33_1$col,test_33$var1_Placebo_N,label = "T33.1")
  #manual check(s)

  #clean up working directory
  rm(t33_tots)
  rm(t33_1)
  rm(test_33)
})


#test 34 ----
test_that('T34',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND), by=vars(PARAMCD, AVISIT)) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct)) %>%
          set_denoms_by(TRTA, PARAMCD, AVISIT)
      )

    test_34 <- build(t)

    # output table to check attributes
    save(test_34, file = "~/Tplyr/uat/output/test_34.RData")

    #clean up working directory
    rm(t)
    rm(test_34)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_34.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t34_tots <- filter(adlb) %>%
    group_by(TRTA, PARAMCD, AVISIT) %>%
    summarise(total=n())
  t34_1 <- filter(adlb) %>%
    group_by(TRTA, PARAMCD, AVISIT, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, PARAMCD, AVISIT, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t34_tots, by=c("TRTA", "PARAMCD", "AVISIT")) %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%3s",n)," (",sprintf("%5.1f",pct),"%)")) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t34_1$col,test_34$var1_Placebo_N,label = "T34.1")
  #manual check(s)

  #clean up working directory
  rm(t34_tots)
  rm(t34_1)
  rm(test_34)
})


#test 35 ----
test_that('T35',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE, by = "Race")
      )

    test_35 <- build(t)

    # output table to check attributes
    save(test_35, file = "~/Tplyr/uat/output/test_35.RData")

    #clean up working directory
    rm(t)
    rm(test_35)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_35.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(replicate(n = length(unique(adsl$RACE)), "Race", simplify = TRUE ),
                         test_35$row_label1,
                         label = "T35.1")
  #manual check(s)

  #clean up working directory
  rm(test_35)
})


#test 36 ----
test_that('T36',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(vars(ETHNIC, RACE), by = vars("Ethnicity","Race"))
      )

    test_36 <- build(t)

    # output table to check attributes
    save(test_36, file = "~/Tplyr/uat/output/test_36.RData")

    #clean up working directory
    rm(t)
    rm(test_36)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_36.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(,
                         list(test_36$row_label1, test_36$row_label2),
                         label = "T36.1")
  #manual check(s)

  #clean up working directory
  rm(test_36)
})


#test 37 ----
test_that('T37',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE)
      ) %>%
      add_layer(
        group_desc(AGE)
      ) %>%
      add_layer(
        group_desc(CUMDOSE)
      ) %>%
      add_layer(
        group_count(ETHNIC)
      )

    test_37 <- build(t)

    # output table to check attributes
    save(test_37, file = "~/Tplyr/uat/output/test_37.RData")

    #clean up working directory
    rm(t)
    rm(test_37)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_37.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t37_denoms <- filter(adsl, TRT01P == "Placebo") %>%
    group_by(TRT01P) %>%
    summarise(total = n())

  t37_race <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE, fill=list(n = 0)) %>%
    filter(TRT01P == "Placebo") %>%
    left_join(t37_denoms, by="TRT01P") %>%
    mutate(pct = n / total *100) %>%
    mutate(col = paste0(sprintf("%2s", n)," (",sprintf("%5.1f",pct),"%) ")) %>%
    mutate(label = RACE) %>%
    select(label, col)

  t37_ethnic <- group_by(adsl, TRT01P, ETHNIC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, ETHNIC, fill=list(n = 0)) %>%
    filter(TRT01P == "Placebo") %>%
    left_join(t37_denoms, by="TRT01P") %>%
    mutate(pct = n / total *100) %>%
    mutate(col = paste0(sprintf("%2s", n)," (",sprintf("%5.1f",pct),"%) ")) %>%
    mutate(label = ETHNIC) %>%
    select(label, col)

  t37_age <- filter(adsl, TRT01P == "Placebo") %>%
    summarise(n=n(),
              mean=mean(AGE),
              median=median(AGE),
              sd=sd(AGE),
              min=min(AGE),
              max=max(AGE),
              q1=quantile(AGE)[[2]],
              q3=quantile(AGE)[[4]]) %>%
    mutate(col_n = sprintf("%2s", n)) %>%
    mutate(col_meansd = paste0(sprintf("%4.1f", mean)," (",sprintf("%5.2f", sd),")")) %>%
    mutate(col_median = sprintf("%4.1f", median)) %>%
    mutate(col_q1q3 = paste0(sprintf("%2.0f", q1),", ",sprintf("%2.0f", q3))) %>%
    mutate(col_minmax = paste0(sprintf("%2.0f", min),", ",sprintf("%2.0f", max))) %>%
    pivot_longer(cols = c(col_n,col_meansd,col_median,col_q1q3,col_minmax),
                 names_to = "label", values_to = "col") %>%
    select(label, col)

  t37_cumdose <- filter(adsl, TRT01P == "Placebo") %>%
    summarise(n=n(),
              mean=mean(CUMDOSE),
              median=median(CUMDOSE),
              sd=sd(CUMDOSE),
              min=min(CUMDOSE),
              max=max(CUMDOSE),
              q1=quantile(CUMDOSE)[[2]],
              q3=quantile(CUMDOSE)[[4]]) %>%
    mutate(col_n = sprintf("%2s", n)) %>%
    mutate(col_meansd = paste0(sprintf("%4.1f", mean)," (",sprintf("%5.2f", sd),")")) %>%
    mutate(col_median = sprintf("%4.1f", median)) %>%
    mutate(col_q1q3 = paste0(sprintf("%2.0f", q1),", ",sprintf("%2.0f", q3))) %>%
    mutate(col_minmax = paste0(sprintf("%2.0f", min),", ",sprintf("%2.0f", max))) %>%
    pivot_longer(cols = c(col_n,col_meansd,col_median,col_q1q3,col_minmax),
                 names_to = "label", values_to = "col") %>%
    select(label, col)

  t37_1 <- rbind(t37_race, t37_age, t37_cumdose, t37_ethnic)

  testthat::expect_equal(t37_1$col, filter(test_37, row_label1 != 'Missing')$var1_Placebo,label = "T37.1")
  #manual check(s)

  #clean up working directory
  rm(t37_denoms)
  rm(t37_race)
  rm(t37_ethnic)
  rm(t37_age)
  rm(t37_cumdose)
  rm(t37_1)
  rm(test_37)
})

#clean up ----
rm(vur)
