context("Atorus Validation")

#' @title Test Cases Code
#' @section Last Updated By:
#' Nathan Kosiba
#' @section Last Update Date:
#' 8/31/2020

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

opts = options()

#no updates needed - initializes vur which is used to determine which parts of code to execute during testing
vur <- NULL
if(file.exists("~/Tplyr/uat/references/output/vur_auto.Rds")) vur <- readRDS("~/Tplyr/uat/references/output/vur_auto.Rds")


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
        group_count(AEDECOD)
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
  #manual check(s)

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
  #manual check(s)

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
  #manual check(s)

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
  #manual check(s)

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
  #manual check(s)

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

    test_15 <- list(build(t), header_n(t))

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
    summarise(total=n())
  t15_2 <- group_by(adae, TRTA, AEDECOD) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, AEDECOD, fill = list(n = 0)) %>%
    merge(t15_1, by.y='TRT01P', by.x = "TRTA") %>%
    mutate(pct = round((n / total) * 100, digits = 1)) %>%
    mutate(col = paste0(sprintf("%3s",n),' (',sprintf("%4.1f",pct),'%)')) %>%
    select(TRTA, AEDECOD, col) %>%
    pivot_wider(names_from = "TRTA", values_from = col)
  testthat::expect_equal(t15_2$Placebo,test_15[[1]]$var1_Placebo, label = "T15.1")
  testthat::expect_equal(t15_1$total,test_15[[2]]$n, label = "T15.2")
  #manual check(s)

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
  #manual check(s)

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
      ) %>%
      add_layer(
        group_count(RACE) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'),
                        args = list(conf.level = 0.9, correct=FALSE, alternative='less'))
      )

    suppressWarnings(build(t))
    test_17 <- get_stats_data(t)


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
  t17_noarg <- prop.test(c(cnt_t, cnt_p), c(tot_t,tot_p))
  t17_args <- prop.test(c(cnt_t, cnt_p), c(tot_t,tot_p), conf.level = 0.9, correct=FALSE, alternative='less')
  testthat::expect_equal(t17_noarg$estimate[[1]] - t17_noarg$estimate[[2]],
                         filter(test_17[[1]]$riskdiff, summary_var == 'WHITE' & measure == 'dif')[[3]],
                         label = "T17.1")
  testthat::expect_equal(c(t17_noarg$conf.int[1], t17_noarg$conf.int[2]),
                         c(filter(test_17[[1]]$riskdiff, summary_var == 'WHITE' & measure == 'low')[[3]],
                           filter(test_17[[1]]$riskdiff, summary_var == 'WHITE' & measure == 'high')[[3]]),
                         label = "T17.2")
  testthat::expect_equal(c(t17_args$estimate[[1]] - t17_args$estimate[[2]], t17_args$conf.int[1], t17_args$conf.int[2]),
                         c(filter(test_17[[2]]$riskdiff, summary_var == 'WHITE' & measure == 'dif')[[3]],
                           filter(test_17[[2]]$riskdiff, summary_var == 'WHITE' & measure == 'low')[[3]],
                           filter(test_17[[2]]$riskdiff, summary_var == 'WHITE' & measure == 'high')[[3]]),
                         label = "T17.3")
  #manual check(s)

  #clean up working directory
  rm(tot_p)
  rm(cnt_p)
  rm(tot_t)
  rm(cnt_t)
  rm(t17_noarg)
  rm(t17_args)
  rm(test_17)
})


#test 18 ----
test_that('T18',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01A, cols=SEX) %>%
      add_layer(
        group_count(RACE) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'))
      )
    suppressWarnings(build(t))
    test_18 <- get_stats_data(t)


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
  tot_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose" & SEX == "F"), n=n())[[1]]
  cnt_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose" & RACE == 'WHITE' & SEX == "F"), n=n())[[1]]
  tot_p <- summarise(filter(adsl, TRT01P == "Placebo" & SEX == "F"), n=n())[[1]]
  cnt_p <- summarise(filter(adsl, TRT01P == "Placebo" & RACE == 'WHITE' & SEX == "F"), n=n())[[1]]
  suppressWarnings(t18 <- prop.test(c(cnt_t, cnt_p), c(tot_t,tot_p)))

  testthat::expect_equal(c(t18$estimate[[1]] - t18$estimate[[2]], t18$conf.int[1], t18$conf.int[2]),
                         c(filter(test_18[[1]]$riskdiff, summary_var == 'WHITE' & SEX == "F" & measure == 'dif')[[4]],
                           filter(test_18[[1]]$riskdiff, summary_var == 'WHITE' & SEX == "F" & measure == 'low')[[4]],
                           filter(test_18[[1]]$riskdiff, summary_var == 'WHITE' & SEX == "F" & measure == 'high')[[4]]),
                         label = "T18.1")
  #manual check(s)

  #clean up working directory
  rm(tot_p)
  rm(cnt_p)
  rm(tot_t)
  rm(cnt_t)
  rm(t18)
  rm(test_18)
})


#test 19 ----
test_that('T19',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(vars(AEBODSYS, AEDECOD)) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'))
      ) %>%
      add_layer(
        group_count(AEBODSYS, by = SEX) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'))
      ) %>%
      add_layer(
        group_count(vars(AEBODSYS, AEDECOD), by = SEX) %>%
          add_risk_diff(c('Xanomeline High Dose','Placebo'))
      )
    suppressWarnings(build(t))
    test_19 <- get_stats_data(t)

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
  tot_p <- summarise(filter(adae, TRTA == "Placebo"), n=n())[[1]]
  tot_t <- summarise(filter(adae, TRTA == 'Xanomeline High Dose'), n=n())[[1]]
  cnt_p1 <- summarise(filter(adae, TRTA == "Placebo" &
                               AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                               AEDECOD == "PRURITUS"),
                      n=n())[[1]]
  cnt_t1 <- summarise(filter(adae, TRTA == 'Xanomeline High Dose' &
                               AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                               AEDECOD == "PRURITUS"),
                      n=n())[[1]]
  cnt_p2 <- summarise(filter(adae, TRTA == "Placebo" &
                               AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                               SEX == "F"),
                      n=n())[[1]]
  cnt_t2 <- summarise(filter(adae, TRTA == 'Xanomeline High Dose' &
                               AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                               SEX == "F"),
                      n=n())[[1]]
  cnt_p3 <- summarise(filter(adae, TRTA == "Placebo" &
                               AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                               AEDECOD == "PRURITUS" &
                               SEX == "F"),
                      n=n())[[1]]
  cnt_t3 <- summarise(filter(adae, TRTA == 'Xanomeline High Dose' &
                               AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                               AEDECOD == "PRURITUS" &
                               SEX == "F"),
                      n=n())[[1]]

  suppressWarnings(t19_1 <- prop.test(c(cnt_t1, cnt_p1), c(tot_t, tot_p)))
  suppressWarnings(t19_2 <- prop.test(c(cnt_t2, cnt_p2), c(tot_t, tot_p)))
  suppressWarnings(t19_3 <- prop.test(c(cnt_t3, cnt_p3), c(tot_t, tot_p)))

  testthat::expect_equal(c(t19_1$estimate[[1]] - t19_1$estimate[[2]], t19_1$conf.int[1], t19_1$conf.int[2]),
                         c(filter(test_19[[1]]$riskdiff, AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    summary_var == '   PRURITUS' & measure == 'dif')[[4]],
                           filter(test_19[[1]]$riskdiff, AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    summary_var == '   PRURITUS' & measure == 'low')[[4]],
                           filter(test_19[[1]]$riskdiff, AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    summary_var == '   PRURITUS' & measure == 'high')[[4]]),
                         label = "T19.1")
  testthat::expect_equal(c(t19_2$estimate[[1]] - t19_2$estimate[[2]], t19_2$conf.int[1], t19_2$conf.int[2]),
                         c(filter(test_19[[2]]$riskdiff, summary_var == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    SEX == 'F' & measure == 'dif')[[4]],
                           filter(test_19[[2]]$riskdiff, summary_var == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    SEX == 'F' & measure == 'low')[[4]],
                           filter(test_19[[2]]$riskdiff, summary_var == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    SEX == 'F' & measure == 'high')[[4]]),
                         label = "T19.2")
  testthat::expect_equal(c(t19_3$estimate[[1]] - t19_3$estimate[[2]], t19_3$conf.int[1], t19_3$conf.int[2]),
                         c(filter(test_19[[3]]$riskdiff, AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    summary_var == '   PRURITUS' & SEX == 'F' & measure == 'dif')[[5]],
                           filter(test_19[[3]]$riskdiff, AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    summary_var == '   PRURITUS' & SEX == 'F' & measure == 'low')[[5]],
                           filter(test_19[[3]]$riskdiff, AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
                                    summary_var == '   PRURITUS' & SEX == 'F' & measure == 'high')[[5]]),
                         label = "T19.3")
  #manual check(s)

  #clean up working directory
  rm(tot_p)
  rm(tot_t)
  rm(cnt_p1)
  rm(cnt_t1)
  rm(cnt_p2)
  rm(cnt_t2)
  rm(cnt_p3)
  rm(cnt_t3)
  rm(t19_1)
  rm(t19_2)
  rm(t19_3)
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
    test_20 <- get_numeric_data(t)[[1]]

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
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], n=n())[[1]],
                         subset(test_20, stat == 'n' & TRT01P == 'Placebo')[['value']],
                         label = "T20.1")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], mean=mean(AGE))[[1]],
                         subset(test_20, stat == 'mean' & TRT01P == 'Placebo')[['value']],
                         label = "T20.2")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], median=median(AGE))[[1]],
                         subset(test_20, stat == 'median' & TRT01P == 'Placebo')[['value']],
                         label = "T20.3")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], sd=sd(AGE))[[1]],
                         subset(test_20, stat == 'sd' & TRT01P == 'Placebo')[['value']],
                         label = "T20.4")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], var=var(AGE))[[1]],
                         subset(test_20, stat == 'var' & TRT01P == 'Placebo')[['value']],
                         label = "T20.5")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], min=min(AGE))[[1]],
                         subset(test_20, stat == 'min' & TRT01P == 'Placebo')[['value']],
                         label = "T20.6")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], max=max(AGE))[[1]],
                         subset(test_20, stat == 'max' & TRT01P == 'Placebo')[['value']],
                         label = "T20.7")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], iqr=IQR(AGE))[[1]],
                         subset(test_20, stat == 'iqr' & TRT01P == 'Placebo')[['value']],
                         label = "T20.8")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q1=quantile(AGE)[[2]])[[1]],
                         subset(test_20, stat == 'q1' & TRT01P == 'Placebo')[['value']],
                         label = "T20.9")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q3=quantile(AGE)[[4]])[[1]],
                         subset(test_20, stat == 'q3' & TRT01P == 'Placebo')[['value']],
                         label = "T20.10")
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo' & is.na(adsl$AGE),], n=n())[[1]],
                         subset(test_20, stat == 'missing' & TRT01P == 'Placebo')[['value']],
                         label = "T20.11")
  #manual check(s)

  #clean up working directory
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
          set_custom_summaries(
            geometric_mean = exp(sum(log(.var[.var > 0]),
                                     na.rm=TRUE) / length(.var))
          ) %>%
          set_format_strings(
            'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
          )
      )

    build(t)
    test_21 <- get_numeric_data(t)[[1]]

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
  testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',],
                                   geometric_mean = exp(sum(log(AGE[AGE > 0]),na.rm=TRUE) / length(AGE)))[[1]],
                         subset(test_21, stat == 'geometric_mean' & TRT01P == 'Placebo')[['value']],
                         label = "T21.1")
  #manual check(s)

  #clean up working directory
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
    test_22 <- filter(get_numeric_data(t)[[1]], TRT01P == 'Placebo')

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
  t22_1 <- pivot_longer(data.frame(summarise(group_by(adsl[adsl$TRT01P == 'Placebo',],ETHNIC),
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

  testthat::expect_equal(t22_1$value,
                         test_22$value,
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
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            'combo' = f_str('xx, xx.x, xx, x.xx, xx.xx, xx, xx, xx.x, xx.x, xx.x',
                            n,   mean, median, sd,  var,   min, max, iqr, q1,  q3)
          )
      )

    test_23 <- build(t)$var1_Placebo

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
  t23_1 <- paste(summarise(adsl[adsl$TRT01P == 'Placebo',],n=n())[[1]],
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

  testthat::expect_equal(t23_1,
                         test_23,
                         label = "T23.1")
  #manual check(s)

  #clean up working directory
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
            'combo' = f_str('xx, xx.xx, xx.xx, xx.xxx, xx.xxx, xx, xx, xx.xx, xx.xx, xx.xx',
                             n,  mean, median, sd,     var,    min, max, iqr, q1,  q3)
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
  t24_1 <- paste(summarise(adsl[adsl$TRT01P == 'Placebo',],n=n())[[1]],
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
    t <- tplyr_table(advs, TRTA) %>%
      add_layer(
        group_desc(AVAL, by=PARAMCD) %>%
          set_format_strings(
            'combo' = f_str('xxxx, a.a+1, xx.a+1, a.a+2, xx.a+2, xxx, a, a.xx, xxx.xx, a.a+1',
                             n,    mean,  median, sd,    var,    min, max, iqr,   q1,    q3)
          )
      )

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
  t25_dat <- mutate(advs, avalc = as.character(AVAL)) %>%
    rowwise() %>%
    mutate(intlen = nchar(unlist(strsplit(avalc,'\\.'))[[1]])) %>%
    mutate(hasdec = as.numeric(grepl('\\.', avalc))) %>%
    mutate(declen = ifelse(hasdec > 0, nchar(unlist(strsplit(avalc,'\\.'))[[2]]), 0)) %>%
    ungroup() %>%
    group_by(PARAMCD) %>%
    mutate(intlen = max(intlen, na.rm=TRUE)) %>%
    mutate(hasdec = max(hasdec)) %>%
    mutate(declen = max(declen))

  t25_1 <- unique(t25_dat[,c("PARAMCD","intlen","declen","hasdec")]) %>%
    left_join(summarise(t25_dat[t25_dat$TRTA == 'Placebo',], n=n(), mean=mean(AVAL), median=median(AVAL), sd=sd(AVAL),
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

  testthat::expect_equal(t25_1$combo,
                         test_25$var1_Placebo,
                         label = "T25.1")
  #manual check(s)

  #clean up working directory
  rm(t25_dat)
  rm(t25_1)
  rm(test_25)
})


#test 26 ----
test_that('T26',{
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

    test_26 <- build(t)$var1_Placebo

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
  t26_1 <- paste0(summarise(adsl[adsl$TRT01P == 'Placebo',],n=n())[[1]],
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

  testthat::expect_equal(t26_1,
                         test_26,
                         label = "T26.1")
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
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE, by = RACE_FACTOR) %>%
          set_format_strings(
            'n' = f_str('xx', n, empty = "NA"),
            'mean' = f_str('xx.x', mean, empty = "N/A")
          )
      )
    build(t)
    test_27 <- build(t)

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
  t27_1 <- group_by(adsl, TRT01P, RACE_FACTOR) %>%
    summarise(n=n(), mean = round(mean(AGE),1)) %>%
    ungroup() %>%
    complete(TRT01P, RACE_FACTOR, fill=list(n="NA",mean="N/A")) %>%
    filter(TRT01P == "Placebo") %>%
    pivot_longer(cols=c(n,mean))
  testthat::expect_equal(t27_1$value, trimws(test_27$var1_Placebo),label = "T27.1")
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
        group_shift(vars(row=ANRIND, column=BNRIND))
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
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0))
  testthat::expect_equal(t28_1$n,test_28$n,label = "T28.1")
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
        group_shift(vars(row=ANRIND, column=BNRIND), by=SEX)
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
    group_by(TRTA, SEX, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, SEX, ANRIND, BNRIND, fill=list(n = 0))
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
        group_shift(vars(row=ANRIND, column=BNRIND), by=vars(RACE, SEX))
      )
    build(t)
    test_30 <- get_numeric_data(t)[[1]]

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
  t30_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, RACE, SEX, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, RACE, SEX, ANRIND, BNRIND, fill=list(n = 0))
  testthat::expect_equal(t30_1$n,test_30$n,label = "T30.1")
  #manual check(s)

  #clean up working directory
  rm(t30_1)
  rm(test_30)
})

#test 31 ----
test_that('T31',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND_FACTOR, column=BNRIND_FACTOR))
      )
    build(t)
    test_31 <- get_numeric_data(t)[[1]]

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
  t31_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND_FACTOR, BNRIND_FACTOR) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND_FACTOR, BNRIND_FACTOR, fill=list(n = 0))
  testthat::expect_equal(t31_1$n,test_31$n,label = "T31.1")
  #manual check(s)

  #clean up working directory
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
    group_by(TRTA, BNRIND) %>%
    summarise(total=n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, fill=list(total = 0))
  t32_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t32_tots, by=c("TRTA","BNRIND")) %>%
    mutate(pct = ifelse(total > 0, n / total * 100,0)) %>%
    mutate(col =paste0(sprintf("%3s",n),' (',sprintf("%5.1f",pct),'%)')) %>%
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
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND)) %>%
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
  t33_tots <- filter(adlb) %>%
    group_by(TRTA) %>%
    summarise(total=n())
  t33_1 <- filter(adlb) %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t33_tots, by="TRTA") %>%
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
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND)) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct))
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
  t34_tots <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA) %>%
    summarise(total=n())
  t34_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t34_tots, by="TRTA") %>%
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
    t <- tplyr_table(adlb, TRTA) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND),
                    where=(PARAMCD == "BILI" & AVISIT == "Week 2")) %>%
          set_format_strings(f_str("xxx (xxx.x%)",n,pct))
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
  t35_tots <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2") %>%
    select(TRTA, USUBJID) %>%
    distinct(TRTA, USUBJID) %>%
    merge(adsl, by.x=c("USUBJID", "TRTA"), by.y=c("USUBJID", "TRT01P"), all.y = FALSE) %>%
    group_by(TRTA) %>%
    summarise(total=n())
  t35_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, ANRIND, BNRIND, fill=list(n = 0)) %>%
    merge(t35_tots, by.x="TRTA", by.y="TRTA") %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%3s",n)," (",sprintf("%5.1f",pct),"%)")) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t35_1$col,test_35$var1_Placebo_N,label = "T35.1")
  #manual check(s)

  #clean up working directory
  rm(t35_tots)
  rm(t35_1)
  rm(test_35)
})


#test 36 ----
test_that('T36',{
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
  t36_tots <- filter(adlb) %>%
    group_by(TRTA, PARAMCD, AVISIT) %>%
    summarise(total=n())
  t36_1 <- filter(adlb) %>%
    group_by(TRTA, PARAMCD, AVISIT, ANRIND, BNRIND) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, PARAMCD, AVISIT, ANRIND, BNRIND, fill=list(n = 0)) %>%
    left_join(t36_tots, by=c("TRTA", "PARAMCD", "AVISIT")) %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%3s",n)," (",sprintf("%5.1f",pct),"%)")) %>%
    filter(TRTA == "Placebo" & BNRIND == "N")
  testthat::expect_equal(t36_1$col,test_36$var1_Placebo_N,label = "T36.1")
  #manual check(s)

  #clean up working directory
  rm(t36_tots)
  rm(t36_1)
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
        group_count(RACE, by = "Race")
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
  testthat::expect_equal(replicate(n = length(unique(adsl$RACE)), "Race", simplify = TRUE ),
                         test_37$row_label1,
                         label = "T37.1")
  #manual check(s)

  #clean up working directory
  rm(test_37)
})


#test 38 ----
test_that('T38',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE, by = vars("Ethnicity", ETHNIC, "Race"))
      )

    test_38 <- build(t)

    # output table to check attributes
    save(test_38, file = "~/Tplyr/uat/output/test_38.RData")

    #clean up working directory
    rm(t)
    rm(test_38)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_38.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t38_1 <- distinct(adsl, ETHNIC, RACE) %>%
    complete(ETHNIC, RACE) %>%
    mutate(ethnic_text = "Ethnicity") %>%
    mutate(race_text = "Race")
  testthat::expect_equal(c(t38_1$ethnic_text, t38_1$ETHNIC, t38_1$race_text, t38_1$RACE),
                         c(test_38$row_label1, test_38$row_label2, test_38$row_label3, test_38$row_label4),
                         label = "T38.1")
  #manual check(s)

  #clean up working directory
  rm(t38_1)
  rm(test_38)
})


#test 39 ----
test_that('T39',{
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

    test_39 <- build(t)

    # output table to check attributes
    save(test_39, file = "~/Tplyr/uat/output/test_39.RData")

    #clean up working directory
    rm(t)
    rm(test_39)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_39.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t39_denoms <- filter(adsl, TRT01P == "Placebo") %>%
    group_by(TRT01P) %>%
    summarise(total = n())

  t39_race <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE, fill=list(n = 0)) %>%
    filter(TRT01P == "Placebo") %>%
    left_join(t39_denoms, by="TRT01P") %>%
    mutate(pct = n / total *100) %>%
    mutate(col = paste0(sprintf("%2s", n)," (",sprintf("%5.1f",pct),"%)")) %>%
    mutate(label = RACE) %>%
    select(label, col)

  t39_ethnic <- group_by(adsl, TRT01P, ETHNIC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, ETHNIC, fill=list(n = 0)) %>%
    filter(TRT01P == "Placebo") %>%
    left_join(t39_denoms, by="TRT01P") %>%
    mutate(pct = n / total *100) %>%
    mutate(col = paste0(sprintf("%2s", n)," (",sprintf("%5.1f",pct),"%)")) %>%
    mutate(label = ETHNIC) %>%
    select(label, col)

  t39_age <- filter(adsl, TRT01P == "Placebo") %>%
    summarise(n=n(),
              mean=mean(AGE),
              median=median(AGE),
              sd=sd(AGE),
              min=min(AGE),
              max=max(AGE),
              q1=quantile(AGE)[[2]],
              q3=quantile(AGE)[[4]]) %>%
    mutate(col_n = sprintf("%3s", n)) %>%
    mutate(col_meansd = paste0(sprintf("%4.1f", mean)," (",sprintf("%5.2f", sd),")")) %>%
    mutate(col_median = sprintf("%4.1f", median)) %>%
    mutate(col_q1q3 = paste0(sprintf("%4.1f", q1),", ",sprintf("%4.1f", q3))) %>%
    mutate(col_minmax = paste0(sprintf("%2.0f", min),", ",sprintf("%2.0f", max))) %>%
    pivot_longer(cols = c(col_n,col_meansd,col_median,col_q1q3,col_minmax),
                 names_to = "label", values_to = "col") %>%
    select(label, col)

  t39_cumdose <- filter(adsl, TRT01P == "Placebo") %>%
    summarise(n=n(),
              mean=mean(CUMDOSE),
              median=median(CUMDOSE),
              sd=sd(CUMDOSE),
              min=min(CUMDOSE),
              max=max(CUMDOSE),
              q1=quantile(CUMDOSE)[[2]],
              q3=quantile(CUMDOSE)[[4]]) %>%
    mutate(col_n = sprintf("%3s", n)) %>%
    mutate(col_meansd = paste0(sprintf("%7.1f", mean)," (",sprintf("%8.2f", sd),")")) %>%
    mutate(col_median = sprintf("%7.1f", median)) %>%
    mutate(col_q1q3 = paste0(sprintf("%7.1f", q1),", ",sprintf("%7.1f", q3))) %>%
    mutate(col_minmax = paste0(sprintf("%5.0f", min),", ",sprintf("%5.0f", max))) %>%
    pivot_longer(cols = c(col_n,col_meansd,col_median,col_q1q3,col_minmax),
                 names_to = "label", values_to = "col") %>%
    select(label, col)

  t39_1 <- rbind(t39_race, t39_age, t39_cumdose, t39_ethnic)

  testthat::expect_equal(t39_1$col, filter(test_39, row_label1 != 'Missing')$var1_Placebo,label = "T39.1")
  #manual check(s)

  #clean up working directory
  rm(t39_denoms)
  rm(t39_race)
  rm(t39_ethnic)
  rm(t39_age)
  rm(t39_cumdose)
  rm(t39_1)
  rm(test_39)
})


#test 40 ----
test_that('T40',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE_FACTOR) %>%
          set_order_count_method("byfactor")
      )

    test_40 <- build(t) %>%
      arrange(ord_layer_index, ord_layer_1)

    # output table to check attributes
    save(test_40, file = "~/Tplyr/uat/output/test_40.RData")

    #clean up working directory
    rm(t)
    rm(test_40)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_40.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(c(c("WHITE", "BLACK OR AFRICAN AMERICAN","AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"),
                           c(1, 2, 3, 4)),
                         c(test_40$row_label1, test_40$ord_layer_1),
                         label = "T40.1")
  #manual check(s)

  #clean up working directory
  rm(test_40)
})


#test 41 ----
test_that('T41',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE) %>%
          set_order_count_method("bycount") %>%
          set_ordering_cols("Xanomeline High Dose")
      )

    test_41 <- build(t) %>%
      arrange(ord_layer_index, desc(ord_layer_1))

    # output table to check attributes
    save(test_41, file = "~/Tplyr/uat/output/test_41.RData")

    #clean up working directory
    rm(t)
    rm(test_41)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_41.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t41_1 <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE, fill = list(n=0)) %>%
    filter(TRT01P == "Xanomeline High Dose") %>%
    arrange(desc(n))

  testthat::expect_equal(c(t41_1$RACE, t41_1$n),
                         c(test_41$row_label1, test_41$ord_layer_1),
                         label = "T41.1")
  #manual check(s)

  #clean up working directory
  rm(t41_1)
  rm(test_41)
})


#test 42 ----
test_that('T42',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE)
      )

    test_42 <- build(t) %>%
      arrange(ord_layer_index, row_label1)

    # output table to check attributes
    save(test_42, file = "~/Tplyr/uat/output/test_42.RData")

    #clean up working directory
    rm(t)
    rm(test_42)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_42.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(sort(unique(adsl$RACE)),
                         test_42$row_label1,
                         label = "T42.1")
  #manual check(s)

  #clean up working directory
  rm(test_42)
})


#test 43 ----
test_that('T43',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE) %>%
          set_order_count_method("byvarn")
      )

    test_43 <- build(t) %>%
      arrange(ord_layer_index, ord_layer_1)

    # output table to check attributes
    save(test_43, file = "~/Tplyr/uat/output/test_43.RData")

    #clean up working directory
    rm(t)
    rm(test_43)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_43.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t43_1 <- distinct(adsl, RACE, RACEN) %>%
    arrange(RACEN)

  testthat::expect_equal(c(t43_1$RACE, t43_1$RACEN),
                         c(test_43$row_label1, test_43$ord_layer_1),
                         label = "T43.1")
  #manual check(s)

  #clean up working directory
  rm(t43_1)
  rm(test_43)
})


#test 44 ----
test_that('T44',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(ETHNIC, by=RACE)
      ) %>%
      add_layer(
        group_count(ETHNIC, by=SEX)
      )

    test_44 <- build(t) %>%
      arrange(ord_layer_index, ord_layer_1)

    # output table to check attributes
    save(test_44, file = "~/Tplyr/uat/output/test_44.RData")

    #clean up working directory
    rm(t)
    rm(test_44)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_44.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t44_racesort <- distinct(adsl, RACE, RACEN) %>%
    mutate(sorter = as.numeric(RACEN)) %>%
    select(RACE,sorter)

  t44_sexsort <- distinct(adsl, SEX) %>%
    mutate(sorter = ifelse(SEX == 'F',1,2)) %>%
    select(SEX,sorter)

  t44_byrace <- group_by(adsl, TRT01P, RACE, ETHNIC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE, ETHNIC ,fill = list(n=0)) %>%
    filter(TRT01P == "Placebo") %>%
    mutate(label = RACE) %>%
    left_join(t44_racesort, by="RACE") %>%
    select(label, ETHNIC, sorter)  %>%
    mutate(ord_layer = 1)

  t44_bysex <- group_by(adsl, TRT01P, SEX, ETHNIC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, SEX, ETHNIC ,fill = list(n=0)) %>%
    filter(TRT01P == "Placebo") %>%
    mutate(label = SEX) %>%
    left_join(t44_sexsort, by="SEX") %>%
    select(label, ETHNIC, sorter) %>%
    mutate(ord_layer = 2)

  t44_1 <- rbind(t44_byrace, t44_bysex)%>%
    arrange(ord_layer, sorter)

  testthat::expect_equal(c(t44_1$label, t44_1$sorter),
                         c(test_44$row_label1, test_44$ord_layer_1),
                         label = "T44.1")
  #manual check(s)

  #clean up working directory
  rm(t44_racesort)
  rm(t44_sexsort)
  rm(t44_byrace)
  rm(t44_bysex)
  rm(t44_1)
  rm(test_44)
})


#test 45 ----
test_that('T45',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(vars(AEBODSYS, AEDECOD)) %>%
          set_order_count_method("bycount") %>%
          set_ordering_cols("Xanomeline High Dose")
      )

    test_45 <- build(t) %>%
      arrange(ord_layer_index, desc(ord_layer_1), row_label1, desc(ord_layer_2), row_label2)

    # output table to check attributes
    save(test_45, file = "~/Tplyr/uat/output/test_45.RData")

    #clean up working directory
    rm(t)
    rm(test_45)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_45.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t45_aebodsys <- group_by(adae, TRTA, AEBODSYS) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, AEBODSYS, fill = list(n=0)) %>%
    mutate(total = n) %>%
    mutate(AEDECOD = AEBODSYS)
  t45_1 <- group_by(adae, TRTA, AEBODSYS, AEDECOD) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, AEBODSYS, AEDECOD, fill = list(n=0)) %>%
    left_join(select(t45_aebodsys, TRTA, AEBODSYS, total), by=c("TRTA","AEBODSYS")) %>%
    rbind(mutate(t45_aebodsys, n=Inf)) %>%
    pivot_wider(values_from=c(n,total), names_from = TRTA) %>%
    arrange(desc(`total_Xanomeline High Dose`), AEBODSYS, desc(`n_Xanomeline High Dose`), AEDECOD) %>%
    filter(n_Placebo > 0 | `n_Xanomeline Low Dose` > 0 | `n_Xanomeline High Dose` > 0) %>%
    mutate(AEDECOD = ifelse(AEBODSYS == AEDECOD, AEDECOD, paste0('   ',AEDECOD)))

  testthat::expect_equal(c(t45_1$AEBODSYS, t45_1$AEDECOD, t45_1$`total_Xanomeline High Dose`, t45_1$`n_Xanomeline High Dose`),
                         c(test_45$row_label1, test_45$row_label2, test_45$ord_layer_1, test_45$ord_layer_2),
                         label = "T45.1")
  #manual check(s)

  #clean up working directory
  rm(t45_aebodsys)
  rm(t45_1)
  rm(test_45)
})


#test 46 ----
test_that('T46',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE) %>%
          set_order_count_method("byvarn")
      ) %>%
      add_layer(
        group_count(ETHNIC) %>%
          set_order_count_method("bycount") %>%
          set_ordering_cols("Xanomeline High Dose")
      ) %>%
      add_layer(
        group_count(SEX) %>%
          set_order_count_method("byfactor")
      ) %>%
      add_layer(
        group_count(RACE_FACTOR) %>%
          set_order_count_method("byfactor")
      )

    test_46 <- build(t) %>%
      arrange(ord_layer_index, ord_layer_1)

    # output table to check attributes
    save(test_46, file = "~/Tplyr/uat/output/test_46.RData")

    #clean up working directory
    rm(t)
    rm(test_46)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_46.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t46_racesort <- distinct(adsl, RACE, RACEN) %>%
    mutate(sorter = as.numeric(RACEN)) %>%
    select(RACE,sorter)

  t46_ethnicsort <- filter(adsl, TRT01P == "Xanomeline High Dose")%>%
    group_by(ETHNIC) %>%
    summarise(sorter = n()) %>%
    select(ETHNIC,sorter)

  t46_sexsort <- distinct(adsl, SEX) %>%
    mutate(sorter = ifelse(SEX == 'F',1,2)) %>%
    select(SEX,sorter)

  t46_racefactorsort <- distinct(adsl, RACE_FACTOR) %>%
    complete(RACE_FACTOR) %>%
    cbind(sorter = c(1,2,3,4))

  t46_race <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE ,fill = list(n=0)) %>%
    filter(TRT01P == "Placebo") %>%
    mutate(label = RACE) %>%
    left_join(t46_racesort, by="RACE") %>%
    select(label, sorter)  %>%
    mutate(ord_layer = 1)

  t46_ethnic <- group_by(adsl, TRT01P, ETHNIC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, ETHNIC,fill = list(n=0)) %>%
    filter(TRT01P == "Placebo") %>%
    mutate(label = ETHNIC) %>%
    left_join(t46_ethnicsort, by="ETHNIC") %>%
    select(label, sorter) %>%
    mutate(ord_layer = 2)

  t46_sex <- group_by(adsl, TRT01P, SEX) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, SEX,fill = list(n=0)) %>%
    filter(TRT01P == "Placebo") %>%
    mutate(label = SEX) %>%
    left_join(t46_sexsort, by="SEX") %>%
    select(label, sorter) %>%
    mutate(ord_layer = 3)

  t46_racefactor <- group_by(adsl, TRT01P, RACE_FACTOR) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE_FACTOR ,fill = list(n=0)) %>%
    filter(TRT01P == "Placebo") %>%
    mutate(label = RACE_FACTOR) %>%
    left_join(t46_racefactorsort, by="RACE_FACTOR") %>%
    select(label, sorter)  %>%
    mutate(ord_layer = 4)

  t46_1 <- rbind(t46_race, t46_ethnic, t46_sex, t46_racefactor)%>%
    arrange(ord_layer, sorter)

  testthat::expect_equal(c(t46_1$label, t46_1$sorter),
                         c(test_46$row_label1, test_46$ord_layer_1),
                         label = "T46.1")
  #manual check(s)

  #clean up working directory
  rm(t46_racesort)
  rm(t46_ethnicsort)
  rm(t46_sexsort)
  rm(t46_racefactorsort)
  rm(t46_race)
  rm(t46_ethnic)
  rm(t46_sex)
  rm(t46_racefactor)
  rm(t46_1)
  rm(test_46)
})

#test 47 ----
test_that('T47',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      set_count_layer_formats(n_counts = f_str('xxxx (xxx.x%)',n,pct)) %>%
      add_layer(
        group_count(RACE)
      ) %>%
      add_layer(
        group_count(SEX) %>%
          set_format_strings(n_counts = f_str('[xxx]',n))
      )

    test_47 <- build(t)

    # output table to check attributes
    save(test_47, file = "~/Tplyr/uat/output/test_47.RData")

    #clean up working directory
    rm(t)
    rm(test_47)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_47.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t47_tots <- group_by(adsl, TRT01P) %>%
    summarise(total = n())

  t47_1 <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE, fill = list(n=0)) %>%
    left_join(t47_tots, by="TRT01P") %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%4s",n),' (',sprintf("%5.1f", pct),'%)')) %>%
    select(col, TRT01P, RACE) %>%
    pivot_wider(values_from = col, names_from = TRT01P)

  t47_2 <- group_by(adsl, TRT01P, SEX) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, SEX, fill = list(n=0)) %>%
    mutate(col = paste0('[',sprintf("%3s",n),']')) %>%
    select(col, TRT01P, SEX) %>%
    pivot_wider(values_from = col, names_from = TRT01P)

  testthat::expect_equal(t47_1$Placebo,
                         filter(test_47, ord_layer_index == 1)$var1_Placebo,
                         label = "T47.1")
  testthat::expect_equal(t47_2$Placebo,
                         filter(test_47, ord_layer_index == 2)$var1_Placebo,
                         label = "T47.2")
  #manual check(s)

  #clean up working directory
  rm(t47_tots)
  rm(t47_1)
  rm(t47_2)
  rm(test_47)
})

#test 48 ----
test_that('T48',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      set_desc_layer_formats(meansd = f_str('xxx.x (xxx.xx)',mean, sd),
                             quartiles = f_str('xxx.x (xxx.x, xxx.x)',iqr, q1, q3)
                             )%>%
      add_layer(
        group_desc(CUMDOSE)
      ) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            n = f_str('xxx',n),
            meansdvar = f_str('xxx.x (xxx.xx) [xxx.xx]',mean, sd, var),
            medianquarts = f_str('xxx.x (xxx.x, xxx.x)',median, q1, q3)
          )
      )

    test_48 <- build(t)

    # output table to check attributes
    save(test_48, file = "~/Tplyr/uat/output/test_48.RData")

    #clean up working directory
    rm(t)
    rm(test_48)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_48.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t48_1 <- group_by(adsl, TRT01P) %>%
    summarise(mean = mean(CUMDOSE),
              sd = sd(CUMDOSE),
              iqr = IQR(CUMDOSE),
              q1 = quantile(CUMDOSE)[[2]],
              q3 = quantile(CUMDOSE)[[4]]
              ) %>%
    mutate(meansd = paste0(sprintf("%5.1f", mean), ' (',sprintf("%6.2f", sd), ')'))%>%
    mutate(quartiles = paste0(sprintf("%5.1f", iqr), ' (',sprintf("%6.1f", q1),', ',sprintf("%6.1f", q3), ')')) %>%
    pivot_longer(cols = c(meansd, quartiles), values_to = "stat") %>%
    select(TRT01P, name, stat) %>%
    pivot_wider(values_from = stat, names_from = TRT01P)

  t48_2 <- group_by(adsl, TRT01P) %>%
    summarise(n = n(),
              mean = mean(AGE),
              sd = sd(AGE),
              var = var(AGE),
              med = median(AGE),
              q1 = quantile(AGE)[[2]],
              q3 = quantile(AGE)[[4]]
    ) %>%
    mutate(n = sprintf("%3s", n)) %>%
    mutate(meansdvar = paste0(sprintf("%5.1f", mean), ' (',sprintf("%6.2f", sd), ') [',sprintf("%6.2f",var),']')) %>%
    mutate(medianquarts = paste0(sprintf("%5.1f", med), ' (',sprintf("%5.1f", q1),', ',sprintf("%5.1f", q3), ')')) %>%
    pivot_longer(cols = c(n, meansdvar, medianquarts), values_to = "stat") %>%
    select(TRT01P, name, stat) %>%
    pivot_wider(values_from = stat, names_from = TRT01P)

  testthat::expect_equal(t48_1$`Xanomeline High Dose`,
                         filter(test_48, ord_layer_index == 1)$`var1_Xanomeline High Dose`,
                         label = "T48.1")
  testthat::expect_equal(t48_2$Placebo,
                         filter(test_48, ord_layer_index == 2)$var1_Placebo,
                         label = "T48.2")
  #manual check(s)

  #clean up working directory
  rm(t48_1)
  rm(t48_2)
  rm(test_48)
})

#test 49 ----
test_that('T49',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      set_shift_layer_formats(f_str('xxxx (xxx.x%)',n,pct)) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND))
      ) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND)) %>%
          set_format_strings(f_str("xxx",n))
      )

    test_49 <- build(t)

    # output table to check attributes
    save(test_49, file = "~/Tplyr/uat/output/test_49.RData")

    #clean up working directory
    rm(t)
    rm(test_49)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_49.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t49_tots <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, BNRIND) %>%
    summarise(total = n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, fill = list(total = 0))

  t49_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, BNRIND, ANRIND) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, ANRIND, fill = list(n=0)) %>%
    left_join(t49_tots, by=c("TRTA", "BNRIND")) %>%
    mutate(pct = ifelse(total > 0, n / total * 100, 0)) %>%
    mutate(col = paste0(sprintf("%4s",n),' (',sprintf("%5.1f", pct),'%)')) %>%
    select(col, TRTA, BNRIND, ANRIND) %>%
    pivot_wider(values_from = col, names_from = c(TRTA, BNRIND))

  t49_2 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, BNRIND, ANRIND) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, ANRIND, fill = list(n=0)) %>%
    mutate(col = sprintf("%3s",n)) %>%
    select(col, TRTA, BNRIND, ANRIND) %>%
    pivot_wider(values_from = col, names_from = c(TRTA, BNRIND))


  testthat::expect_equal(c(t49_1$Placebo_H,t49_1$Placebo_N),
                         c(filter(test_49, ord_layer_index == 1)$var1_Placebo_H,
                           filter(test_49, ord_layer_index == 1)$var1_Placebo_N),
                         label = "T49.1")
  testthat::expect_equal(c(t49_2$Placebo_H,t49_2$Placebo_N),
                         c(filter(test_49, ord_layer_index == 2)$var1_Placebo_H,
                           filter(test_49, ord_layer_index == 2)$var1_Placebo_N),
                         label = "T49.2")
  #manual check(s)

  #clean up working directory
  rm(t49_tots)
  rm(t49_1)
  rm(t49_2)
  rm(test_49)
})


#test 50 ----
test_that('T50',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.count_layer_default_formats' = list(
      'n_counts' = f_str('xxxx [xxx.xx%]', n, pct)
    ))


    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE)
      )

    test_50 <- build(t)

    # output table to check attributes
    save(test_50, file = "~/Tplyr/uat/output/test_50.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_50)


    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_50.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t50_tots <- group_by(adsl, TRT01P) %>%
    summarise(total = n())

  t50_1 <- group_by(adsl, TRT01P, RACE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRT01P, RACE, fill = list(n=0)) %>%
    left_join(t50_tots, by="TRT01P") %>%
    mutate(pct = n / total * 100) %>%
    mutate(col = paste0(sprintf("%4s",n),' [',sprintf("%6.2f", pct),'%]')) %>%
    select(col, TRT01P, RACE) %>%
    pivot_wider(values_from = col, names_from = TRT01P)

  testthat::expect_equal(t50_1$Placebo,
                         test_50$var1_Placebo,
                         label = "T50.1")
  #manual check(s)

  #clean up working directory
  rm(t50_tots)
  rm(t50_1)
  rm(test_50)
})


#test 51 ----
test_that('T51',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.desc_layer_default_formats' = list(
      'meansd' = f_str('xxx.x [xxx.xx]', mean, sd),
      'medquarts' = f_str('xxx.x, xxx.x, xxx.x', q1, median, q3)
    ))

    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE)
      )

    test_51 <- build(t)

    # output table to check attributes
    save(test_51, file = "~/Tplyr/uat/output/test_51.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_51)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_51.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t51_1 <- group_by(adsl, TRT01P) %>%
    summarise(mean = mean(AGE),
              sd = sd(AGE),
              med = median(AGE),
              q1 = quantile(AGE)[[2]],
              q3 = quantile(AGE)[[4]]
    ) %>%
    mutate(meansd = paste0(sprintf("%5.1f", mean), ' [',sprintf("%6.2f", sd), ']'))%>%
    mutate(quartiles = paste0(sprintf("%5.1f", q1), ', ',sprintf("%5.1f", med),', ',sprintf("%5.1f", q3))) %>%
    pivot_longer(cols = c(meansd, quartiles), values_to = "stat") %>%
    select(TRT01P, name, stat) %>%
    pivot_wider(values_from = stat, names_from = TRT01P)

  testthat::expect_equal(t51_1$Placebo,
                         test_51$var1_Placebo,
                         label = "T51.1")
  #manual check(s)

  #clean up working directory
  rm(t51_1)
  rm(test_51)
})


#test 52 ----
test_that('T52',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.shift_layer_default_formats' = list(
      f_str('xxxx (xxx.xx%)', n, pct)
    ))

    t <- tplyr_table(adlb, TRTA, where=(PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "")) %>%
      add_layer(
        group_shift(vars(row=ANRIND, column=BNRIND))
      )

    test_52 <- build(t)

    # output table to check attributes
    save(test_52, file = "~/Tplyr/uat/output/test_52.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_52)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_52.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t52_tots <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, BNRIND) %>%
    summarise(total = n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, fill = list(total = 0))

  t52_1 <- filter(adlb, PARAMCD == "BILI" & AVISIT == "Week 2" & ANRIND != "" & BNRIND != "") %>%
    group_by(TRTA, BNRIND, ANRIND) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(TRTA, BNRIND, ANRIND, fill = list(n=0)) %>%
    left_join(t52_tots, by=c("TRTA", "BNRIND")) %>%
    mutate(pct = ifelse(total > 0, n / total * 100, 0)) %>%
    mutate(col = paste0(sprintf("%4s",n),' (',sprintf("%6.2f", pct),'%)')) %>%
    select(col, TRTA, BNRIND, ANRIND) %>%
    pivot_wider(values_from = col, names_from = c(TRTA, BNRIND))

  testthat::expect_equal(c(t52_1$Placebo_H,t52_1$Placebo_N),
                         c(test_52$var1_Placebo_H, test_52$var1_Placebo_N),
                         label = "T52.1")
  #manual check(s)

  #clean up working directory
  rm(t52_tots)
  rm(t52_1)
  rm(test_52)
})


#test 53 ----
test_that('T53',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.precision_cap' = c('int'=5, 'dec'=2))

    t <- tplyr_table(adlb, TRTA, where=PARAMCD == "BUN") %>%
      add_layer(
        group_desc(AVAL)
      )

    test_53 <- filter(build(t), row_label1 != 'Missing')

    # output table to check attributes
    save(test_53, file = "~/Tplyr/uat/output/test_53.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_53)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_53.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t53_int = min(5, max(nchar(sub("\\..*", "", filter(adlb, PARAMCD == "BUN")$AVAL))))
  t53_dec = min(2, max(nchar(sub("*.\\.", "", filter(adlb, PARAMCD == "BUN")$AVAL))))
  t53_1 <- filter(adlb, PARAMCD == "BUN") %>%
    group_by(TRTA) %>%
    summarise(n = n(),
              mean = mean(AVAL),
              sd = sd(AVAL),
              median = median(AVAL),
              q1 = quantile(AVAL)[[2]],
              q3 = quantile(AVAL)[[4]],
              min = min(AVAL),
              max = max(AVAL)
    ) %>%
    mutate(n = sprintf("%*s",t53_int,n)) %>%
    mutate(meansd = paste0(sprintf("%*s",t53_int + t53_dec + 2, sprintf("%.*f", t53_dec + 1, mean)), ' (',
                           sprintf("%*s",t53_int + t53_dec + 3, sprintf("%.*f", t53_dec + 2, sd)), ')')) %>%
    mutate(median = sprintf("%*s",t53_int + t53_dec + 2, sprintf("%.*f", t53_dec + 1, median))) %>%
    mutate(quartiles = paste0(sprintf("%*s",t53_int + t53_dec + 2, sprintf("%.*f", t53_dec + 1, q1)),', ',
                              sprintf("%*s",t53_int + t53_dec + 2, sprintf("%.*f", t53_dec + 1, q3)))) %>%
    mutate(minmax = paste0(sprintf("%*s",t53_int + t53_dec + 1, sprintf("%.*f", t53_dec, min)),', ',
                           sprintf("%*s",t53_int + t53_dec + 1, sprintf("%.*f", t53_dec, max)))) %>%
    pivot_longer(cols = c(n, meansd, median, quartiles, minmax), values_to = "stat") %>%
    select(TRTA, name, stat) %>%
    pivot_wider(values_from = stat, names_from = TRTA)

  testthat::expect_equal(t53_1$Placebo,
                         test_53$var1_Placebo,
                         label = "T53.1")
  #manual check(s)

  #clean up working directory
  rm(t53_int)
  rm(t53_dec)
  rm(t53_1)
  rm(test_53)
})


#test 54 ----
test_that('T54',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.custom_summaries' = quos(geometric_mean = exp(sum(log(.var[.var > 0]), na.rm=TRUE) / length(.var))))

    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(AGE) %>%
          set_format_strings(
            'Geometric Mean)' = f_str('xxx.xx', geometric_mean)
          )
      )

    test_54 <- filter(build(t), row_label1 != 'Missing')

    # output table to check attributes
    save(test_54, file = "~/Tplyr/uat/output/test_54.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_54)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_54.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t54_1 <- group_by(adsl, TRT01P) %>%
    summarise(geometric_mean = exp(sum(log(AGE[AGE > 0]), na.rm=TRUE) / length(AGE))) %>%
    mutate(geometric_mean = sprintf("%6.2f",geometric_mean)) %>%
    pivot_wider(values_from = "geometric_mean",names_from = "TRT01P")

  testthat::expect_equal(c(t54_1$Placebo, t54_1$`Xanomeline Low Dose`, t54_1$`Xanomeline High Dose`),
                         c(test_54$var1_Placebo, test_54$`var1_Xanomeline Low Dose`, test_54$`var1_Xanomeline High Dose`),
                         label = "T54.1")
  #manual check(s)

  #clean up working directory
  rm(t54_1)
  rm(test_54)
})


#test 55 ----
test_that('T55',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.scipen' = -3)

    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE) %>%
          add_risk_diff(c("Xanomeline High Dose", "Placebo"))
      )

    test_55 <- suppressWarnings(build(t))

    # output table to check attributes
    save(test_55, file = "~/Tplyr/uat/output/test_55.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_55)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_55.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  options("scipen" = -3)
  tot_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose"), n=n())[[1]]
  cnt_t <- summarise(filter(adsl, TRT01P == "Xanomeline High Dose" & RACE == 'WHITE'), n=n())[[1]]
  tot_p <- summarise(filter(adsl, TRT01P == "Placebo"), n=n())[[1]]
  cnt_p <- summarise(filter(adsl, TRT01P == "Placebo" & RACE == 'WHITE'), n=n())[[1]]
  testvals <- prop.test(c(cnt_t, cnt_p), c(tot_t,tot_p))
  t55_1 = paste0(format(round(testvals$estimate[[1]] - testvals$estimate[[2]],3),nsmall = 3), ' (',
                 format(round(testvals$conf.int[[1]],3),nsmall = 3), ', ',
                 format(round(testvals$conf.int[[2]],3),nsmall = 3), ')'
                 )

  testthat::expect_equal(t55_1,
                         filter(test_55,row_label1 == 'WHITE')$`rdiff_Xanomeline High Dose_Placebo`,
                         label = "T55.1")
  #manual check(s)

  #clean up working directory
  options("scipen" = 0)
  rm(tot_t)
  rm(cnt_t)
  rm(tot_p)
  rm(cnt_p)
  rm(testvals)
  rm(t55_1)
  rm(test_55)
})


#test 56 ----
test_that('T56',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    options('tplyr.quantile_type' = 3)

    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_desc(CUMDOSE) %>%
          set_format_strings(
            'Quartiles' = f_str('xxx.x, xxx.x', q1, q3)
          )
      )

    test_56 <- filter(build(t), row_label1 != 'Missing')

    # output table to check attributes
    save(test_56, file = "~/Tplyr/uat/output/test_56.RData")

    #clean up working directory
    options(opts)
    rm(t)
    rm(test_56)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_56.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t56_1 <- group_by(adsl, TRT01P) %>%
    summarise(q1 = quantile(CUMDOSE, type = 3)[[2]],
              q3 = quantile(CUMDOSE, type = 3)[[4]]) %>%
    mutate(col = paste0(sprintf("%5.1f", q1), ', ', sprintf("%5.1f", q3))) %>%
    select(TRT01P, col) %>%
    pivot_wider(values_from = "col",names_from = "TRT01P")

  testthat::expect_equal(c(t56_1$Placebo, t56_1$`Xanomeline Low Dose`, t56_1$`Xanomeline High Dose`),
                         c(test_56$var1_Placebo, test_56$`var1_Xanomeline Low Dose`, test_56$`var1_Xanomeline High Dose`),
                         label = "T56.1")
  #manual check(s)

  #clean up working directory
  rm(t56_1)
  rm(test_56)
})


#test 57 ----
test_that('T57',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_layer(
        group_count(RACE)
      ) %>%
      build() %>%
      mutate_all(as.character)


    test_57 <- add_column_headers(t, "Race|Placebo|Xanomeline High Dose|Xanomeline Low Dose|LayerIndex|Sorter")

    # output table to check attributes
    save(test_57, file = "~/Tplyr/uat/output/test_57.RData")

    #clean up working directory
    rm(t)
    rm(test_57)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_57.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(c("Race", "Placebo","Xanomeline High Dose","Xanomeline Low Dose", "LayerIndex", "Sorter"),
                         as.character(test_57[1,]),
                         label = "T57.1")
  #manual check(s)

  #clean up working directory
  rm(test_57)
})


#test 58 ----
test_that('T58',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adae, TRTA) %>%
      add_layer(
        group_count(vars(AEBODSYS, AEDECOD)) %>%
          set_format_strings(f_str('xxx', n))
      ) %>%
      build() %>%
      arrange(desc(ord_layer_1), desc(ord_layer_2))

    test_58 <- apply_row_masks(t, row_breaks = TRUE, ord_layer_1)

    # output table to check attributes
    save(test_58, file = "~/Tplyr/uat/output/test_58.RData")

    #clean up working directory
    rm(t)
    rm(test_58)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_58.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  t58_aebodsys <- group_by(adae, TRTA, AEBODSYS) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, AEBODSYS, fill = list(n=0)) %>%
    mutate(AEDECOD = AEBODSYS)
  t58_breaks <- select(t58_aebodsys, TRTA, AEBODSYS) %>%
    mutate(n = -1) %>%
    mutate(AEDECOD = "")
  t58_1 <- group_by(adae, TRTA, AEBODSYS, AEDECOD) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    complete(TRTA, AEBODSYS, AEDECOD, fill = list(n=0)) %>%
    rbind(t58_aebodsys) %>%
    rbind(t58_breaks) %>%
    pivot_wider(values_from=c(n), names_from = TRTA) %>%
    filter(Placebo != 0 | `Xanomeline Low Dose` != 0 | `Xanomeline High Dose` != 0) %>%
    mutate(AEDECOD = ifelse(AEDECOD == "", "", ifelse(AEBODSYS == AEDECOD, paste0('z', AEDECOD), paste0('    ',AEDECOD)))) %>%
    arrange(AEBODSYS, desc(AEDECOD)) %>%
    mutate(AEDECOD = substring(AEDECOD, 2)) %>%
    mutate(AEBODSYS = ifelse(AEBODSYS == AEDECOD, AEBODSYS, "")) %>%
    mutate(Placebo = ifelse(Placebo == -1, "", sprintf("%3s",Placebo))) %>%
    mutate(`Xanomeline Low Dose` = ifelse(`Xanomeline Low Dose` == -1, "", sprintf("%3s",`Xanomeline Low Dose`))) %>%
    mutate(`Xanomeline High Dose` = ifelse(`Xanomeline High Dose` == -1, "", sprintf("%3s",`Xanomeline High Dose`)))

  testthat::expect_equal(c(t58_1$AEBODSYS, t58_1$AEDECOD, t58_1$Placebo,
                           t58_1$`Xanomeline High Dose`, t58_1$`Xanomeline Low Dose`),
                         c(test_58$row_label1, test_58$row_label2, test_58$var1_Placebo,
                           test_58$`var1_Xanomeline High Dose`, test_58$`var1_Xanomeline Low Dose`),
                         label = "T58.1")
  #manual check(s)

  #clean up working directory
  rm(t58_aebodsys)
  rm(t58_breaks)
  rm(t58_1)
  rm(test_58)
})

#test 59 ----
test_that('T59',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P) %>%
      add_total_group() %>%
      add_treat_grps('Total Xanomeline' = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
      add_layer(
        group_count(SEX, by = "Sex")
      ) %>%
      add_layer(
        group_desc(AGE, by = "Age")
      ) %>%
      add_layer(
        group_count(RACE_FACTOR, by = "Race")
      ) %>%
      add_layer(
        group_count(ETHNIC, by = "Ethnicity")
      ) %>%
      add_layer(
        group_desc(WEIGHTBL, by = "Baseline Weight")
      )

    built <- build(t) %>%
      apply_row_masks() %>%
      select(starts_with("row"),"var1_Placebo",starts_with("var1_X"),"var1_Total Xanomeline","var1_Total") %>%
      add_column_headers("Parameter |  | Placebo | Xanomeline Low Dose | Xanomeline High Dose |
                         Total | Total Xanomeline")

    hux <- huxtable::as_hux(built) %>%
      huxtable::set_width(1.5) %>%
      huxtable::map_align(huxtable::by_cols("left","left","center","center","center","center","center"))

    test_59 <- pharmaRTF::rtf_doc(hux) %>%
      pharmaRTF::add_titles(pharmaRTF::hf_line("Demographics Summary", bold=TRUE))

    # output table to check attributes
    pharmaRTF::write_rtf(test_59, file = "~/Tplyr/uat/output/test_59.rtf")

    #clean up working directory
    rm(t)
    rm(built)
    rm(hux)
    rm(test_59)

    #load output for checks
  } else {

  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  #manual check(s)
  expect_true(vur[vur$ID == "T59.1", "Response"])

  #clean up working directory

})
#clean up ----
rm(vur)
