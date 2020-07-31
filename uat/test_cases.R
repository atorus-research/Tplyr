context("Atorus Validation")

#' @title Test Cases Code
#' @section Last Updated By:
#' Nathan Kosiba
#' @section Last Update Date:
#' 7/29/2020

#insert any necessary libraries
library(Tplyr)
library(tidyverse)
library(testthat)
library(haven)
library(rlang)

#insert code applicable to all tests i.e. functions
adsl <- read_xpt("~/Tplyr/uat/input/adsl.xpt")
adae <- read_xpt("~/Tplyr/uat/input/adae.xpt")


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
    test_3 <- tplyr_table(adsl, TRT01P) %>%
      add_treat_group('Total Xanomeline', c("Xanomeline High Dose", "Xanomeline Low Dose"))

    # output table to check attributes
    save(test_3, file = "~/Tplyr/uat/output/test_3.RData")

    #clean up working directory
    rm(test_3)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_3.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(, label = "T3.1")
  testthat::expect_equal(, label = "T3.2")
  #manual check(s)

  #clean up working directory
  rm(test_3)
})

#test 4
test_that('T4',{
  if(is.null(vur)) {

    #perform test and create outputs to use for checks
    #if input files are needed they should be read in from "~/uat/input" folder
    #outputs should be sent to "~/uat/output" folder
    t <- tplyr_table(adsl, TRT01P)
    test_4 <- group_desc(t, AGE, by="Age (Groups)", where = SAFFL == "Y")

    # output table to check attributes
    save(test_4, file = "~/Tplyr/uat/output/test_4.RData")

    #clean up working directory
    rm(t)
    rm(test_4)

    #load output for checks
  } else {
    load("~/Tplyr/uat/output/test_4.RData")
  }

  #perform checks
  skip_if(is.null(vur))
  #programmatic check(s)
  testthat::expect_equal(, label = "T4.1")
  testthat::expect_equal(, label = "T4.2")
  #manual check(s)

  #clean up working directory
  rm(test_4)
})

#count layer testing

t_dm <- tplyr_table(adsl, TRT01P) %>%
  add_total_group() %>%
  add_layer(
    group_count(RACE) %>%
      set_order_count_method("byvarn")
  ) %>%
  add_layer(
    group_count(ETHNIC) %>%
      set_order_count_method("byvarn")
  ) %>%
  add_layer(
    group_count(SEX) %>%
      set_order_count_method("byvarn")
  ) %>%
  add_layer(
    group_desc(AGE)
  )

built_dm <- t_dm %>%
  build() %>%
  arrange(ord_layer_index, ord_layer_1)


t_by <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(AGEGR1, by=vars(ETHNIC,RACE))
  )

built_by <- build(t_by)


#desc layer testing

t_cont <- tplyr_table(adsl, TRT01P) %>%
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
built_cont <- t_cont %>%
  build()

ndat <- get_numeric_data(t_cont)[[1]]

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], n=n())[[1]],
                       subset(ndat, stat == 'n' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], mean=mean(AGE))[[1]],
                       subset(ndat, stat == 'mean' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], median=median(AGE))[[1]],
                       subset(ndat, stat == 'median' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], sd=sd(AGE))[[1]],
                       subset(ndat, stat == 'sd' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], var=var(AGE))[[1]],
                       subset(ndat, stat == 'var' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], min=min(AGE))[[1]],
                       subset(ndat, stat == 'min' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], max=max(AGE))[[1]],
                       subset(ndat, stat == 'max' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], iqr=IQR(AGE))[[1]],
                       subset(ndat, stat == 'iqr' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q1=quantile(AGE)[[2]])[[1]],
                       subset(ndat, stat == 'q1' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo',], q3=quantile(AGE)[[4]])[[1]],
                       subset(ndat, stat == 'q3' & TRT01P == 'Placebo')[['value']])

testthat::expect_equal(summarise(adsl[adsl$TRT01P == 'Placebo' & is.na(adsl$AGE),], n=n())[[1]],
                       subset(ndat, stat == 'missing' & TRT01P == 'Placebo')[['value']])


#format matching
re_npe <- "\s?\d?\d\s\(\s?\d?\d?\d.\d%\)\s\[\s?\d?\d\]"
re_np <- "\s?\d?\d\s\(\s?\d?\d?\d.\d%\)"


#clean up
rm(vur)
