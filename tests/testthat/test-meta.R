load(test_path('adae.Rdata'))
load(test_path('adsl.Rdata'))
load(test_path('adlb.Rdata'))

adae <- adae %>%
  filter(AEBODSYS %in% c("NERVOUS SYSTEM DISORDERS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                         "PSYCHIATRIC DISORDERS" ))

# Alter some reference indicators for shift
adlb[c(5, 10, 15, 20, 25, 30), 'ANRIND'] <- "H"
adlb[c(5, 10, 15, 20, 25, 30), 'BNRIND'] <- "L"

# Insert a missing value
adsl$ETHNIC[1] <- NA_character_

# Define a function to flip factors to characters
fct2chr <- function(.data) {
  .data %>%
    mutate(
      across(where(is.factor), ~as.character(.x))
    )
}

# Table to test out totals, missings, table where, cols, by, unnested
# basic counts, and descriptive stats
t1 <- tplyr_table(adsl, TRT01A, where = SAFFL == "Y", cols=SEX) %>%
  add_treat_grps(
    Treated = c("Xanomeline High Dose", "Xanomeline Low Dose")
  ) %>%
  # Create a total group column
  add_total_group() %>%
  # Add a count layer for SEX
  add_layer(
    group_count(ETHNIC, by = RACE) %>%
      set_denoms_by(TRT01A) %>%
      # Make a total row
      add_total_row(fmt=f_str("xx",n), count_missings=FALSE, sort_value=-Inf) %>%
      # Change the total row label
      set_total_row_label("n") %>%
      # Add a missing count row, which is made up of any NA values
      set_missing_count(f_str("xx", n), denom_ignore=TRUE, Missing = NA, Empty = "Blah")
  ) %>%
  # Add a descriptive statistics layer for AGE
  add_layer(
    group_desc(AGE, by = RACE)
  )

dat1 <- t1 %>%
  build(metadata=TRUE)

# Table to test out character unnested, and nested counts, layer where
t2 <- tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count("Text label", where = AESEV == "MODERATE") %>%
      add_risk_diff(
        c("Xanomeline High Dose", "Placebo")
      )
  ) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  )

dat2 <- suppressWarnings(t2 %>% build(metadata=TRUE))

# Table to test out character outer for count layers
t3 <- tplyr_table(adsl, TRT01A) %>%
  add_layer(
    group_count(vars("Outer string", RACE))
  )

dat3 <- t3 %>%
  build(metadata=TRUE)

# Table for testing of Shift layers
t4 <- tplyr_table(adlb, TRTA, where = AVISIT != "") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column=ANRIND), by=AVISIT)
  )

dat4 <- t4 %>%
  build(metadata=TRUE)

test_that("Metadata creation errors generate properly", {
  m <- tplyr_meta()

  # Not providing metadata object
  expect_snapshot_error(add_variables(mtcars, quos(a)))
  expect_snapshot_error(add_filters(mtcars, quos(a==1)))
  expect_snapshot_error(add_anti_join(mtcars, m, quos(a==1)))
  expect_snapshot_error(add_anti_join(m, mtcars, quos(a==1)))

  # Didn't provide filter
  expect_snapshot_error(tplyr_meta(quos(a), 'x'))
  expect_snapshot_error(add_filters(m, 'x'))

  # Didn't provide names
  expect_snapshot_error(tplyr_meta('x'))
  expect_snapshot_error(add_variables(m, 'x'))
  expect_snapshot_error(add_anti_join(m, m, 'x'))

})

test_that("Exported metadata function construct metadata properly", {
  m <- tplyr_meta(quos(a, b, c), quos(a==1, b==2, c==3))

  expect_equal(m$names, quos(a, b, c))
  expect_equal(m$filters, quos(a==1, b==2, c==3))

  m <- add_variables(m, quos(x))
  m <- add_filters(m, quos(x=="a"))
  m2 <- add_anti_join(m, m, quos(y))

  expect_equal(m$names, quos(a, b, c, x))
  expect_equal(m$filters, quos(a==1, b==2, c==3, x=="a"))
  expect_equal(m2$anti_join$join_meta, m)
  expect_equal(m2$anti_join$on, quos(y))
})

test_that("Descriptive Statistics metadata backend assembles correctly", {

  # Standard treatment group
  m1 <- get_meta_subset(t1, 'd7_2', 'var1_Placebo_M')
  m1_comp <- t1$built_target %>%
    filter(
      RACE == "BLACK OR AFRICAN AMERICAN",
      SEX == "M",
      SAFFL == "Y",
      TRT01A == "Placebo"
    ) %>%
    select(USUBJID, TRT01A, RACE, SEX, SAFFL, AGE) %>%
    fct2chr()

  expect_equal(m1, m1_comp, ignore_attr=TRUE)

  # Total group
  m2 <- get_meta_subset(t1, 'd7_2', 'var1_Total_F')
  m2_comp <- t1$built_target %>%
    filter(
      RACE == "BLACK OR AFRICAN AMERICAN",
      SEX == "F",
      SAFFL == "Y",
      TRT01A %in% c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
    ) %>%
    select(USUBJID, TRT01A, RACE, SEX, SAFFL, AGE) %>%
    fct2chr()

  expect_equal(m2, m2_comp, ignore_attr=TRUE)

  # Treated group
  m3 <- get_meta_subset(t1, 'd7_2', 'var1_Treated_F')
  m3_comp <- t1$built_target %>%
    filter(
      RACE == "BLACK OR AFRICAN AMERICAN",
      SEX == "F",
      SAFFL == "Y",
      TRT01A %in% c("Xanomeline High Dose", "Xanomeline Low Dose")
    ) %>%
    select(USUBJID, TRT01A, RACE, SEX, SAFFL, AGE) %>%
    fct2chr()

  expect_equal(m3, m3_comp, ignore_attr=TRUE)
})

test_that("Count Layer metadata backend assembles correctly", {

  # Here use demographics t1
  # Standard treatment, normal row count
  m1 <- get_meta_subset(t1, 'c6_1', 'var1_Placebo_M')
  m1_comp <- t1$built_target %>%
    filter(
      RACE == "BLACK OR AFRICAN AMERICAN",
      SEX == "M",
      SAFFL == "Y",
      TRT01A == "Placebo",
      ETHNIC == "NOT HISPANIC OR LATINO"
    ) %>%
    select(USUBJID, TRT01A, RACE, SEX, SAFFL, ETHNIC) %>%
    fct2chr()

  expect_equal(m1, m1_comp, ignore_attr=TRUE)

  # Total group, missing row
  m2 <- get_meta_subset(t1, 'c11_1', 'var1_Total_F')
  m2_comp <- t1$built_target %>%
    filter(
      RACE == "WHITE",
      SEX == "F",
      SAFFL == "Y",
      TRT01A %in% c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
      is.na(ETHNIC)
    ) %>%
    select(USUBJID, TRT01A, RACE, SEX, SAFFL, ETHNIC) %>%
    fct2chr()

  expect_equal(m2, m2_comp, ignore_attr=TRUE)

  # Treated group, total row
  m3 <- get_meta_subset(t1, 'c13_1', 'var1_Treated_F')
  m3_comp <- t1$built_target %>%
    filter(
      SEX == "F",
      SAFFL == "Y",
      TRT01A %in% c("Xanomeline High Dose", "Xanomeline Low Dose")
    ) %>%
    select(USUBJID, TRT01A, SEX, SAFFL, ETHNIC, RACE) %>%
    fct2chr()

  expect_equal(m3, m3_comp, ignore_attr=TRUE)

  # Now using AE table t2
  # Unnested character target
  m4 <- get_meta_subset(t2, "c1_1", "var1_Xanomeline Low Dose")
  m4_comp <- t2$built_target %>%
    filter(
      AESEV == "MODERATE",
      TRTA == "Xanomeline Low Dose"
    ) %>%
    select(USUBJID, TRTA, AESEV) %>%
    fct2chr()

  expect_equal(m4, m4_comp, ignore_attr=TRUE)

  # Outer layer
  m5 <- get_meta_subset(t2, "c1_2", "var1_Xanomeline High Dose")
  m5_comp <- t2$built_target %>%
    filter(
      TRTA == "Xanomeline High Dose",
      AEBODSYS == "NERVOUS SYSTEM DISORDERS"
    ) %>%
    select(USUBJID, TRTA, AEDECOD, AEBODSYS) %>%
    fct2chr()

  expect_equal(m5, m5_comp, ignore_attr=TRUE)

  # Inner layer
  m6 <- get_meta_subset(t2, 'c6_2', "var1_Xanomeline Low Dose")
  m6_comp <- t2$built_target %>%
    filter(
      TRTA == "Xanomeline Low Dose",
      AEBODSYS == "NERVOUS SYSTEM DISORDERS",
      AEDECOD == "DIZZINESS"
    ) %>%
    select(USUBJID, TRTA, AEBODSYS, AEDECOD) %>%
    fct2chr()

  expect_equal(m6, m6_comp, ignore_attr=TRUE)

  # Risk difference
  m7 <- get_meta_subset(t2, 'c1_1', 'rdiff_Xanomeline High Dose_Placebo')
  m7_comp <- t2$built_target %>%
    filter(
      AESEV == "MODERATE",
      TRTA %in% c("Xanomeline High Dose", "Placebo")
    ) %>%
    select(USUBJID, TRTA, AESEV) %>%
    fct2chr()

  expect_equal(m4, m4_comp, ignore_attr=TRUE)


  # Character outer string
  m8 <- get_meta_subset(t3, 'c1_1', 'var1_Placebo')
  m8_comp <- t3$built_target %>%
    filter(
      TRT01A == "Placebo"
    ) %>%
    select(USUBJID, TRT01A, RACE) %>%
    fct2chr()

  expect_equal(m8, m8_comp, ignore_attr=TRUE)

  m9 <- get_meta_subset(t3, 'c3_1', 'var1_Placebo')
  m9_comp <- t3$built_target %>%
    filter(
      TRT01A == "Placebo",
      RACE == "BLACK OR AFRICAN AMERICAN"
    ) %>%
    select(USUBJID, TRT01A, RACE) %>%
    fct2chr()

  expect_equal(m9, m9_comp, ignore_attr=TRUE)

})

test_that("Shift Layer metadata backend assembles correctly", {
  m1 <- get_meta_subset(t4, 's3_1', 'var1_Placebo_H')
  m1_comp <- t4$built_target %>%
    filter(
      BNRIND == "L",
      ANRIND == "H",
      AVISIT == "End of Treatment",
      TRTA == "Placebo"
    ) %>%
    select(USUBJID, TRTA, AVISIT, ANRIND, BNRIND) %>%
    fct2chr()

  expect_equal(m1, m1_comp, ignore_attr=TRUE)
})

test_that("metadata queried without Tplyr table queries effectively", {
  # Pull out the dataframes directly
  meta <- t1$metadata
  dat <- t1$target

  m1 <- get_meta_subset(meta, 'd7_2', 'var1_Placebo_M', target = dat)

  m1_comp <- t1$built_target %>%
    filter(
      RACE == "BLACK OR AFRICAN AMERICAN",
      SEX == "M",
      SAFFL == "Y",
      TRT01A == "Placebo",
      ETHNIC == "NOT HISPANIC OR LATINO"
    ) %>%
    select(USUBJID, TRT01A, RACE, SEX, SAFFL, AGE) %>%
    fct2chr()

  expect_equal(m1, m1_comp, ignore_attr=TRUE)
})

t <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(wt)
  )

test_that("Metadata extraction and extension error properly", {

  expect_snapshot_error(get_metadata(mtcars))

  expect_snapshot_error(get_metadata(t))

  dat <- t %>% build(metadata=TRUE)

  m <- tibble(
    var1_3 = list(tplyr_meta())
  )

  expect_snapshot_error(append_metadata(t, m))

  m['row_id'] <- c("d1_1")
  expect_snapshot_error(append_metadata(t, m))

})

test_that("Metadata extraction and extension work properly", {

  m <- tibble(
    row_id = 'x1_1',
    var1_3 = list(tplyr_meta())
  )

  t <- append_metadata(t, m)
  expect_snapshot(as.data.frame(get_metadata(t)))

})

test_that("Metadata print method is accurate", {
  x <- tplyr_meta(quos(a, b, c), quos(a==1, b==2, c==3, x=="a"))
  expect_snapshot(print(x))
})


test_that("Anti-join extraction works properly", {

  # This is purposefully a convoluted warning that's unrealistic, hence the
  # warning that's generating.
  expect_snapshot_warning({
    t <- tplyr_table(tplyr_adsl, TRT01A, cols = ETHNIC) %>%
      add_layer(
        group_count(RACE, by = SEX) %>%
          set_distinct_by(USUBJID) %>%
          add_missing_subjects_row()
      )
  })

  x <- build(t, metadata=TRUE)

  # Check that the object looks right
  res <- get_meta_result(t, 'c7_1', 'var1_Placebo_HISPANIC OR LATINO')

  expect_equal(unname(map_chr(res$names, as_label)), c("TRT01A", "SEX", "ETHNIC", "RACE"))
  expect_equal(
    unname(map_chr(res$filters, as_label)),
    c("TRT01A == c(\"Placebo\")", "SEX == c(\"F\")", "ETHNIC == c(\"HISPANIC OR LATINO\")",
      "TRUE", "TRUE")
    )
  expect_equal(unname(map_chr(res$anti_join$join_meta$names, as_label)), c("TRT01A", "ETHNIC"))
  expect_equal(
    unname(map_chr(res$anti_join$join_meta$filters, as_label)),
    c("TRT01A == c(\"Placebo\")", "ETHNIC == c(\"HISPANIC OR LATINO\")", "TRUE", "TRUE")
  )
  expect_equal(as_label(res$anti_join$on[[1]]), "USUBJID")

  # Variables needed for the merge aren't there
  expect_snapshot_error(get_meta_subset(t, 'c7_1', 'var1_Placebo_HISPANIC OR LATINO', add_cols = quos(SITEID)))


  sbst <- get_meta_subset(t, 'c7_1', 'var1_Placebo_HISPANIC OR LATINO')


  cmp <- tplyr_adsl %>% filter(
      USUBJID == "01-701-1023"
    )

  # The counted subjects will include female, so this subject would have to be male
  # Again - this is a weird example that wouldn't be used in practice, but this is the
  # row variable
  expect_true(cmp$SEX == "M")
  # Since this is column, these would both match the metadata
  expect_true(cmp$TRT01A == "Placebo")
  expect_true(cmp$ETHNIC == "HISPANIC OR LATINO")

  # and then selecting out the columns these should match
  expect_equal(
    sbst,
    cmp %>%
      select(USUBJID, TRT01A, ETHNIC)
  )

  # Now for a real example, but also test for nested counts
  t <- tplyr_table(tplyr_adae, TRTA) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_distinct_by(USUBJID) %>%
        add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = Inf)
    )

  x <- build(t, metadata=TRUE)

  sbst <- get_meta_subset(t, 'c23_1', 'var1_Placebo')

  # If you manually check out x, the count here is 65
  expect_equal(nrow(sbst), 65)
  expect_equal(unique(sbst$TRT01A), "Placebo")

})

test_that("Tplyr meta print method works as expected", {
  meta <- tplyr_meta(
    names = quos(TRTP, EFFFL, ITTFL, ANL01FL, SITEGR1, AVISIT, AVISITN, PARAMCD, AVAL, BASE, CHG),
    filters = quos(EFFFL == "Y", ITTFL == "Y", PARAMCD == "ACTOT", ANL01FL == "Y", AVISITN == 24)
  )

  meta2 <- meta %>%
    add_anti_join(
      join_meta = tplyr_meta(
        names = quos(TRT01P, EFFFL, ITTFL, SITEGR1),
        filters = quos(EFFFL == "Y", ITTFL == "Y")
      ),
      on = quos(USUBJID)
    )

  expect_snapshot(print(meta2))
})
