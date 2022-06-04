load(test_path('adae.Rdata'))
load(test_path('adsl.Rdata'))

# Insert a missing value
adsl$ETHNIC[1] <- NA_character_

# Table to test out totals, missings
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
      set_missing_count(f_str("xx", n), denom_ignore=TRUE, Missing = NA)
  ) %>%
  # Add a descriptive statistics layer for AGE
  add_layer(
    group_desc(AGE, by = RACE)
  )

dat <- t1 %>%
  build(metadata=TRUE)

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
    mutate(
      across(where(is.factor), ~as.character(.x))
    )

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
    mutate(
      across(where(is.factor), ~as.character(.x))
    )

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
    mutate(
      across(where(is.factor), ~as.character(.x))
    )

  expect_equal(m3, m3_comp, ignore_attr=TRUE)
})

test_that("Count Layer metadata backend assembles correctly", {


})
