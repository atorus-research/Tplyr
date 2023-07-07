

t <- tplyr_table(mtcars, gear)
# Add header_n before build
header_n <- tibble(
  gear = c(3, 4, 5),
  n = c(15, 12, 5)
)
treat_var <- quo(gear)

test_that("this_denom can be called after a group_by and gives totals", {

  df <- mtcars %>%
    group_by(gear) %>%
    do(this_denom(., header_n, treat_var)) %>%
    ungroup() %>%
    select(total)

  expect_equal(df, tibble(total = rep(c(15, 12, 5), c(15, 12, 5))))

  df2 <- mtcars %>%
    group_by(gear, vs) %>%
    do(this_denom(., header_n, treat_var)) %>%
    ungroup() %>%
    select(total)

  expect_equal(df2, tibble(total = rep(c(15, 12, 5), c(15, 12, 5))))
})

test_that("can build when data has no rows and population data is set (#131)", {

  load(test_path("adae.Rdata"))
  load(test_path("adsl.Rdata"))

  t <- adae %>%
    tplyr_table(TRTA) %>%
    set_pop_data(adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(
        TRTEMFL,
        where = TRTEMFL == "Y" & AESER == "Y" & AEREL == "REMOTE"
      ) %>%
        set_distinct_by(USUBJID)
    )

  expect_no_error(build(t))

})
