

test_that("A group_count layer can be ordered properly with factors", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl)
    )
  b_t <- build(t)
  expect_equivalent(b_t[, 1], tibble(row_label1 = as.character(c(4, 6, 8))))
  expect_equivalent(b_t[, 5], tibble(ord_layer_index = as.integer(c(1, 1, 1))))
  expect_equivalent(b_t[, 6], tibble(ord_layer_1 = as.numeric(c(1, 2, 3))))

  mtcars$cyl <- factor(mtcars$cyl, c(6, 8, 4))
  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl)
    )
  b_t2 <- build(t2)
  expect_equivalent(b_t2[, 1], tibble(row_label1 = as.character(c(6, 8, 4))))
  expect_equivalent(b_t2[, 5], tibble(ord_layer_index = as.integer(c(1, 1, 1))))
  expect_equivalent(b_t2[, 6], tibble(ord_layer_1 = as.numeric(c(1, 2, 3))))

  t3 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(am, vs))
    )
  b_t3 <- build(t3) %>%
    arrange(ord_layer_1, ord_layer_2, ord_layer_3)
  expect_equivalent(b_t3[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equivalent(b_t3[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equivalent(b_t3[, 3], tibble(row_label3 = as.character(rep(c(6, 8, 4), 4))))
  expect_equivalent(b_t3[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equivalent(b_t3[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equivalent(b_t3[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), 2))))
  expect_equivalent(b_t3[, 10], tibble(ord_layer_3 = as.numeric(rep(c(1, 2, 3), 4))))
})

test_that("A group_count layer can be ordered properly by counts", {
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(am,vs)) %>%
        set_order_count_method("bycount")
    )
  b_t1 <- build(t1) %>%
    arrange(ord_layer_1, ord_layer_2, ord_layer_2)
  expect_equivalent(b_t1[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equivalent(b_t1[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equivalent(b_t1[, 3], tibble(row_label3 = as.character(rep(c(4, 6, 8), 4))))
  expect_equivalent(b_t1[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equivalent(b_t1[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equivalent(b_t1[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), 2))))
  expect_equivalent(b_t1[, 10], tibble(ord_layer_3 = as.numeric(c(0, 0, 0,
                                                             2, 2, 0,
                                                             0, 2, 0,
                                                             6, 0, 0))))

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(am, vs)) %>%
        set_order_count_method("bycount") %>%
        set_ordering_cols("3")
    )
  b_t2 <- build(t2) %>%
    arrange(ord_layer_1, ord_layer_2, ord_layer_3)
  expect_equivalent(b_t2[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equivalent(b_t2[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equivalent(b_t2[, 3], tibble(row_label3 = as.character(c(4, 6, 8, 8, 4, 6, 4, 6, 8, 4, 6, 8))))
  expect_equivalent(b_t2[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equivalent(b_t2[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equivalent(b_t2[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), 2))))
  expect_equivalent(b_t2[, 10], tibble(ord_layer_3 = as.numeric(c(0, 0, 12,
                                                             0, 1, 2,
                                                             0, 0, 0,
                                                             0, 0, 0))))

})

test_that("A group_count layer can be ordered properly by a VARN", {
  mtcars$cylN <- mtcars$cyl
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(vs, am)) %>%
        set_order_count_method("byvarn")
    )
  b_t1 <- build(t1) %>%
    arrange(ord_layer_1, ord_layer_2)

  expect_equivalent(b_t1[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equivalent(b_t1[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equivalent(b_t1[, 3], tibble(row_label3 = as.character(rep(c(4, 6, 8), 4))))
  expect_equivalent(b_t1[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equivalent(b_t1[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equivalent(b_t1[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), 2))))
  expect_equivalent(b_t1[, 10], tibble(ord_layer_3 = as.numeric(rep(c(4, 6, 8), 4))))
})

test_that("A nested group_count layer can be ordered properly", {
  iris$treat <- rep(c("Group1", "Group2"), 75)
  iris$grp <- rep(c("A", "B", "C", "D", "E", "F"), each = 25)
  iris <- iris %>%
    mutate_all(as.character)

  t <- tplyr_table(iris, treat) %>%
    add_layer(
      group_count(vars(Species, grp))
    )
  b_t <- build(t)

  expect_equivalent(b_t[, 6], tibble(ord_layer_1 = as.integer(rep(c( 1, 2, 3), each = 3))))
})

test_that("A group_desc layer can be ordered properly", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(mpg, by = cyl) %>%
        set_format_strings(
          "n"         = f_str("xx", n),
          "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd)
        )
    )
  b_t <- build(t) %>%
    arrange(ord_layer_1, ord_layer_2)

  expect_equivalent(b_t[, 1], tibble(row_label1 = rep(c("4", "6", "8"), each = 2)))
  expect_equivalent(b_t[, 2], tibble(row_label2 = rep(c("n", "Mean (SD)"), 3)))
  expect_equivalent(b_t[, 7], tibble(ord_layer_1 = as.integer(rep(c(1, 2, 3), each = 2))))
  expect_equivalent(b_t[, 8], tibble(ord_layer_2 = as.integer(rep(c(1, 2), 3))))

})

##### Nested
adsl <- haven::read_xpt("adsl.xpt")
adsl$EOSSTTN <- unclass(as.factor(adsl$EOSSTT)) + 100
adsl$DCDECODN <- unclass(as.factor(adsl$DCDECOD)) + 100
adsl1 <- tplyr_table(adsl, TRT01A, cols = AGEGR1) %>%
  add_total_group() %>%
  add_treat_grps("T1&T2" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
  add_layer(
    group_count(vars(EOSSTT, DCDECOD)) %>%
      set_ordering_cols(Placebo, `65-80`) %>%
      set_result_order_var(n) %>%
      set_order_count_method(c("byvarn", "byvarn"))
  )
adsl_1 <- build(adsl1)
byvarn_out <- c(101, 101, 102, 102,
                 102, 102, 102, 102,
                 102, 102, 102)
byvarn_in <- c(Inf, 102, Inf, 101,
                103, 104, 105, 106,
                107, 108, 109)

adsl2 <- tplyr_table(adsl, TRT01A, cols = AGEGR1) %>%
  add_total_group() %>%
  add_treat_grps("T1&T2" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
  add_layer(
    group_count(vars(EOSSTT, DCDECOD)) %>%
      set_ordering_cols(Placebo, `65-80`) %>%
      set_result_order_var(n) %>%
      set_order_count_method(c("bycount", "bycount"))
  )
adsl_2 <- build(adsl2)
bycount_out <- c(30, 30, 12, 12,
                  12, 12, 12, 12,
                  12, 12, 12)
bycount_in <- c(Inf, 30, Inf, 2,
                 1, 0, 0, 0,
                 1, 1, 7)

adsl3  <- tplyr_table(adsl, TRT01A, cols = AGEGR1) %>%
  add_total_group() %>%
  add_treat_grps("T1&T2" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
  add_layer(
    group_count(vars(EOSSTT, DCDECOD)) %>%
      set_ordering_cols(Placebo, `65-80`) %>%
      set_result_order_var(n) %>%
      set_order_count_method(c("byfactor", "byfactor"))
  )
adsl_3 <- build(adsl3)
byfactor_out <- c(101, 101, 102, 102,
                   102, 102, 102, 102,
                   102, 102, 102)
byfactor_in <- c(Inf, 1, Inf, 1,
                  2, 3, 4, 5,
                  6, 7, 8)

test_that("Nested count layers are ordered properly", {

  expect_equivalent(adsl_1$ord_layer_1, byvarn_out)
  expect_equivalent(adsl_1$ord_layer_2, byvarn_in)
  expect_equivalent(adsl_2$ord_layer_1, bycount_out)
  expect_equivalent(adsl_2$ord_layer_2, bycount_in)
  expect_equivalent(adsl_3$ord_layer_1, byfactor_out)
  expect_equivalent(adsl_3$ord_layer_2, byfactor_in)

})

test_that("Sorting columns don't have names", {
  mtcars$cylN <- unclass(mtcars$cyl)

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_order_count_method("byvarn")
    ) %>%
    add_layer(
      group_count(cyl) %>%
        set_order_count_method("byfactor")
    ) %>%
    add_layer(
      group_count(cyl) %>%
        set_order_count_method("bycount")
    ) %>%
    build()

  expect_true(is.null(names(t$ord_layer_1)))
})
