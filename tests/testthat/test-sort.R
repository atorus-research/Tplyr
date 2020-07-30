

test_that("A group_count layer can be ordered properly with factors", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl)
    )
  b_t <- build(t)
  expect_equal(b_t[, 1], tibble(row_label1 = as.character(c(4, 6, 8))))
  expect_equal(b_t[, 5], tibble(ord_layer_index = as.integer(c(1, 1, 1))))
  expect_equal(b_t[, 6], tibble(ord_layer_1 = as.numeric(c(1, 2, 3))))

  mtcars$cyl <- factor(mtcars$cyl, c(6, 8, 4))
  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl)
    )
  b_t2 <- build(t2)
  expect_equal(b_t2[, 1], tibble(row_label1 = as.character(c(6, 8, 4))))
  expect_equal(b_t2[, 5], tibble(ord_layer_index = as.integer(c(1, 1, 1))))
  expect_equal(b_t2[, 6], tibble(ord_layer_1 = as.numeric(c(1, 2, 3))))

  t3 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(am, vs))
    )
  b_t3 <- build(t3)
  expect_equal(b_t3[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equal(b_t3[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equal(b_t3[, 3], tibble(row_label3 = as.character(rep(c(4, 6, 8), each = 4))))
  expect_equal(b_t3[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equal(b_t3[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equal(b_t3[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), each = 2))))
  expect_equal(b_t3[, 10], tibble(ord_layer_3 = as.numeric(rep(c(1, 2, 3), 4))))
})

test_that("A group_count layer can be ordered properly by counts", {
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(am,vs)) %>%
        set_order_count_method("bycount")
    )
  b_t1 <- build(t1)
  expect_equal(b_t1[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equal(b_t1[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equal(b_t1[, 3], tibble(row_label3 = as.character(rep(c(4, 6, 8), each = 4))))
  expect_equal(b_t1[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equal(b_t1[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equal(b_t1[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), each = 2))))
  expect_equal(b_t1[, 10], tibble(ord_layer_3 = as.numeric(c(0, 0, 0,
                                                             2, 2, 0,
                                                             0, 2, 0,
                                                             6, 0, 0))))

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, by = vars(am, vs)) %>%
        set_order_count_method("bycount") %>%
        set_ordering_cols("3")
    )
  b_t2 <- build(t2)
  expect_equal(b_t2[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equal(b_t2[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equal(b_t2[, 3], tibble(row_label3 = as.character(rep(c(4, 6, 8), each = 4))))
  expect_equal(b_t2[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equal(b_t2[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equal(b_t2[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), each = 2))))
  expect_equal(b_t2[, 10], tibble(ord_layer_3 = as.numeric(c(0, 0, 12,
                                                             1, 2, 0,
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
  b_t1 <- build(t1)

  expect_equal(b_t1[, 1], tibble(row_label1 = as.character(rep(c(0, 1), each = 6))))
  expect_equal(b_t1[, 2], tibble(row_label2 = as.character(rep(c(0, 0, 0, 1, 1, 1), 2))))
  expect_equal(b_t1[, 3], tibble(row_label3 = as.character(rep(c(4, 6, 8), each = 4))))
  expect_equal(b_t1[, 7], tibble(ord_layer_index = as.integer(rep(1, 12))))
  expect_equal(b_t1[, 8], tibble(ord_layer_1 = as.integer(rep(c(1, 2), each = 6))))
  expect_equal(b_t1[, 9], tibble(ord_layer_2 = as.integer(rep(c(1, 1, 1, 2, 2, 2), each = 2))))
  expect_equal(b_t1[, 10], tibble(ord_layer_3 = as.numeric(rep(c(4, 6, 8), each = 4))))
})

test_that("A nested group_count layer can be ordered properly", {
  iris$treat <- rep(c("Group1", "Group2"), 75)
  iris$grp <- rep(c("A", "B", "C", "D", "E", "F"), each = 25)
  iris %<>% mutate_all(as.character)

  t <- tplyr_table(iris, treat) %>%
    add_layer(
      group_count(vars(Species, grp))
    )
  b_t <- build(t)

  expect_equal(b_t[, 6], tibble(ord_layer_2 = rep(c(Inf, 13, 12), 3)))
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
  b_t <- build(t)

  expect_equal(b_t[, 1], tibble(row_label1 = rep(c("4", "6", "8"), 2)))
  expect_equal(b_t[, 2], tibble(row_label2 = rep(c("n", "Mean (SD)"), 3)))
  expect_equal(b_t[, 7], tibble(ord_layer_1 = as.integer(rep(c(1, 2, 3), 2))))
  expect_equal(b_t[, 8], tibble(ord_layer_2 = 1:6))

})
