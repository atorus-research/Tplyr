
test_that("tplyr_order orders columns of the output of a build properly", {

})

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
})

test_that("A group_count layer can be ordered properly by counts", {

})

test_that("A group_count layer can be ordered properly by a VARN", {

})

test_that("A nested group_count layer can be ordered properly", {

})

test_that("A group_desc layer can be ordered properly", {

})
