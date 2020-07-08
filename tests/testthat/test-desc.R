# Desc Layer

# TODO: Remove the type coersion when the build can handle different types
mtcars$cyl <- as.character(mtcars$cyl)
mtcars$gear <- as.character(mtcars$gear)
# This is used for nesting counts
mtcars$grp <- rep(c("A", "B", "C", "D"), each = 8)
t1 <- tplyr_table(mtcars, gear)
t2 <- tplyr_table(mtcars, gear)
t3 <- tplyr_table(mtcars, gear)
t4 <- tplyr_table(mtcars, gear)
t5 <- tplyr_table(mtcars, gear)

d1 <- group_desc(t1, mpg)
d2 <- group_desc(t2, mpg, by = am)
d3 <- group_desc(t3, mpg, by = vars(am, vs))
# d4 <- group_desc(t4, mpg) %>%
#   set_custom_summaries(mean_squared = mean(mpg, na.rm=TRUE)**2) %>%
#   set_format_strings(
#     "Mean Squared" = f_str("xx.xx", mean_squared)
#   )
d5 <- group_desc(t5, vars(mpg, gear))

t1 <- add_layers(t1, d1)
t2 <- add_layers(t2, d2)
t3 <- add_layers(t3, d3)
# t4 <- add_layers(t4, d4)
t5 <- add_layers(t5, d5)

test_that("Group_desc can be created without warnings and errors", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  # expect_silent(build(t4))
  expect_silent(build(t5))
})

test_that("group_desc are built as expected", {
  expect_length(d1, 6)
  expect_length(d2, 6)
  expect_length(d3, 6)
  # expect_length(d4, 6)
  expect_length(d5, 6)
})
