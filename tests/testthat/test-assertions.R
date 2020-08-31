#
# my_func <- function(){
#   assert_that(is_class(2, 'character', 'some_param'))
# }
#
# my_func()
#
# test1 <- function() {
#   my_func()
# }
# test1()
#
# test2 <- function(){
#   test1()
# }
# test2()
#
#
# my_func <- function(my_param) {
#   assert_that(is_class(my_param, 'character'))
# }
# my_func(2)

# Issues with the call stack in tests
# test_that("assert_is_layer_raises errors properly", {
#   tab <- tplyr_table(iris, Species)
#   l <- group_count(tab, Species)
#
#   expect_silent(assert_is_layer(l))
#   expect_error(assert_is_layer(tab), "asdf")
#   expect_error(assert_is_layer(list()), "asdf")
# })
#
# test_that("assert_is_table raises errors properly", {
#   tab <- tplyr_table(iris, Species)
#   l <- group_count(tab, Species)
#
#   expect_silent(assert_is_table(tab))
#   expect_error(assert_is_table(l), "asdf")
#   expect_error(assert_is_table(list()), "asdf")
# })

