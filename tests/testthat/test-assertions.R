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

test_that("Placeholder", {
  expect_true(TRUE)
})
