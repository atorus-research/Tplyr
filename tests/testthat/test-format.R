
test_that("Format string must be character", {
  expect_snapshot_error(f_str(123)) # Need to come back to issues with assert_has_class to address the fact that
  # error messages don't work in testthat
})

# Errors are generated when too few or too many variables
test_that("Error is thrown when format doesn't match variables", {
  expect_snapshot_error(f_str("xx.x xx.x", a))
  expect_snapshot_error(f_str("xx.x", a, b))
})

# Variables are picked up appropriately
test_that("Variables are properly captured", {
  fmt1 <- f_str('xx', a)
  fmt2 <- f_str('xx xx', a, b)
  fmt3 <- f_str('xx xx xx', a, b, c)

  expect_equal(unname(map(quos(a), as_name)), map(fmt1$vars, as_name))
  expect_equal(unname(map(quos(a, b), as_name)), map(fmt2$vars, as_name))
  expect_equal(unname(map(quos(a, b, c), as_name)), map(fmt3$vars, as_name))
})

# Set some f_str objects to be reused across multiple tests
fmt1 <- f_str("xx", a)
fmt2 <- f_str("a", a)
fmt3 <- f_str("a+1", a)
fmt4 <- f_str("a+2", a)
fmt5 <- f_str("xx.x", a)
fmt6 <- f_str("xx.xx", a)
fmt7 <- f_str("a.xx", a)
fmt8 <- f_str("xx.a", a)
fmt9 <- f_str("a+1.xx", a)
fmt10 <- f_str("a+2.xx", a)
fmt11 <- f_str("xx.a+1", a)
fmt12 <- f_str("xx.a+2", a)
fmt13 <- f_str("xx xx", a, b)
fmt14 <- f_str("xx.x xx", a, b)
fmt15 <- f_str("xx.x xx.xx", a, b)
fmt16 <- f_str("xx.a xx.a+1", a, b)
fmt17 <- f_str("xx.a (xx.a+2)", a, b)
fmt18 <- f_str("xx.a (xx.a+2%) [a.xx]", a, b, c)
fmt19 <- f_str("xx.a --->>>> xx.xx <<<------ a%, xx.a+1%%", a, b, c, d)
fmt20 <- f_str("xx.a+12 --->>>> xx.xx <<<------ a+123%, xx.a+1234%%", a, b, c, d)

# Regex tests
test_that("Format strings are parsed correctly", {
  expect_equal(fmt1$formats, "xx")
  expect_equal(fmt2$formats, "a")
  expect_equal(fmt3$formats, "a+1")
  expect_equal(fmt4$formats, "a+2")
  expect_equal(fmt5$formats, "xx.x")
  expect_equal(fmt6$formats, "xx.xx")
  expect_equal(fmt7$formats, "a.xx")
  expect_equal(fmt8$formats, "xx.a")
  expect_equal(fmt9$formats, "a+1.xx")
  expect_equal(fmt10$formats, "a+2.xx")
  expect_equal(fmt11$formats, "xx.a+1")
  expect_equal(fmt12$formats, "xx.a+2")
  expect_equal(fmt13$formats, c("xx", "xx"))
  expect_equal(fmt14$formats, c("xx.x", "xx"))
  expect_equal(fmt15$formats, c("xx.x", "xx.xx"))
  expect_equal(fmt16$formats, c("xx.a", "xx.a+1"))
  expect_equal(fmt17$formats, c("xx.a", "xx.a+2"))
  expect_equal(fmt18$formats, c("xx.a", "xx.a+2", "a.xx"))
  expect_equal(fmt19$formats, c("xx.a", "xx.xx", "a", "xx.a+1"))
  expect_equal(fmt20$formats, c("xx.a+12", "xx.xx", "a+123", "xx.a+1234"))

})

# sprintf strings are formatted properly
test_that("Replacement strings are parsed correctly", {
  expect_equal(fmt1$repl_str, "%s")
  expect_equal(fmt2$repl_str, "%s")
  expect_equal(fmt3$repl_str, "%s")
  expect_equal(fmt4$repl_str, "%s")
  expect_equal(fmt5$repl_str, "%s")
  expect_equal(fmt6$repl_str, "%s")
  expect_equal(fmt7$repl_str, "%s")
  expect_equal(fmt8$repl_str, "%s")
  expect_equal(fmt9$repl_str, "%s")
  expect_equal(fmt10$repl_str, "%s")
  expect_equal(fmt11$repl_str, "%s")
  expect_equal(fmt12$repl_str, "%s")
  expect_equal(fmt13$repl_str, "%s %s")
  expect_equal(fmt14$repl_str, "%s %s")
  expect_equal(fmt15$repl_str, "%s %s")
  expect_equal(fmt16$repl_str, "%s %s")
  expect_equal(fmt17$repl_str, "%s (%s)")
  expect_equal(fmt18$repl_str, "%s (%s%%) [%s]")
  expect_equal(fmt19$repl_str, "%s --->>>> %s <<<------ %s%%, %s%%%%")
  expect_equal(fmt20$repl_str, "%s --->>>> %s <<<------ %s%%, %s%%%%")
})

# Auto precision is detected and precision formats are properly set
test_that("Format string setting and autoprecision are detected appropriately", {

  #f_str("xx", a)
  s1 <- list(c('int'=2,'dec'=0))
  attr(s1[[1]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  expect_equal(fmt1$settings, s1)

  #f_str("a", a)
  s2 <- list(c('int'=0,'dec'=0))
  attr(s2[[1]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt2$settings, s2)

  #f_str("a+1", a)
  s3 <- list(c('int'=1,'dec'=0))
  attr(s3[[1]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt3$settings, s3)

  #f_str("a+2", a)
  s4 <- list(c('int'=2,'dec'=0))
  attr(s4[[1]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt4$settings, s4)

  #f_str("xx.x", a)
  s5 <- list(c('int'=2,'dec'=1))
  attr(s5[[1]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  expect_equal(fmt5$settings, s5)

  #f_str("xx.xx", a)
  s6 <- list(c('int'=2,'dec'=2))
  attr(s6[[1]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  expect_equal(fmt6$settings, s6)

  #f_str("a.xx", a)
  s7 <- list(c('int'=0,'dec'=2))
  attr(s7[[1]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt7$settings, s7)

  #f_str("xx.a", a)
  s8 <- list(c('int'=2,'dec'=0))
  attr(s8[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt8$settings, s8)

  #f_str("a+1.xx", a)
  s9 <- list(c('int'=1,'dec'=2))
  attr(s9[[1]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt9$settings, s9)

  #f_str("a+2.xx", a)
  s10 <- list(c('int'=2,'dec'=2))
  attr(s10[[1]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt10$settings, s10)

  #f_str("xx.a+1", a)
  s11 <- list(c('int'=2,'dec'=1))
  attr(s11[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt11$settings, s11)

  #f_str("xx.a+2", a)
  s12 <- list(c('int'=2,'dec'=2))
  attr(s12[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt12$settings, s12)

  #f_str("xx xx", a, b)
  s13 <- list(c('int'=2,'dec'=0), c('int'=2,'dec'=0))
  attr(s13[[1]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  attr(s13[[2]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  expect_equal(fmt13$settings, s13)

  #f_str("xx.x xx", a, b)
  s14 <- list(c('int'=2,'dec'=1), c('int'=2,'dec'=0))
  attr(s14[[1]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  attr(s14[[2]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  expect_equal(fmt14$settings, s14)

  #f_str("xx.x xx.xx", a, b)
  s15 <- list(c('int'=2,'dec'=1), c('int'=2,'dec'=2))
  attr(s15[[1]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  attr(s15[[2]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  expect_equal(fmt15$settings, s15)

  #f_str("xx.a xx.a+1", a, b)
  s16 <- list(c('int'=2,'dec'=0), c('int'=2,'dec'=1))
  attr(s16[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  attr(s16[[2]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt16$settings, s16)

  #f_str("xx.a (xx.a+2)", a, b)
  s17 <- list(c('int'=2,'dec'=0), c('int'=2,'dec'=2))
  attr(s17[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  attr(s17[[2]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt17$settings, s17)

  #f_str("xx.a (xx.a+2%) [a.xx]", a, b, c)
  s18 <- list(c('int'=2,'dec'=0), c('int'=2,'dec'=2), c('int'=0,'dec'=2))
  attr(s18[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  attr(s18[[2]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  attr(s18[[3]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  expect_equal(fmt18$settings, s18)

  #f_str("xx.a --->>>> xx.xx <<<------ a%, xx.a+1%%", a, b, c, d)
  s19 <- list(c('int'=2,'dec'=0), c('int'=2,'dec'=2), c('int'=0,'dec'=0), c('int'=2,'dec'=1))
  attr(s19[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  attr(s19[[2]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  attr(s19[[3]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  attr(s19[[4]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt19$settings, s19)

  #f_str("xx.a+12 --->>>> xx.xx <<<------ a+123%, xx.a+1234%%", a, b, c, d)
  s20 <- list(c('int'=2,'dec'=12), c('int'=2,'dec'=2), c('int'=123,'dec'=0), c('int'=2,'dec'=1234))
  attr(s20[[1]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  attr(s20[[2]], 'auto') <- c('int'=FALSE, 'dec'=FALSE)
  attr(s20[[3]], 'auto') <- c('int'=TRUE, 'dec'=FALSE)
  attr(s20[[4]], 'auto') <- c('int'=FALSE, 'dec'=TRUE)
  expect_equal(fmt20$settings, s20)

})


