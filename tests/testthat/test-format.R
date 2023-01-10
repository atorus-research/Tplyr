
test_that("Format string must be character", {
  expect_snapshot_error(f_str(123)) # Need to come back to issues with assert_has_class to address the fact that
  # error messages don't work in testthat
})

# Errors are generated when too few or too many variables
test_that("Error is thrown when format doesn't match variables", {
  expect_snapshot_error(f_str("xx.x xx.x", a))
  expect_snapshot_error(f_str("xx.x", a, b))
})

# Hug character errors generate properly
test_that("Hug character format string errors generate properly in f_str()", {
  expect_error(f_str("(AA.a+1)", a), "*only use a single 'A' or 'a'*")
  expect_error(f_str("XX", a), "*an 'X' or 'A' was used but no hug character*")
  expect_error(f_str("xx.X", a), "*'X' or 'A' can only be used*")
  expect_error(f_str("(X.X)", a), "*The following section failed to parse*")
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
fmt21 <- f_str("(X.x)", a)
fmt22 <- f_str("(A.x)", a)
fmt23 <- f_str("(A+1.x)", a)
fmt24 <- f_str("({[X.x)]}", a)
fmt25 <- f_str("({[A.x)]}", a)
fmt26 <- f_str("({[A+1.x)]}", a)


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
  expect_equal(fmt21$formats, c("(X.x"))
  expect_equal(fmt22$formats, c("(A.x"))
  expect_equal(fmt23$formats, c("(A+1.x"))
  expect_equal(fmt24$formats, c("({[X.x"))
  expect_equal(fmt25$formats, c("({[A.x"))
  expect_equal(fmt26$formats, c("({[A+1.x"))

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
  expect_equal(fmt21$repl_str, "%s)")
  expect_equal(fmt22$repl_str, "%s)")
  expect_equal(fmt23$repl_str, "%s)")
  expect_equal(fmt24$repl_str, "%s)]}")
  expect_equal(fmt25$repl_str, "%s)]}")
  expect_equal(fmt26$repl_str, "%s)]}")
})

# Auto precision is detected and precision formats are properly set
test_that("Format string setting and autoprecision are detected appropriately", {

  #f_str("xx", a)
  s1 <- list(list(int=2, dec=0, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt1$settings, s1)

  #f_str("a", a)
  s2 <- list(list(int=0, dec=0, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt2$settings, s2)

  #f_str("a+1", a)
  s3 <- list(list(int=1, dec=0, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt3$settings, s3)

  #f_str("a+2", a)
  s4 <- list(list(int=2, dec=0, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt4$settings, s4)

  #f_str("xx.x", a)
  s5 <- list(list(int=2, dec=1, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt5$settings, s5)

  #f_str("xx.xx", a)
  s6 <- list(list(int=2, dec=2, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt6$settings, s6)

  #f_str("a.xx", a)
  s7 <- list(list(int=0, dec=2, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt7$settings, s7)

  #f_str("xx.a", a)
  s8 <- list(list(int=2, dec=0, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_))
  expect_equal(fmt8$settings, s8)

  #f_str("a+1.xx", a)
  s9 <- list(list(int=1, dec=2, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt9$settings, s9)

  #f_str("a+2.xx", a)
  s10 <- list(list(int=2, dec=2, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_))
  expect_equal(fmt10$settings, s10)

  #f_str("xx.a+1", a)
  s11 <- list(list(int=2, dec=1, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_))
  expect_equal(fmt11$settings, s11)

  #f_str("xx.a+2", a)
  s12 <- list(list(int=2, dec=2, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_))
  expect_equal(fmt12$settings, s12)

  #f_str("xx xx", a, b)
  s13 <- list(
    list(int=2, dec=0, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=2, dec=0, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_)
    )
  expect_equal(fmt13$settings, s13)

  #f_str("xx.x xx", a, b)
  s14 <- list(
    list(int=2, dec=1, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=2, dec=0, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_)
  )
  expect_equal(fmt14$settings, s14)

  #f_str("xx.x xx.xx", a, b)
  s15 <- list(
    list(int=2, dec=1, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=2, dec=2, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_)
  )
  expect_equal(fmt15$settings, s15)

  #f_str("xx.a xx.a+1", a, b)
  s16 <- list(
    list(int=2, dec=0, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_),
    list(int=2, dec=1, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_)
  )
  expect_equal(fmt16$settings, s16)

  #f_str("xx.a (xx.a+2)", a, b)
  s17 <- list(
    list(int=2, dec=0, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_),
    list(int=2, dec=2, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_)
  )
  expect_equal(fmt17$settings, s17)

  #f_str("xx.a (xx.a+2%) [a.xx]", a, b, c)
  s18 <- list(
    list(int=2, dec=0, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_),
    list(int=2, dec=2, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_),
    list(int=0, dec=2, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_)
  )
  expect_equal(fmt18$settings, s18)

  #f_str("xx.a --->>>> xx.xx <<<------ a%, xx.a+1%%", a, b, c, d)
  s19 <- list(
    list(int=2, dec=0, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_),
    list(int=2, dec=2, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=0, dec=0, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=2, dec=1, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_)
  )
  expect_equal(fmt19$settings, s19)

  #f_str("xx.a+12 --->>>> xx.xx <<<------ a+123%, xx.a+1234%%", a, b, c, d)
  s20 <- list(
    list(int=2, dec=12, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_),
    list(int=2, dec=2, auto_int=FALSE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=123, dec=0, auto_int=TRUE, auto_dec=FALSE, hug_char=NA_character_),
    list(int=2, dec=1234, auto_int=FALSE, auto_dec=TRUE, hug_char=NA_character_)
  )
  expect_equal(fmt20$settings, s20)

  # f_str("(X.x)", a)
  s21 <- list(list(int=2, dec=1, auto_int=FALSE, auto_dec=FALSE, hug_char="("))
  expect_equal(fmt21$settings, s21)

  # f_str("(A.x)", a)
  s22 <- list(list(int=0, dec=1, auto_int=TRUE, auto_dec=FALSE, hug_char="("))
  expect_equal(fmt22$settings, s22)

  # f_str("(A+1.x)", a)
  s23 <- list(list(int=1, dec=1, auto_int=TRUE, auto_dec=FALSE, hug_char="("))
  expect_equal(fmt23$settings, s23)

  # f_str("({[X.x)]}", a)
  s24 <- list(list(int=4, dec=1, auto_int=FALSE, auto_dec=FALSE, hug_char="({["))
  expect_equal(fmt24$settings, s24)

  # f_str("({[A.x)]}", a)
  s25 <- list(list(int=2, dec=1, auto_int=TRUE, auto_dec=FALSE, hug_char="({["))
  expect_equal(fmt25$settings, s25)

  # f_str("({[A+1.a+1)]}", a)
  s26 <- list(list(int=3, dec=1, auto_int=TRUE, auto_dec=FALSE, hug_char="({["))
  expect_equal(fmt26$settings, s26)

})
