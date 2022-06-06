test_that("apply_formats works correctly applies f_str() formatting", {
  out <- mtcars %>%
    head(10) %>%
    mutate(
      fmt_example = apply_formats('xxx (xx.x)', hp, wt)
    )

  comp <- c("110 ( 2.6)", "110 ( 2.9)", " 93 ( 2.3)", "110 ( 3.2)", "175 ( 3.4)",
            "105 ( 3.5)", "245 ( 3.6)", " 62 ( 3.2)", " 95 ( 3.1)", "123 ( 3.4)")
  expect_equal(out$fmt_example, comp)

  expect_snapshot_error({
    mtcars %>%
      head(10) %>%
      mutate(
        fmt_example = apply_formats('a (xx.a)', hp, wt)
      )
  })
})
