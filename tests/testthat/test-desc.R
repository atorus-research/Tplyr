# Desc Layer
mtcars_long <- mtcars %>%
  rownames_to_column(var = "model") %>%
  pivot_longer(cols = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'))

t1 <- tplyr_table(mtcars, gear)
t2 <- tplyr_table(mtcars, gear)
t3 <- tplyr_table(mtcars, gear)
t4 <- tplyr_table(mtcars, gear)
t5 <- tplyr_table(mtcars, gear)
t6 <- tplyr_table(mtcars, gear)
t7 <- tplyr_table(mtcars, gear, cols=vs)
t8 <- tplyr_table(mtcars, gear, cols=vs)

d1 <- group_desc(t1, mpg)
d2 <- group_desc(t2, mpg, by = am)
d3 <- group_desc(t3, mpg, by = vars(am, vs))
d4 <- group_desc(t4, mpg) %>%
  set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
  set_format_strings(
    "Mean Squared" = f_str("xx.xx", mean_squared)
  )
d5 <- group_desc(t5, vars(mpg, wt))
# Update for custom summaries - two target variables
d6 <- group_desc(t6, vars(mpg, wt)) %>%
  set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
  set_format_strings(
    "Mean Squared" = f_str("xx.xx", mean_squared)
  )
d7 <- group_desc(t7, mpg)
d8 <- group_desc(t7, mpg, by = carb)

t1 <- add_layers(t1, d1)
t2 <- add_layers(t2, d2)
t3 <- add_layers(t3, d3)
t4 <- add_layers(t4, d4)
t5 <- add_layers(t5, d5)
t6 <- add_layers(t6, d6)
t7 <- add_layers(t7, d7)
t8 <- add_layers(t8, d8)

test_that("group_desc are built as expected", {
  expect_setequal(names(d1), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(d2), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(d3), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(d4), c("max_length", "format_strings", "by", "cap",
                                "row_labels", "stats", "keep_vars",
                                "precision_on", "custom_summaries", "where",
                                "target_var", "need_prec_table", "precision_by",
                                "trans_vars", "summary_vars", "layers" ))
  expect_setequal(names(d5), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(d6), c("max_length", "format_strings", "by", "cap",
                                "row_labels", "stats", "keep_vars",
                                "precision_on", "custom_summaries", "where",
                                "target_var", "need_prec_table", "precision_by",
                                "trans_vars", "summary_vars", "layers"))
  expect_setequal(names(d7), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(d8), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
})

test_that("Group_desc can be created without warnings and errors", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
  expect_silent(build(t6))
  expect_silent(build(t7))
  expect_silent(build(t8))
})

test_that("group_desc are processed as expected", {

  expect_setequal(names(d1), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "prec"))
  expect_setequal(names(d2), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "prec"))
  expect_setequal(names(d3), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "prec"))
  expect_setequal(names(d4), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "custom_summaries"))
  expect_setequal(names(d5), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "prec"))
  expect_setequal(names(d6), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "custom_summaries"))
  expect_setequal(names(d7), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "prec"))
  expect_setequal(names(d7), c("where", "need_prec_table", "built_target",
                               "summary_vars", "formatted_data", "trans_vars",
                               "target_var", "keep_vars", "cap", "precision_on",
                               "max_length", "layers", "row_labels", "by",
                               "precision_by", "stats", "numeric_data",
                               "format_strings", "prec"))

  expect_equal(dim(d1$numeric_data), c(27, 4))
  expect_equal(dim(d2$numeric_data), c(54, 5))
  expect_equal(dim(d3$numeric_data), c(108, 6))
  expect_equal(dim(d4$numeric_data), c(3, 4))
  expect_equal(dim(d5$numeric_data), c(54, 4))
  expect_equal(dim(d6$numeric_data), c(6, 4))
  expect_equal(dim(d7$numeric_data), c(54, 5))
  expect_equal(dim(d8$numeric_data), c(324, 6))


  expect_type(d1$numeric_data$value, "double")
  expect_type(d2$numeric_data$value, "double")
  expect_type(d3$numeric_data$value, "double")
  expect_type(d4$numeric_data$value, "double")
  expect_type(d5$numeric_data$value, "double")
  expect_type(d6$numeric_data$value, "double")
  expect_type(d7$numeric_data$value, "double")
  expect_type(d8$numeric_data$value, "double")

  expect_equal(dim(d1$formatted_data), c(6, 5))
  expect_equal(dim(d2$formatted_data), c(12, 7))
  expect_equal(dim(d3$formatted_data), c(24, 9))
  expect_equal(dim(d4$formatted_data), c(1, 5))
  expect_equal(dim(d5$formatted_data), c(6, 8))
  expect_equal(dim(d6$formatted_data), c(1, 8))
  expect_equal(dim(d7$formatted_data), c(6, 8))
  expect_equal(dim(d8$formatted_data), c(36, 10))


  expect_true(!any(is.na(unlist(d1$formatted_data[, 2:4]))))
  expect_true(!any(is.na(unlist(d2$formatted_data[, 2:4]))))
  expect_true(!any(is.na(unlist(d3$formatted_data[, 4:6]))))
  expect_true(!any(is.na(unlist(d4$formatted_data[, 2:4]))))
  expect_true(!any(is.na(unlist(d5$formatted_data[, 2:7]))))
  expect_true(!any(is.na(unlist(d6$formatted_data[, 2:7]))))
  expect_true(!any(is.na(unlist(d7$formatted_data[, 2:7]))))
  expect_true(!any(is.na(unlist(d8$formatted_data[, 3:8]))))

})

test_that("Auto precision builds correctly", {

  t_uncap <- tplyr_table(mtcars_long, gear) %>%
    add_layer(
      group_desc(value, by = name) %>%
        set_format_strings(
          'n' = f_str('xxx', n),
          'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
          'Variance' = f_str('a.a+2', var),
          'Q1, Median, Q3' = f_str('a.a, a.a, a.a', q1, median, q3),
          'IQR' = f_str('a', iqr),
          'Missing' = f_str('xxx', missing)
        )
    ) %>%
    build() %>%
    mutate_at(vars(starts_with('var')), ~ str_trim(.x)) # Reading in the CSV removes leading spaces

  t_cap <- tplyr_table(mtcars_long, gear) %>%
    add_layer(
      group_desc(value, by = name) %>%
        set_format_strings(
          'n' = f_str('xxx', n),
          'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
          'Variance' = f_str('a.a+2', var),
          'Q1, Median, Q3' = f_str('a.a, a.a, a.a', q1, median, q3),
          'IQR' = f_str('a', iqr),
          'Missing' = f_str('xxx', missing),
          cap = c('int'=3, 'dec'=2)
        )
    ) %>%
    build() %>%
    mutate_at(vars(starts_with('var')), ~ str_trim(.x)) # Reading in the CSV removes leading spaces

  t_uncap_comp <- readr::read_csv('t_uncap.csv')
  t_cap_comp <- readr::read_csv('t_cap.csv')

  expect_equal(mutate_all(t_uncap, as.character),
               mutate_all(t_uncap_comp, as.character), ignore_attr = TRUE)
  expect_equal(mutate_all(t_cap, as.character),
               mutate_all(t_cap_comp, as.character), ignore_attr = TRUE)

})

test_that("Desc layer clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_desc(drat, where=bad == code)
    )

  expect_snapshot_error(build(t))
})
