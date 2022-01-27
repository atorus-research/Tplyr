
## Numeric Data ----
test_that("Error handling - numeric", {

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(name='drat',
      group_desc(drat)
    ) %>%
    add_layer(name='cyl',
              group_count(cyl)
    )

  expect_snapshot_error(get_numeric_data(t, where=x==1))
  expect_snapshot_error(get_numeric_data(t, layer=c('drat', 'cyl'), where=x==1))
  expect_snapshot_error(get_numeric_data(t, layer='blah'))
  expect_snapshot_error(get_numeric_data(t, layer=c('drat','blah')))
  expect_snapshot_error(get_numeric_data(t, layer=10))
  expect_snapshot_error(get_numeric_data(t, layer=c(1, 10)))

})

t <- tplyr_table(mtcars, gear) %>%
  add_layer(name='drat',
            group_desc(drat)
  ) %>%
  add_layer(name='cyl',
            group_count(cyl)
  )

test_that("Numeric data is pulled from unbuilt table", {
  # Check that the necessary processing is run by get_numeric_data
  expect_true(!'built_target' %in% ls(envir = t))
  get_numeric_data(t)
  expect_true('built_target' %in% ls(envir = t))

})

test_that("No parameters gives a list of all numeric datasets", {
  # Check the types of everything
  dat_list <- get_numeric_data(t)
  expect_type(dat_list, 'list')
  expect_s3_class(dat_list[[1]], 'tbl_df')
  expect_s3_class(dat_list[[2]], 'tbl_df')
})

test_that("Requesting multiple layers gives a list of those layers' numeric datasets", {

  # Reorder to show that the selection works
  dat_list <- get_numeric_data(t, layer=c('cyl', 'drat'))
  # Check the types of everything
  expect_type(dat_list, 'list')
  expect_s3_class(dat_list[[1]], 'tbl_df')
  expect_s3_class(dat_list[[2]], 'tbl_df')
  expect_named(dat_list, c('cyl', 'drat'))

  expect_equal(dat_list, get_numeric_data(t, layer=c(2,1)))

})

test_that("Providing a layer returns a dataframe from that layer", {

  dat <- get_numeric_data(t, layer='cyl')
  dat2 <- get_numeric_data(t, layer=2)
  expect_equal(dat, t$layers$cyl$numeric_data)
  expect_equal(dat, dat2)

})

test_that("Providing a filter returns a filtered dataframe", {

  dat <- get_numeric_data(t, layer='drat', where=gear==3)
  expect_equal(dat, {
    t$layers$drat$numeric_data %>%
      filter(gear==3)
  }
  )

})

## Statistic Data ----
test_that("Error handling - statistic", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(name="carb",
              group_count(carb) %>%
                add_risk_diff(c('4', '3'))
    )

  expect_snapshot_error(get_stats_data(t, where=x==1))
  expect_snapshot_error(get_stats_data(t, layer=c(1, 2), where=x==1))
  expect_snapshot_error(get_stats_data(t, layer='blah'))
  expect_snapshot_error(get_stats_data(t, layer=c('am', 'blah')))
  expect_snapshot_error(get_stats_data(t, layer=10))
  expect_snapshot_error(get_stats_data(t, layer=c(1, 10)))

})

t <- tplyr_table(mtcars, gear) %>%
  add_layer(name='drat',
            group_desc(drat)
  ) %>%
  add_layer(name="cyl",
            group_count(cyl)
  ) %>%
  add_layer(name="am",
            group_count(am) %>%
              add_risk_diff(c('4', '3'))
  ) %>%
  add_layer(name="carb",
            group_count(carb) %>%
              add_risk_diff(c('4', '3'))
  )

test_that("Statistics data is pulled from unbuilt table", {
  # Check that the necessary processing is run by get_numeric_data
  expect_true(!'built_target' %in% ls(envir = t))
  suppressWarnings(get_numeric_data(t))
  expect_true('built_target' %in% ls(envir = t))
})

test_that("No parameters gives a list of statistics data in a list of layers", {
  dat_list <- get_stats_data(t)

  # Named list
  expect_type(dat_list, 'list')
  expect_named(dat_list, c('drat', 'cyl', 'am', 'carb'))
  # Elements of list contain a list
  walk(dat_list, expect_type, type='list')
  # No stats on first two - empty
  expect_equal(map_int(dat_list, length), c(0, 0, 1, 1), ignore_attr = TRUE)
  # Last 2 have dataframes
  expect_s3_class(dat_list[[3]][[1]], 'tbl_df')
  expect_s3_class(dat_list[[4]][[1]], 'tbl_df')


})

test_that("Multiple layers gives a list of statistics data in a list for those layers", {
  dat_list <- get_stats_data(t)

  # Named list
  expect_type(dat_list, 'list')
  expect_named(dat_list, c('drat', 'cyl', 'am', 'carb'))
  # Elements of list contain a list
  walk(dat_list, expect_type, type='list')
  # No stats on first two - empty
  expect_equal(map_int(dat_list, length), c(0, 0, 1, 1), ignore_attr = TRUE)
  # Last 2 have dataframes
  expect_s3_class(dat_list[[3]][[1]], 'tbl_df')
  expect_s3_class(dat_list[[4]][[1]], 'tbl_df')

  dat_list <- get_stats_data(t, layer=c("carb", "am"))

  expect_type(dat_list, 'list')
  expect_named(dat_list, c('carb', 'am'))
  # Elements of list contain a list
  walk(dat_list, expect_type, type='list')
  # No stats on first two - empty
  expect_equal(map_int(dat_list, length), c(1, 1), ignore_attr = TRUE)
  # Last 2 have dataframes
  expect_s3_class(dat_list[[1]][[1]], 'tbl_df')
  expect_s3_class(dat_list[[2]][[1]], 'tbl_df')

  expect_equal(dat_list, get_stats_data(t, layer=c(4, 3)))


})

test_that("Providing a layer returns a list of statistics from that layer", {

  stats <- get_stats_data(t, layer='drat')
  expect_equal(stats, list())
  stats <- get_stats_data(t, layer='am')
  stats2 <- get_stats_data(t, layer=3)
  expect_equal(stats, map(t$layers$am$stats, ~ env_get(env=.x, 'stats_numeric_data')))
  expect_equal(stats, stats2)

})

test_that("Providing a statistic returns a list of dataframes for that statistic", {

  dat_list <- get_stats_data(t, statistic='riskdiff')
  expect_type(dat_list, 'list')
  expect_equal(dat_list[[1]], NULL)
  expect_equal(dat_list[[4]], env_get(env=t$layers$carb$stats$riskdiff, 'stats_numeric_data'))

})

test_that("Providing a layer and a statistic returns a dataframe matching that criteria", {
  dat <- get_stats_data(t, layer='carb', statistic='riskdiff')
  expect_equal(dat, env_get(env=t$layers$carb$stats$riskdiff, 'stats_numeric_data'))
})

test_that("Providing a filter returns a filtered dataframe", {

  dat <- get_stats_data(t, layer='carb', statistic='riskdiff', where=summary_var==1)
  expect_equal(dat, {
    env_get(env=t$layers$carb$stats$riskdiff, 'stats_numeric_data') %>%
      filter(summary_var==1)
    })
})
