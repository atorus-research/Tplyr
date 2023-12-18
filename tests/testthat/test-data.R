test_that("get_data_labels", {
  expect_snapshot(get_data_labels(tplyr_adsl))
  expect_snapshot(get_data_labels(tplyr_adae))
  expect_snapshot(get_data_labels(tplyr_adas))
  expect_snapshot(get_data_labels(tplyr_adlb))
})
