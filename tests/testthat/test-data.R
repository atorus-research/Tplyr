test_that("get_data_labels", {
  expect_snapshot(get_data_labels(adsl))
  expect_snapshot(get_data_labels(adae))
  expect_snapshot(get_data_labels(adas))
  expect_snapshot(get_data_labels(adlb))
})
