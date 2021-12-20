test_that("day 20", {
  x <- example_data_20()
  m <- f20_enhance_n_times(x, 2)
  expect_equal(m, 35)

  m <- f20_enhance_n_times(x, 50)
  expect_equal(m, 3351)
})
