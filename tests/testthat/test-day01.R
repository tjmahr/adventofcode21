test_that("day 1", {
  x <- example_data_01()
  expect_equal(f01_count_increases(x), 7)
  expect_equal(f01_count_shingled_increases(x), 5)
})
