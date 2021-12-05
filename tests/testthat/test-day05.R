test_that("day 05", {
  expect_equal(
    f05a_count_intersections(example_data_05()),
    5
  )
  expect_equal(
    f05a_count_intersections(example_data_05(), include_diagonals = TRUE),
    12
  )
})
