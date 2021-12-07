test_that("day 07", {
  example_data_07() |> f07a_optimize_fuel_linear() |> expect_equal(37)
  example_data_07() |> f07b_optimize_fuel_triangular() |> expect_equal(168)
})
