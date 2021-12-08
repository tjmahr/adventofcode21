test_that("day 08 works", {
  example_data_08(2) |>
    f08a_count_1478s() |>
    expect_equal(26)

  example_data_08(1) |>
    f08b_solve_and_sum_output() |>
    expect_equal(5353)

  example_data_08(2) |>
    f08b_solve_and_sum_output() |>
    expect_equal(61229)
})
