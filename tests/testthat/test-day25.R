test_that("day 25", {
  s0 <- example_data_25(1)|> f25_prepare_grid()
  s1 <- example_data_25(2)|> f25_prepare_grid()
  s2 <- example_data_25(3)|> f25_prepare_grid()
  s3 <- example_data_25(4)|> f25_prepare_grid()

  s0 |>
    f25_step_once() |>
    expect_equal(s1) |>
    f25_step_once() |>
    expect_equal(s2) |>
    f25_step_once() |>
    expect_equal(s3)

  l <- f25a_find_stable_grid(example_data_25(1))
  expect_equal(l$n, 58)
})
