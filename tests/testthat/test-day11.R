test_that("day 11", {
  x <- example_data_11(1)
  x |> f11a_run_n_steps(1) |> expect_equal(9)

  x <- example_data_11(2)
  x |> f11a_run_n_steps(1) |> expect_equal(0)
  x |> f11a_run_n_steps(2) |> expect_equal(c(0, 35))
  x |> f11a_run_n_steps(10) |> sum() |> expect_equal(204)
  x |> f11a_run_n_steps(100) |> sum() |> expect_equal(1656)

  x |> f11b_run_until_all_flash() |> expect_equal(195)
})
