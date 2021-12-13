test_that("day 13", {
  example_data_13() |>
    f13a_fold_once() |>
    unique() |>
    nrow() |>
    expect_equal(17)
})
