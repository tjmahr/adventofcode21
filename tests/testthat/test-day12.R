test_that("day 12 works", {
  example_data_12() |>
    f12a_traverse_caves() |>
    expect_length(10)

  example_data_12(2) |>
    f12a_traverse_caves() |>
    expect_length(19)

  example_data_12(3) |>
    f12a_traverse_caves() |>
    expect_length(226)

})
