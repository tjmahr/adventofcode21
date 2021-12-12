test_that("day 12 works", {
  example_data_12() |>
    f12a_traverse_caves_once() |>
    expect_length(10)

  example_data_12(2) |>
    f12a_traverse_caves_once() |>
    expect_length(19)

  example_data_12(3) |>
    f12a_traverse_caves_once() |>
    expect_length(226)

  example_data_12() |>
    f12b_traverse_caves_maybe_twice() |>
    expect_length(36)

  example_data_12(2) |>
    f12b_traverse_caves_maybe_twice() |>
    expect_length(103)

  example_data_12(3) |>
    f12b_traverse_caves_maybe_twice() |>
    expect_length(3509)

})
