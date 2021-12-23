test_that("day 22", {
  example_data_22(1) |>
    f22a_flip_cubes_small() |>
    expect_equal(39)

  example_data_22(2) |>
    f22a_flip_cubes_small() |>
    expect_equal(590784)

  example_data_22(2)[1:20] |>
    f22b_flip_cubes_big() |>
    expect_equal(590784)
})
