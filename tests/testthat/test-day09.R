test_that("day 09 works", {
  example_data_09() |>
    f09a_find_lava_risk_level() |>
    expect_equal(15)

  example_data_09() |>
    f09b_find_basins() |>
    expect_equal(1134)
})
