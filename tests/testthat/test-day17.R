test_that("day 17", {
  x <- f17_helper(example_data_17())
  p1 <- f17_make_probe(c(0, 0), c(7, 2), target = x)
  p1$step_all()

  p1$get_history()[["in_target"]] |>
    any() |>
    expect_true()

  p1 <- f17_make_probe(c(0, 0), c(6, 3), target = x)
  p1$step_all()
  p1$get_history()[["in_target"]] |>
    any() |>
    expect_true()

  p1 <- f17_make_probe(c(0, 0), c(17, -4), target = x)
  p1$step_all()
  p1$get_history()[["in_target"]] |>
    any() |>
    expect_false()

  example_data_17() |>
    f17a_maximize_height_old() |>
    expect_equal(45)

  example_data_17() |>
    f17b_count_velocities() |>
    nrow() |>
    expect_equal(112)

  example_data_17() |>
    f17b_count_velocities() |>
    getElement("highest") |>
    max() |>
    expect_equal(45)
})
