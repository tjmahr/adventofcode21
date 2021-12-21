test_that("day 21+", {
  x <- example_data_21()
  g <- f21_new_game(x)
  g <- g$roll()$roll_n(7)
  h <-  g$get_history()
  expect_equal(h$active, c(1, 2, 1, 2, 1, 2, 1, 2))
  expect_equal(h$score, c(10, 3, 14, 9, 20, 16, 26, 22))

  f21a_play_deterministic_game(x) |>
    expect_equal(739785)
})
