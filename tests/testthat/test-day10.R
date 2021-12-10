test_that("day 10", {
  example_data_10() |>
    f10a_score_syntax_errors() |>
    expect_equal(26397)

  example_data_10() |>
    f10b_score_incomplete_lines() |>
    expect_equal(288957)
})
