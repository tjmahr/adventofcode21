test_that("day 04", {
  expect_equal(
    f04a_play_bingo(readLines(example_data_04())),
    4512
  )
  expect_equal(
    f04b_play_until_last_bingo(readLines(example_data_04())),
    1924
  )
})
