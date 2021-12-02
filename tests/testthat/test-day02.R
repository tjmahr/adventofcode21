test_that("day 2", {
  expect_equal(
    f02a_find_submarine_product(example_data_02()),
    150
  )
  expect_equal(
    f02b_find_aimed_submarine_product(example_data_02()),
    900
  )
})
