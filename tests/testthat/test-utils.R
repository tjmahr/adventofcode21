test_that("mod1() works", {
  expect_equal(mod1(7, 1), 1)
  expect_equal(mod1(7, 7), 7)
  expect_equal(mod1(4, 3), 1)
  expect_equal(mod1(4, 2), 2)
})
