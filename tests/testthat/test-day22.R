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

test_that("day 22 intervals", {
  a <- c(-20, 10)
  b <- c(-20, 10)
  c <- c( 10, 20)
  d <- c( 21, 30)

  are_identical <- function(a, b) {
    all(a == b)
  }

  are_disjoint <- function(a, b) {
    s <- unique(sort(c(a, b)))
    length(s) == 4 &&
      (all(s[1:2] == a) || all(s[1:2] == b))
  }
  are_identical(a, b) |>
    expect_true()
  are_disjoint(a, b) |>
    expect_false()

  # c,d disjoint
})
