test_that("day 18", {
  expect_pair <- function(x1, x2) {
    expect_equal(
      x1 |> f18_snailfish_rows() |> f18_reduce_snailfish_once(),
      x2 |> f18_snailfish_rows()
    )
  }

  # test explosions
  expect_pair("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
  expect_pair("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
  expect_pair("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
  expect_pair(
    "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
    "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
  )
  expect_pair(
    "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
    "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
  )

  a1 <- f18_snailfish_rows("[[[[4,3],4],4],[7,[[8,4],9]]]")
  a2 <- f18_snailfish_rows("[1,1]")

  # test an add
  a3 <- f18_add_snailfish_pair(a1, a2)
  a3_solution <- f18_snailfish_rows("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
  expect_equal(a3, a3_solution)

  # test the long reduction
   a3 |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[15,[0,13]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")) |>
    # is full reduction equal result of each reduction step
    expect_equal(f18_reduce_snailfish(a3))
})


test_that("day 18 sums", {
  expect_sum <- function(x1, x2) {
    x1 <- f18_sum_snailfish(x1)
    x2 <- f18_snailfish_rows(x2)
    expect_equal(x1, x2)
  }

  example_data_18(1) |>
    expect_sum("[[[[1,1],[2,2]],[3,3]],[4,4]]")

  example_data_18(2) |>
    expect_sum("[[[[3,0],[5,3]],[4,4]],[5,5]]")

  example_data_18(3) |>
    expect_sum("[[[[5,0],[7,4]],[5,5]],[6,6]]")

  example_data_18(4) |>
    expect_sum("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
})


test_that("day 18 magnitudes", {
  expect_magnitude <- function(x, n) {
    x |>
      f18_snailfish_rows() |>
      f18_snailfish_magnitude() |>
      expect_equal(n)
  }
  expect_magnitude("[[1,2],[[3,4],5]]", 143)
  expect_magnitude("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)
  expect_magnitude("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)
  expect_magnitude("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791)
  expect_magnitude("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137)
  expect_magnitude(
    "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
    3488
  )
})


test_that("day 18 part 2", {
  example_data_18(5) |>
    f18b_snailfish_homework2() |>
    max() |>
    expect_equal(3993)
})

