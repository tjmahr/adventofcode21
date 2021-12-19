test_that("day 18", {
  expect_pair <- function(x1, x2) {
    expect_equal(
      x1 |> f18_snailfish_rows() |> f18_reduce_snailfish(),
      x2 |> f18_snailfish_rows()
    )
  }

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
  a3 <- f18_add_snailfish(a1, a2)

  a3_solution <- f18_snailfish_rows("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

  expect_equal(a3, a3_solution)

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
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

   a3 |>
    f18_reduce_snailfish() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[15,[0,13]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")) |>
    f18_reduce_snailfish_once() |>
    expect_equal(f18_snailfish_rows("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")) |>
    expect_equal(f18_reduce_snailfish(a3))



  a3 |> f18_snailfish_rows()
})

