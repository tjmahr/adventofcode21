test_that("day 14", {
  x <- example_data_14()

  x |>
    f14a_grow_polymer(n = 1) |>
    paste0(collapse = "") |>
    expect_equal("NCNBCHB")

  x |>
    f14a_grow_polymer(n = 2) |>
    paste0(collapse = "") |>
    expect_equal("NBCCNBBBCBHCB")

  x |>
    f14a_grow_polymer(n = 3) |>
    paste0(collapse = "") |>
    expect_equal("NBBBCNCCNBBNBNBBCHBHHBCHB")

  x |>
    f14a_grow_polymer(n = 4) |>
    paste0(collapse = "") |>
    expect_equal("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")

  x |>
    f14a_grow_polymer(n = 5) |>
    length() |>
    expect_equal(97)

  x |>
    f14a_grow_polymer(n = 10) |>
    length() |>
    expect_equal(3073)

  x |>
    f14a_grow_polymer(n = 10) |>
    table() |>
    range() |>
    diff() |>
    expect_equal(1588)

  x |>
    f14b_grow_polymer_fast(n = 10) |>
    range() |>
    diff() |>
    expect_equal(1588)

  x |>
    f14b_grow_polymer_fast(n = 0) |>
    range() |>
    diff() |>
    expect_equal(1)

  x |>
    f14b_grow_polymer_fast(n = 40) |>
    range() |>
    diff() |>
    expect_equal(2188189693529)
})
