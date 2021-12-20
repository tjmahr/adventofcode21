test_that("day 19", {
  x <- example_data_19()

  f19_count_beacons(x) |>
    getElement("part1") |>
    expect_equal(79)

  f19_count_beacons(x) |>
    getElement("part2") |>
    expect_equal(3621)
})
