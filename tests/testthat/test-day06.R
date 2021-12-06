test_that("day 06: reading and normalizing counts", {
  i <- c(
    "3,4,3,1,2",
    "2,3,2,0,1",
    "1,2,1,6,0,8",
    "0,1,0,5,6,7,8",
    "6,0,6,4,5,6,7,8,8",
    "5,6,5,3,4,5,6,7,7,8",
    "4,5,4,2,3,4,5,6,6,7",
    "3,4,3,1,2,3,4,5,5,6",
    "2,3,2,0,1,2,3,4,4,5",
    "1,2,1,6,0,1,2,3,3,4,8",
    "0,1,0,5,6,0,1,2,2,3,7,8",
    "6,0,6,4,5,6,0,1,1,2,6,7,8,8,8",
    "5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8",
    "4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8",
    "3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8",
    "2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7",
    "1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8",
    "0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8",
    "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8"
  )

  day0ex <- example_data_06() |> f06_read_and_count()
  day0 <- i[1] |> f06_read_and_count()
  day1 <- i[2] |> f06_read_and_count()
  day2 <- i[3] |> f06_read_and_count()

  # manual uncount checks
  expect_equal(day0ex, day0)
  expect_equal(f06_uncount(day0), c(1, 2, 3, 3, 4))
  expect_equal(f06_uncount(day1), c(0, 1, 2, 2, 3))
  expect_equal(f06_uncount(day2), c(0, 1, 1, 2, 6, 8))

  # uncount by sorting raw input
  days_input_sorted <- i |>
    lapply(strsplit, ",") |>
    lapply(unlist) |>
    lapply(sort) |>
    lapply(as.numeric)

  days <- i |>
    lapply(f06_read_and_count) |>
    lapply(f06_uncount)
  expect_equal(days, days_input_sorted)
})

test_that("day 06: walking through time", {
  i <- c(
    "3,4,3,1,2",
    "2,3,2,0,1",
    "1,2,1,6,0,8",
    "0,1,0,5,6,7,8",
    "6,0,6,4,5,6,7,8,8",
    "5,6,5,3,4,5,6,7,7,8",
    "4,5,4,2,3,4,5,6,6,7",
    "3,4,3,1,2,3,4,5,5,6",
    "2,3,2,0,1,2,3,4,4,5",
    "1,2,1,6,0,1,2,3,3,4,8",
    "0,1,0,5,6,0,1,2,2,3,7,8",
    "6,0,6,4,5,6,0,1,1,2,6,7,8,8,8",
    "5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8",
    "4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8",
    "3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8",
    "2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7",
    "1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8",
    "0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8",
    "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8"
  )

  counts <- i |>
    lapply(f06_read_and_count)

  uncounts <- counts |>
    lapply(f06_uncount)

  counts[[1]] |>
    f06_step_day() |>
    f06_uncount() |>
    expect_equal(uncounts[[2]])

  counts[1:18] |>
    lapply(f06_step_day) |>
    lapply(f06_uncount) |>
    expect_equal(uncounts[2:19])

  i[1] |>
    f06a_step_n_days(n = 18) |>
    f06_uncount() |>
    expect_equal(uncounts[[19]])

  i[1] |>
    f06a_step_n_days(n = 18) |>
    sum() |>
    expect_equal(26)

  i[1] |>
    f06a_step_n_days(n = 80) |>
    sum() |>
    expect_equal(5934)

  i[1] |>
    f06a_step_n_days(n = 256) |>
    sum() |>
    expect_equal(26984457539)
})
