test_that("day 15", {
  map <- example_data_15() |> f15a_dijkstra()
  expect_equal(map[nrow(map), "cost"], 40)

  map <- example_data_15() |> f15a_dijkstra(repeat_5 = TRUE)
  expect_equal(map[nrow(map), "cost"], 315)
})
