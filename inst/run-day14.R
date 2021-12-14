library(adventofcode21)
x <- readLines("./inst/input14.txt")

p1a <- f14a_grow_polymer(x, n = 10)
p1 <- p1a |>
  table() |>
  range() |>
  diff()

p2a <- f14b_grow_polymer_fast(x, 40)
p2 <- p2a |>
  range() |>
  diff() |>
  print(digits = 20)


stopifnot(p1 == aoc_solutions$day14a)
stopifnot(p2 == aoc_solutions$day14b)
