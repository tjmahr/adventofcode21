library(adventofcode21)
x <- readLines("./inst/input12.txt")

p1 <- f12a_traverse_caves_once(x) |> length()
p2 <- f12b_traverse_caves_maybe_twice(x) |> length()

stopifnot(p1 == aoc_solutions$day12a)
stopifnot(p2 == aoc_solutions$day12b)
