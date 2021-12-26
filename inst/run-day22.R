library(adventofcode21)
x <- readLines("./inst/input22.txt")

p1 <- f22a_flip_cubes_small(x)
stopifnot(p1 == aoc_solutions$day22a)

if (FALSE) {
  p2 <- f22b_flip_cubes_big(x)
  stopifnot(p2 == aoc_solutions$day22b)
}
