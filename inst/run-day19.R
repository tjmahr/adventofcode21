library(adventofcode21)
x <- readLines("./inst/input19.txt")

p0 <- f19_count_beacons(x)
p1 <- p0$part1
p2 <- p0$part2

stopifnot(p1 == aoc_solutions$day19a)
stopifnot(p2 == aoc_solutions$day19b)
