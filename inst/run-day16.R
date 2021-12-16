library(adventofcode21)
x <- readLines("./inst/input16.txt")

p1 <- f16a_sum_packet_versions(x)
p2 <- f16b_eval_packets(x)

stopifnot(p1 == aoc_solutions$day16a)
stopifnot(p2 == aoc_solutions$day16b)
