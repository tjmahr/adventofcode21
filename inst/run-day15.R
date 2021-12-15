library(adventofcode21)
x <- readLines("./inst/input15.txt")

p1a <- f15a_dijkstra(x)
p1 <- p1a[p1a$id == "10000", "cost"]

if (interactive) {
  library(dplyr)
  library(igraph)
  library(tidygraph)
  p1a <- adventofcode21:::f15_prepare_input(x, repeat_5 = TRUE)

  base <- p1a %>%
    select(-cost, -via_row, -via_col, -visited)

  neighbors <- bind_rows(
    base %>% mutate(row = row - 1),
    base %>% mutate(row = row + 1),
    base %>% mutate(col = col - 1),
    base %>% mutate(col = col + 1)
  ) %>%
    filter(row <= max(p1a$row), col <= max(p1a$col)) %>%
    filter(0 < row, 0 < col) %>%
    mutate(
      n_id = row * max(p1a$row) + col - max(p1a$col)
    ) %>%
    select(id, n_id) %>%
    rename(from = id, to = n_id) %>%
    left_join(base %>% select(to = id, value)) %>%
    arrange(from, to)

  nodes <- distinct(neighbors, from)
  edges <- distinct(neighbors, from, to)

  g <- tbl_graph(nodes, edges, directed = TRUE) %>%
    activate(edges) %>%
    left_join(neighbors) %>%
    rename(weight = value)

  p2 <- distances(g, 1, max(neighbors$from), mode = "out")
}

stopifnot(p1 == aoc_solutions$day15a)
# stopifnot(p2 == aoc_solutions$day15b)
