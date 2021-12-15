#' Day 15: Chiton
#'
#' [Chiton](https://adventofcode.com/2021/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' You\'ve almost reached the exit of the cave, but the walls are getting
#' closer together. Your submarine can barely still fit, though; the main
#' problem is that the walls of the cave are covered in
#' [chitons](https://en.wikipedia.org/wiki/Chiton), and it would be best
#' not to bump any of them.
#'
#' The cavern is large, but has a very low ceiling, restricting your motion
#' to two dimensions. The shape of the cavern resembles a square; a quick
#' scan of chiton density produces a map of *risk level* throughout the
#' cave (your puzzle input). For example:
#'
#'     1163751742
#'     1381373672
#'     2136511328
#'     3694931569
#'     7463417111
#'     1319128137
#'     1359912421
#'     3125421639
#'     1293138521
#'     2311944581
#'
#' You start in the top left position, your destination is the bottom right
#' position, and you [cannot move
#' diagonally]{title="Can't go diagonal until we can repair the caterpillar unit. Could be the liquid helium or the superconductors."}.
#' The number at each position is its *risk level*; to determine the total
#' risk of an entire path, add up the risk levels of each position you
#' *enter* (that is, don\'t count the risk level of your starting position
#' unless you enter it; leaving it adds no risk to your total).
#'
#' Your goal is to find a path with the *lowest total risk*. In this
#' example, a path with the lowest total risk is highlighted here:
#'
#'     1163751742
#'     1381373672
#'     2136511328
#'     3694931569
#'     7463417111
#'     1319128137
#'     1359912421
#'     3125421639
#'     1293138521
#'     2311944581
#'
#' The total risk of this path is `40` (the starting position is never
#' entered, so its risk is not counted).
#'
#' *What is the lowest total risk of any path from the top left to the
#' bottom right?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f15a(x)` returns .... For Part Two,
#'   `f15b(x)` returns ....
#' @export
#' @examples
#' f15a(example_data_15())
#' f15b()
f15a_dijkstra <- function(x) {
  # strategy: dijkstra's algorithm + fake dplyr functions
  set_start <- function(df, i, j) {
    df[df[["row"]] == i & df[["col"]] == j, "cost"] <- 0
    df
  }

  create_neighbor_df <- function(df) {
    df_minimal <- df[c("row", "col")]
    df_candidates <- df_minimal |>
      split(seq_len(nrow(df))) |>
      lapply(function(df) {
        data.frame(
          source_row = df$row,
          source_col = df$col,
          row = df$row + c(-1, 1,  0, 0),
          col = df$col + c( 0, 0, -1, 1)
        )
      }) |>
      f_reduce(rbind) |>
      # filter with an inner join
      merge(df_minimal, all = FALSE)

    names(df_candidates) <- c("neigh_row", "neigh_col", "row", "col")
    df_candidates
  }

  visit_next_node <- function(df, df_neighbors) {
    current_node <- df[1, ]
    current_node$visited <- TRUE

    neighbors <- current_node |>
      # find unvisited neighboring nodes
      merge(df_neighbors) |>
      select_cols(row = "neigh_row", col = "neigh_col") |>
      merge(df) |>
      filter_rows(~ visited == FALSE) |>
      # update their cost if better than current cost
      mutate_rows(
        via_row = ~ ifelse(
          current_node$cost + value < cost,
          current_node$row,
          via_row
        ),
        via_col = ~ ifelse(
          current_node$cost + value < cost,
          current_node$col,
          via_col
        ),
        cost = ~ ifelse(
          current_node$cost + value < cost,
          current_node$cost + value,
          cost
        )
      )

    df <- df |>
      patch_rows(neighbors, by = c("row", "col")) |>
      patch_rows(current_node, by = c("row", "col"))

    # sort so it is a priority queue
    df[order(df$visited, df$cost), , drop = FALSE]
  }

  has_visited_node <- function(df, i, j) {
    df[df$row == i & df$col == j, "visited", drop = TRUE]
  }

  # x <- example_data_15()
  df <- x |> f15_prepare_input() |> set_start(1, 1)
  df_neighbors <- create_neighbor_df(df)
  row_target <- max(df$row)
  col_target <- max(df$col)

  while(!has_visited_node(df, row_target, col_target)) {
    df <- df |> visit_next_node(df_neighbors)
  }

  df
}


#' @rdname day15
#' @export
f15b <- function(x) {

}


f15_prepare_input <- function(x) {
  create_dijkstra_df <- function(i, j, value) {
    data.frame(
      row = i,
      col = j,
      value,
      cost = Inf,
      via_row = NA,
      via_col = NA,
      visited = FALSE
    )
  }

  # use a dataframe of indices
  df <- x |>
    strsplit("") |>
    lapply(as.numeric) |>
    f_map2(
      seq_along(x),
      function(vs, is) create_dijkstra_df(is, seq_along(vs), vs)
    ) |>
    f_reduce(rbind)

  df
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day15
#' @export
example_data_15 <- function(example = 1) {
  l <- list(
    a = c(
      "1163751742",
      "1381373672",
      "2136511328",
      "3694931569",
      "7463417111",
      "1319128137",
      "1359912421",
      "3125421639",
      "1293138521",
      "2311944581"
    )
  )
  l[[example]]
}
