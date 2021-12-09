#' Day 09: Smoke Basin
#'
#' [Smoke Basin](https://adventofcode.com/2021/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' These caves seem to be [lava
#' tubes](https://en.wikipedia.org/wiki/Lava_tube). Parts are even still
#' volcanically active; small hydrothermal vents release smoke into the
#' caves that slowly [settles like
#' rain]{title="This was originally going to be a puzzle about watersheds, but we're already under water."}.
#'
#' If you can model how the smoke flows through the caves, you might be
#' able to avoid it and be that much safer. The submarine generates a
#' heightmap of the floor of the nearby caves for you (your puzzle input).
#'
#' Smoke flows to the lowest point of the area it\'s in. For example,
#' consider the following heightmap:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Each number corresponds to the height of a particular location, where
#' `9` is the highest and `0` is the lowest a location can be.
#'
#' Your first goal is to find the *low points* - the locations that are
#' lower than any of its adjacent locations. Most locations have four
#' adjacent locations (up, down, left, and right); locations on the edge or
#' corner of the map have three or two adjacent locations, respectively.
#' (Diagonal locations do not count as adjacent.)
#'
#' In the above example, there are *four* low points, all highlighted: two
#' are in the first row (a `1` and a `0`), one is in the third row (a `5`),
#' and one is in the bottom row (also a `5`). All other locations on the
#' heightmap have some lower adjacent location, and so are not low points.
#'
#' The *risk level* of a low point is *1 plus its height*. In the above
#' example, the risk levels of the low points are `2`, `1`, `6`, and `6`.
#' The sum of the risk levels of all low points in the heightmap is
#' therefore `15`.
#'
#' Find all of the low points on your heightmap. *What is the sum of the
#' risk levels of all low points on your heightmap?*
#'
#' **Part Two**
#'
#' Next, you need to find the largest basins so you know what areas are
#' most important to avoid.
#'
#' A *basin* is all locations that eventually flow downward to a single low
#' point. Therefore, every low point has a basin, although some basins are
#' very small. Locations of height `9` do not count as being in any basin,
#' and all other locations will always be part of exactly one basin.
#'
#' The *size* of a basin is the number of locations within the basin,
#' including the low point. The example above has four basins.
#'
#' The top-left basin, size `3`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The top-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The middle basin, size `14`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The bottom-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Find the three largest basins and multiply their sizes together. In the
#' above example, this is `9 * 14 * 9 = 1134`.
#'
#' *What do you get if you multiply together the sizes of the three largest
#' basins?*
#'
#' @param x some data
#' @return For Part One, `f09a_find_lava_risk_level(x)` returns the sum of the
#'   lava risk levels. For Part Two, `f09b_find_basins(x)` returns the product
#'   of the sizes of the three largest basins.
#' @export
#' @examples
#' f09a_find_lava_risk_level(example_data_09())
#' f09b()
f09a_find_lava_risk_level <- function(x) {
  # strategy: matrix subsetting
  l <- f09_find_low_points(x)
  m <- l$matrix
  mask <- l$mask
  sum(m[mask] + 1)
}


#' @rdname day09
#' @export
f09b_find_basins <- function(x) {
  # strategy: matrix subsetting
  # two mistakes i made:
  # - i included diagonal neighbors at first
  # - i thought only differences of 1 counted for basins instead of
  #   any positive difference
  find_neighbors <- function(row, col, dims) {
    n <- cbind(
      row + c(-1, 1,  0, 0),
      col + c( 0, 0, -1, 1)
    )

    in_range <- function(xs, r) min(r) <= xs & xs <= max(r)
    n <- n[in_range(n[, 1], c(1, dims[1])), , drop = FALSE]
    n <- n[in_range(n[, 2], c(1, dims[2])), , drop = FALSE]
    n
  }

  walk_next_basin <- function(data) {
    if (nrow(data$stack)) {
      current_point <- data$stack[1, , drop = FALSE]
      row <- current_point[1, 1]
      col <- current_point[1, 2]
      data$stack <- data$stack[-1, , drop = FALSE]
      current_depth <- data$depths[row, col]
      current_basin <- data$basins[row, col]

      ns <- find_neighbors(row, col, dim(data$depths))
      ns <- ns[is.na(data$basins[ns]), , drop = FALSE]
      # any positive difference is a downward flow
      ns_valid <- ns[(data$depths[ns] - current_depth) <= 9, , drop = FALSE]

      data$basins[ns_valid] <- current_basin
      data$stack <- rbind(ns_valid, data$stack)
    }

    data
  }

  l <- f09_find_low_points(x)
  depths <- l$matrix
  mask <- l$mask

  basins <- matrix(NA, nrow = nrow(depths), ncol = ncol(depths))
  basins[which(mask)] <- seq_len(sum(mask))
  basins[depths == 9] <- 0

  basin_data <- list(
    depths = depths,
    basins = basins,
    stack = which(!is.na(basins) & basins != 0, arr.ind = TRUE)
  )

  while(nrow(basin_data$stack)) {
    basin_data <- walk_next_basin(basin_data)
  }

  basin_data$basins[basin_data$basins == 0] <- NA

  table(basin_data$basins) |>
    sort(decreasing = TRUE) |>
    head(3) |>
    prod()
}


f09_find_low_points <- function(x) {
  # x <- example_data_09()
  m <- x |> strsplit("") |> lapply(as.numeric) |> simplify2array() |> t()

  diff_prev <- function(xs, fill = 10) diff(c(fill, xs))
  diff_next <- function(xs, fill = 10) rev(diff(rev(c(xs, fill))))

  row_diff_prev <- m |> apply(1, diff_prev) |> t()
  row_diff_next <- m |> apply(1, diff_next) |> t()

  col_diff_prev <- m |> apply(2, diff_prev)
  col_diff_next <- m |> apply(2, diff_next)

  mask <- (row_diff_prev < 0) &
    (row_diff_next < 0) &
    (col_diff_prev < 0) &
    (col_diff_next < 0)

  list(matrix = m, mask = mask)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(
      "2199943210",
      "3987894921",
      "9856789892",
      "8767896789",
      "9899965678"
    )
  )
  l[[example]]
}
