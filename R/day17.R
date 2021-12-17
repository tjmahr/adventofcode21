#' Day 17: Trick Shot
#'
#' [Trick Shot](https://adventofcode.com/2021/day/17)
#'
#' @name day17
#' @rdname day17
#' @details
#'
#' **Part One**
#'
#' You finally decode the Elves\' message. `HI`, the message says. You
#' continue searching for the sleigh keys.
#'
#' Ahead of you is what appears to be a large [ocean
#' trench](https://en.wikipedia.org/wiki/Oceanic_trench). Could the keys
#' have fallen into it? You\'d better send a probe to investigate.
#'
#' The probe launcher on your submarine can fire the probe with any
#' [integer](https://en.wikipedia.org/wiki/Integer) velocity in the `x`
#' (forward) and `y` (upward, or downward if negative) directions. For
#' example, an initial `x,y` velocity like `0,10` would fire the probe
#' straight up, while an initial velocity like `10,-1` would fire the probe
#' forward at a slight downward angle.
#'
#' The probe\'s `x,y` position starts at `0,0`. Then, it will follow some
#' trajectory by moving in *steps*. On each step, these changes occur in
#' the following order:
#'
#' -   The probe\'s `x` position increases by its `x` velocity.
#' -   The probe\'s `y` position increases by its `y` velocity.
#' -   Due to drag, the probe\'s `x` velocity changes by `1` toward the
#'     value `0`; that is, it decreases by `1` if it is greater than `0`,
#'     increases by `1` if it is less than `0`, or does not change if it is
#'     already `0`.
#' -   Due to gravity, the probe\'s `y` velocity decreases by `1`.
#'
#' For the probe to successfully make it into the trench, the probe must be
#' on some trajectory that causes it to be within a *target area* after any
#' step. The submarine computer has already calculated this target area
#' (your puzzle input). For example:
#'
#'     target area: x=20..30, y=-10..-5
#'
#' This target area means that you need to find initial `x,y` velocity
#' values such that after any step, the probe\'s `x` position is at least
#' `20` and at most `30`, *and* the probe\'s `y` position is at least `-10`
#' and at most `-5`.
#'
#' Given this target area, one initial velocity that causes the probe to be
#' within the target area after any step is `7,2`:
#'
#'     .............#....#............
#'     .......#..............#........
#'     ...............................
#'     S........................#.....
#'     ...............................
#'     ...............................
#'     ...........................#...
#'     ...............................
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTT#TT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'
#' In this diagram, `S` is the probe\'s initial position, `0,0`. The `x`
#' coordinate increases to the right, and the `y` coordinate increases
#' upward. In the bottom right, positions that are within the target area
#' are shown as `T`. After each step (until the target area is reached),
#' the position of the probe is marked with `#`. (The bottom-right `#` is
#' both a position the probe reaches and a position in the target area.)
#'
#' Another initial velocity that causes the probe to be within the target
#' area after any step is `6,3`:
#'
#'     ...............#..#............
#'     ...........#........#..........
#'     ...............................
#'     ......#..............#.........
#'     ...............................
#'     ...............................
#'     S....................#.........
#'     ...............................
#'     ...............................
#'     ...............................
#'     .....................#.........
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................T#TTTTTTTTT
#'     ....................TTTTTTTTTTT
#'
#' Another one is `9,0`:
#'
#'     S........#.....................
#'     .................#.............
#'     ...............................
#'     ........................#......
#'     ...............................
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTT#
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'
#' One initial velocity that *doesn\'t* cause the probe to be within the
#' target area after any step is `17,-4`:
#'
#'     S..............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     .................#.............................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT..#.............................
#'     ....................TTTTTTTTTTT................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ................................................#..............
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ..............................................................#
#'
#' The probe appears to pass through the target area, but is never within
#' it after any step. Instead, it continues down and to the right - only
#' the first few steps are shown.
#'
#' If you\'re going to fire a highly scientific probe out of a super cool
#' probe launcher, you might as well do it with *style*. How high can you
#' make the probe go while still reaching the target area?
#'
#' In the above example, using an initial velocity of `6,9` is the best you
#' can do, causing the probe to reach a maximum `y` position of `45`. (Any
#' higher initial `y` velocity causes the probe to overshoot the target
#' area entirely.)
#'
#' Find the initial velocity that causes the probe to reach the highest `y`
#' position and still eventually be within the target area after any step.
#' *What is the highest `y` position it reaches on this trajectory?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f17a(x)` returns .... For Part Two,
#'   `f17b(x)` returns ....
#' @export
#' @examples
#' f17a(example_data_17())
#' f17b()
f17a <- function(x) {
  x <- example_data_17()
  target_area <- f17_helper(x)


  coordinate <- c(0, 0)
  velocity <- c(7, 2)
  vx0 = velocity[1]
  vy0 = velocity[2]
  make_probe <- function(coordinate, velocity, target) {
    # coordinate <- coordinate
    # velocity <- velocity
    time <- 0
    vx0 <- velocity[1]
    vy0 <- velocity[2]
    in_range <- function(x, r) {
      r <- range(r)
      r[1] <= x & x <= r[2]
    }
    in_target <- function() {
      in_range(coordinate[1], target$x) &
        in_range(coordinate[2], target$y)
    }
    is_gone <- function() {
      past_x <- max(target$x) < coordinate[1]
      not_going_back <- coordinate[1] <= (coordinate[1] + velocity[1])
      x_gone_right <- past_x & not_going_back

      past_x2 <- min(target$x) > coordinate[1]
      not_going_back2 <- coordinate[1] >= (coordinate[1] + velocity[1])
      x_gone_left <- past_x2 & not_going_back2
      x_gone <- x_gone_left | x_gone_right

      past_y <- coordinate[2] < min(target$y)
      and_falling <- coordinate[2] >= (coordinate[2] + velocity[2])
      y_gone <- past_y & and_falling

      x_gone | y_gone
    }

    history <- data.frame(
      t = time,
      x = coordinate[1],
      y = coordinate[2],
      vx = velocity[1],
      vy = velocity[2],
      vx0 = vx0,
      vy0 = vy0,
      in_target = in_target(),
      is_gone = is_gone()
    )

    update_history <- function() {
      history <<- rbind(
        history,
        data.frame(
          t = time,
          x = coordinate[1],
          y = coordinate[2],
          vx = velocity[1],
          vy = velocity[2],
          vx0 = vx0,
          vy0 = vy0,
          in_target = in_target(),
          is_gone = is_gone()
        )
      )
    }

    step <- function() {
      time <<- time + 1
      coordinate[1] <<- coordinate[1] + velocity[1]
      coordinate[2] <<- coordinate[2] + velocity[2]
      velocity[1] <<- velocity[1] - sign(velocity[1])
      velocity[2] <<- velocity[2] - 1
      update_history()
      invisible(NULL)
    }

    step_n <- function(n) {
      for (i in seq_len(n)) {
        step()
      }
    }

    step_all <- function() {
      while(!is_gone()) step()
    }

    get_history <- function() history

    list(
      step = step,
      get_history = get_history,
      step_n = step_n,
      step_all = step_all
    )
  }

  p <- make_probe(c(0, 0), c(1, 1), target = target_area)

  p <- make_probe(c(0, 0), c(target_area$x[2], target_area$y[2]), target = target_area)

  p$step_all()
  d <- p$get_history()
  d
  # then increment y and x until they maximize y








  start_v <- c(1, 1)
  max_y <- max(d$y)
  better <- TRUE
  while (better) {
    p_inc_y <- make_probe(c(0, 0), start_v + c(0, 1), target = target_area)
    p_inc_y$step_all()
    h1 <- p_inc_y$get_history()
    better <- any(h1$in_target) & max_y < max(h1$y)
    if (better) {
      start_v <- start_v + c(0, 1)
      max_y <- max(d$y)

    }
  }
  p <- make_probe(c(0, 0), start_v, target = target_area)


  p$step_all()
  p$get_history()
  better <- TRUE
  max_y <- max(p$get_history()["y"])
  while (better) {
    p_inc_x <- make_probe(c(0, 0), start_v + c(1, 0), target = target_area)
    p_dec_x <- make_probe(c(0, 0), start_v + c(-1, 0), target = target_area)
    p_inc_x$step_all()
    p_dec_x$step_all()

    h1 <- p_inc_x$get_history()
    h2 <- p_dec_x$get_history()

    inc_better <- any(h1$in_target) & max_y < max(h1$y)
    dec_better <- any(h2$in_target) & max_y < max(h2$y)

    if (better) {
      start_v <- start_v + c(0, 1)
    }
  }




  d <- p$get_history()
  start_v <- c(7, 2)
  max_y <- max(d$y)
  better <- TRUE
  while (better) {
    p_inc_y <- make_probe(c(0, 0), start_v + c(0, 1), target = target_area)
    p_inc_y$step_all()
    h1 <- p_inc_y$get_history()
    better <- any(h1$in_target) & max_y < max(h1$y)
    if (better) {
      start_v <- start_v + c(0, 1)
    }
  }


  p$step_all()
  p$step_n(20)
  lm(x ~ t + vx + vx*t, d) |> summary()
  lm(y ~ 0 + vy0:t + I(t^2), d) |> summary()




  lm(y ~ t + vy + vy*t, d) |> summary()


  d$t * d$vx0 - .5 * d$t ^ 2
  d$x
  d$t * d$vx0 - .5 * d$t ^ 2

  summary(m)
  round(coef(m, 3))

  m <- lm(x ~ t + t:vx , d)

  round(coef(m, 2))
  summary(m)
  x = c + -4-vx + .5*t*vx

  p$get_velocity_history()

  p$step(20)

  step <- function(coordinate, velocity) {
    coordinate[1] <- coordinate[1] + velocity[1]
    coordinate[2] <- coordinate[2] + velocity[2]
    # wish i had a math version of this
    velocity[1] <- velocity[1] + ifelse(velocity[1] > 0, -1, 1)
    velocity[2] <- velocity[2] - 1

  }

  steps <- as.list(1:10)
  for (i in steps) {
    coordin
  }
}


#' @rdname day17
#' @export
f17b <- function(x) {

}


f17_helper <- function(x) {
  # x <- example_data_17()
  # x
  # base R regex functions are so confusing
  x_part <- regmatches(x, regexpr("x=.+,", x))
  y_part <- regmatches(x, regexpr("y=.+", x))

  xs <- x_part |>
    substr(3, nchar(x_part) - 1) |>
    strsplit("[.][.]") |>
    unlist() |>
    as.numeric()

  ys <- y_part |>
    substr(3, nchar(y_part)) |>
    strsplit("[.][.]") |>
    unlist() |>
    as.numeric()

  list(x = xs, y = ys)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day17
#' @export
example_data_17 <- function(example = 1) {
  l <- list(
    a = c(
      "target area: x=20..30, y=-10..-5"

    )
  )
  l[[example]]
}
