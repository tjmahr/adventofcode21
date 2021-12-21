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
#' Maybe a fancy trick shot isn\'t the best idea; after all, you only have
#' one probe, so you had better not miss.
#'
#' To get the best idea of what your options are for launching the probe,
#' you need to find *every initial velocity* that causes the probe to
#' eventually be within the target area after any step.
#'
#' In the above example, there are `112` different initial velocity values
#' that meet these criteria:
#'
#'     23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
#'     25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
#'     8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
#'     26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
#'     20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
#'     25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
#'     25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
#'     8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
#'     24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
#'     7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
#'     23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
#'     27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
#'     8,-2    27,-8   30,-5   24,-7
#'
#' *How many distinct initial velocity values cause the probe to be within
#' the target area after any step?*
#'
#' @param x some data
#' @return For Part One, `f17a_maximize_height_old(x)` returns the maximum height.
#'   For Part Two, `f17b_count_velocities(x)` returns a dataframe of velocities.
#' @export
#' @examples
#' f17a_maximize_height_old(example_data_17())
#' f17b_count_velocities(example_data_17())
f17a_maximize_height_old <- function(x) {
  # strategy: objects for the probes, good guesses using triangular numbers,
  # grid search. very slow. this is so slow that i solved it again in part
  # 2's function
  in_range <- function(x, r) {
    r <- range(r)
    r[1] <= x & x <= r[2]
  }

  guess_x_velocities <- function(target) {
    seq(2, target$x[2] + 1) |>
      # triangular numbers = choose(n + 1, 2)
      choose(2) |>
      in_range(target$x) |>
      which()
  }

  # try all of the heights for this x value
  maximize_height <- function(coordinate, velocity, target) {
    best_y <- 0
    best_v <- 0
    for (i in seq_len(2 * max(abs(target$y)))) {
      p_inc_y <- f17_make_probe(
        coordinate,
        velocity + c(0, i),
        target
      )
      p_inc_y$step_all()
      h <- p_inc_y$get_history()
      if (any(h$in_target) & max(h$y) > best_y) {
        best_y <- max(h$y)
        best_v <- i
      }
    }
    list(
      vx = velocity[1],
      best_vy = best_v,
      best_y = best_y
    )
  }

  target_area <- f17_helper(x)
  xs <- guess_x_velocities(target_area)
  optimizations <- as.list(seq_along(xs))
  for (x_i in seq_along(xs)) {
    o <- maximize_height(c(0, 0), c(xs[x_i], 0), target_area)
    optimizations[[x_i]] <- o
  }

  optimizations |>
    lapply(getElement, "best_y") |>
    unlist(max) |>
    unique()
}

#' @rdname day17
#' @export
f17b_count_velocities <- function(x) {
  # strategy: find times when x velocity is in the target, find times when y
  # velocity is in the target, merge the times together to get x,y pairs.
  in_range <- function(x, r) {
    r <- range(r)
    r[1] <= x & x <= r[2]
  }

  # x <- example_data_17()
  target <- f17_helper(x)

  # find when x velocity would hit the target
  fx2 <- function(n, stalling_ceiling) {
    steps <- cumsum(rev(seq_len(n)))
    times <- which(in_range(steps, target$x))
    stalled <- in_range(steps[n], target$x)

    # add extra rows for longest y path
    if (stalled) {
      times <- seq(min(times), stalling_ceiling)
    }

    if (length(times)) {
      data.frame(
        vx = n,
        times = times,
        stalled_in_x = stalled
      )
    } else {
      data.frame(vx = numeric(0), times = numeric(0))
    }
  }

  # find when y velocity would hit the target
  fy2 <- function(n) {
    steps <- seq(n, min(target$y) - 10) |> cumsum()
    highest <- max(steps)
    times <- which(in_range(steps, target$y))
    # stalled <- in_range(steps[n], target$x)
    if (length(times)) {
      data.frame(
        vy = n,
        times = times,
        highest = highest
      )
    } else {
      data.frame(vy = numeric(0), times = numeric(0), highest = numeric(0))
    }
  }

  y_candidates <- seq(min(target$y) - 1, abs(min(target$y)))
  vy_grid <- y_candidates |>
    lapply(fy2) |>
    f_reduce(rbind)
  longest_time <- max(vy_grid$times)

  x_candidates <- seq(1, max(target$x))
  vx_grid <- x_candidates |>
    lapply(fx2, stalling_ceiling = longest_time) |>
    f_reduce(rbind)

  m <- merge(vx_grid, vy_grid)

  m <- unique(m[c("vx", "vy", "highest")])
  m
}






f17_make_probe <- function(coordinate, velocity, target) {
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
