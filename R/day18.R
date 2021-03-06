#' Day 18: Snailfish
#'
#' [Snailfish](https://adventofcode.com/2021/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' You descend into the ocean trench and encounter some
#' [snailfish](https://en.wikipedia.org/wiki/Snailfish). They say they saw
#' the sleigh keys! They\'ll even tell you which direction the keys went if
#' you help one of the smaller snailfish with his
#' *[math]{title="Or 'maths', if you have more than one."} homework*.
#'
#' Snailfish numbers aren\'t like regular numbers. Instead, every snailfish
#' number is a *pair* - an ordered list of two elements. Each element of
#' the pair can be either a regular number or another pair.
#'
#' Pairs are written as `[x,y]`, where `x` and `y` are the elements within
#' the pair. Here are some example snailfish numbers, one snailfish number
#' per line:
#'
#'     [1,2]
#'     [[1,2],3]
#'     [9,[8,7]]
#'     [[1,9],[8,5]]
#'     [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
#'     [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
#'     [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
#'
#' This snailfish homework is about *addition*. To add two snailfish
#' numbers, form a pair from the left and right parameters of the addition
#' operator. For example, `[1,2]` + `[[3,4],5]` becomes
#' `[[1,2],[[3,4],5]]`.
#'
#' There\'s only one problem: *snailfish numbers must always be reduced*,
#' and the process of adding two snailfish numbers can result in snailfish
#' numbers that need to be reduced.
#'
#' To *reduce a snailfish number*, you must repeatedly do the first action
#' in this list that applies to the snailfish number:
#'
#' -   If any pair is *nested inside four pairs*, the leftmost such pair
#'     *explodes*.
#' -   If any regular number is *10 or greater*, the leftmost such regular
#'     number *splits*.
#'
#' Once no action in the above list applies, the snailfish number is
#' reduced.
#'
#' During reduction, at most one action applies, after which the process
#' returns to the top of the list of actions. For example, if *split*
#' produces a pair that meets the *explode* criteria, that pair *explodes*
#' before other *splits* occur.
#'
#' To *explode* a pair, the pair\'s left value is added to the first
#' regular number to the left of the exploding pair (if any), and the
#' pair\'s right value is added to the first regular number to the right of
#' the exploding pair (if any). Exploding pairs will always consist of two
#' regular numbers. Then, the entire exploding pair is replaced with the
#' regular number `0`.
#'
#' Here are some examples of a single explode action:
#'
#' -   `[[[[[9,8],1],2],3],4]` becomes `[[[[0,9],2],3],4]` (the `9` has no
#'     regular number to its left, so it is not added to any regular
#'     number).
#' -   `[7,[6,[5,[4,[3,2]]]]]` becomes `[7,[6,[5,[7,0]]]]` (the `2` has no
#'     regular number to its right, and so it is not added to any regular
#'     number).
#' -   `[[6,[5,[4,[3,2]]]],1]` becomes `[[6,[5,[7,0]]],3]`.
#' -   `[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]` becomes
#'     `[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]` (the pair `[3,2]` is unaffected
#'     because the pair `[7,3]` is further to the left; `[3,2]` would
#'     explode on the next action).
#' -   `[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]` becomes
#'     `[[3,[2,[8,0]]],[9,[5,[7,0]]]]`.
#'
#' To *split* a regular number, replace it with a pair; the left element of
#' the pair should be the regular number divided by two and rounded *down*,
#' while the right element of the pair should be the regular number divided
#' by two and rounded *up*. For example, `10` becomes `[5,5]`, `11` becomes
#' `[5,6]`, `12` becomes `[6,6]`, and so on.
#'
#' Here is the process of finding the reduced result of
#' `[[[[4,3],4],4],[7,[[8,4],9]]]` + `[1,1]`:
#'
#'     after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
#'     after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
#'     after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
#'     after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
#'     after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
#'     after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
#'
#' Once no reduce actions apply, the snailfish number that remains is the
#' actual result of the addition operation:
#' `[[[[0,7],4],[[7,8],[6,0]]],[8,1]]`.
#'
#' The homework assignment involves adding up a *list of snailfish numbers*
#' (your puzzle input). The snailfish numbers are each listed on a separate
#' line. Add the first snailfish number and the second, then add that
#' result and the third, then add that result and the fourth, and so on
#' until all numbers in the list have been used once.
#'
#' For example, the final sum of this list is
#' `[[[[1,1],[2,2]],[3,3]],[4,4]]`:
#'
#'     [1,1]
#'     [2,2]
#'     [3,3]
#'     [4,4]
#'
#' The final sum of this list is `[[[[3,0],[5,3]],[4,4]],[5,5]]`:
#'
#'     [1,1]
#'     [2,2]
#'     [3,3]
#'     [4,4]
#'     [5,5]
#'
#' The final sum of this list is `[[[[5,0],[7,4]],[5,5]],[6,6]]`:
#'
#'     [1,1]
#'     [2,2]
#'     [3,3]
#'     [4,4]
#'     [5,5]
#'     [6,6]
#'
#' Here\'s a slightly larger example:
#'
#'     [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
#'     [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
#'     [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
#'     [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
#'     [7,[5,[[3,8],[1,4]]]]
#'     [[2,[2,2]],[8,[8,1]]]
#'     [2,9]
#'     [1,[[[9,3],9],[[9,0],[0,7]]]]
#'     [[[5,[7,4]],7],1]
#'     [[[[4,2],2],6],[8,7]]
#'
#' The final sum `[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]` is
#' found after adding up the above snailfish numbers:
#'
#'       [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
#'     + [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
#'     = [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
#'
#'       [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
#'     + [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
#'     = [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
#'
#'       [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
#'     + [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
#'     = [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
#'
#'       [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
#'     + [7,[5,[[3,8],[1,4]]]]
#'     = [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
#'
#'       [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
#'     + [[2,[2,2]],[8,[8,1]]]
#'     = [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
#'
#'       [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
#'     + [2,9]
#'     = [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
#'
#'       [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
#'     + [1,[[[9,3],9],[[9,0],[0,7]]]]
#'     = [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
#'
#'       [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
#'     + [[[5,[7,4]],7],1]
#'     = [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
#'
#'       [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
#'     + [[[[4,2],2],6],[8,7]]
#'     = [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
#'
#' To check whether it\'s the right answer, the snailfish teacher only
#' checks the *magnitude* of the final sum. The magnitude of a pair is 3
#' times the magnitude of its left element plus 2 times the magnitude of
#' its right element. The magnitude of a regular number is just that
#' number.
#'
#' For example, the magnitude of `[9,1]` is `3*9 + 2*1 = 29`; the magnitude
#' of `[1,9]` is `3*1 + 2*9 = 21`. Magnitude calculations are recursive:
#' the magnitude of `[[9,1],[1,9]]` is `3*29 + 2*21 = 129`.
#'
#' Here are a few more magnitude examples:
#'
#' -   `[[1,2],[[3,4],5]]` becomes `143`.
#' -   `[[[[0,7],4],[[7,8],[6,0]]],[8,1]]` becomes `1384`.
#' -   `[[[[1,1],[2,2]],[3,3]],[4,4]]` becomes `445`.
#' -   `[[[[3,0],[5,3]],[4,4]],[5,5]]` becomes `791`.
#' -   `[[[[5,0],[7,4]],[5,5]],[6,6]]` becomes `1137`.
#' -   `[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]` becomes
#'     `3488`.
#'
#' So, given this example homework assignment:
#'
#'     [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
#'     [[[5,[2,8]],4],[5,[[9,9],0]]]
#'     [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
#'     [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
#'     [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
#'     [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
#'     [[[[5,4],[7,7]],8],[[8,3],8]]
#'     [[9,3],[[9,9],[6,[4,9]]]]
#'     [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
#'     [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
#'
#' The final sum is:
#'
#'     [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
#'
#' The magnitude of this final sum is `4140`.
#'
#' Add up all of the snailfish numbers from the homework assignment in the
#' order they appear. *What is the magnitude of the final sum?*
#'
#' **Part Two**
#'
#' You notice a second question on the back of the homework assignment:
#'
#' What is the largest magnitude you can get from adding only two of the
#' snailfish numbers?
#'
#' Note that snailfish addition is not
#' [commutative](https://en.wikipedia.org/wiki/Commutative_property) - that
#' is, `x + y` and `y + x` can produce different results.
#'
#' Again considering the last example homework assignment above:
#'
#'     [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
#'     [[[5,[2,8]],4],[5,[[9,9],0]]]
#'     [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
#'     [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
#'     [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
#'     [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
#'     [[[[5,4],[7,7]],8],[[8,3],8]]
#'     [[9,3],[[9,9],[6,[4,9]]]]
#'     [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
#'     [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
#'
#' The largest magnitude of the sum of any two snailfish numbers in this
#' list is `3993`. This is the magnitude of
#' `[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]` +
#' `[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]`, which reduces to
#' `[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]]`.
#'
#' *What is the largest magnitude of any sum of two different snailfish
#' numbers from the homework assignment?*
#'
#' @param x some data
#' @return For Part One, `f18a(x)` returns the magnitude of the sum of snailfish
#'   numbers. For Part Two, `f18b(x)` returns ....
#' @export
#' @examples
#' f18a_snailfish_homework(example_data_18(5))
#' f18b_snailfish_homework2(example_data_18(5))
f18a_snailfish_homework <- function(x) {
  # strategies: book-keeping, recursion
  sum <- f18_sum_snailfish(x)
  f18_snailfish_magnitude(sum)
}


#' @rdname day18
#' @export
f18b_snailfish_homework2 <- function(x) {
  # not commutative so need to work both directions
  f_rev <- function(y) f18a_snailfish_homework(rev(y))
  g1 <- combn(x, 2, FUN = f18a_snailfish_homework)
  g2 <- combn(x, 2, FUN = f_rev)
  g <- c(g1, g2)
  g
}


f18_snailfish_magnitude <- function(x) {
  # Two adjacent rows with same depth is a merge-able pair
  leaf_pairs <- which(utils::head(x$depth, -1) == utils::tail(x$depth, -1))

  # Merge first pair into left
  pair <- leaf_pairs[1]
  left <- x[pair, "number"] * 3
  right <- x[pair + 1, "number"] * 2
  x[pair, "number"] <- left + right
  x[pair, "depth"] <- x[pair, "depth"] - 1

  # Drop right
  x <- x[-(pair + 1), , drop = FALSE]

  if (nrow(x) == 1) {
    x[["number"]]
  } else {
    f18_snailfish_magnitude(x)
  }
}


f18_sum_snailfish <- function(x) {
  f_pair <- function(x, y) {
    x |>
      f18_add_snailfish_pair(y) |>
      f18_reduce_snailfish()
  }
  x |>
    lapply(f18_snailfish_rows) |>
    f_reduce(f_pair)
}


f18_add_snailfish_pair <- function(x, y) {
  x[["depth"]] <- x[["depth"]] + 1
  y[["depth"]] <- y[["depth"]] + 1
  new <- rbind(x, y)
  new[["position"]] <- seq_len(nrow(new))
  new
}


f18_reduce_snailfish <- function(x) {
  old <- x
  repeat {
    new <- f18_reduce_snailfish_once(old)
    if (identical(old, new)) break
    old <- new
  }
  new
}


f18_reduce_snailfish_once <- function(data) {
  can_explode <- function(...) {
    any(data[["depth"]] > 4)
  }
  find_first_exploder <- function(data) {
    i <- Position(function(xs) xs > 4, data$depth)
    c(i, i + 1)
  }
  explode <- function(data) {
    i <- find_first_exploder(data)
    if (i[2] + 1 <= nrow(data)) {
      data[i[2] + 1, "number"] <- data[i[2], "number"] +
        data[i[2] + 1, "number"]
    }
    if (i[1] - 1 >= 1) {
      data[i[1] - 1, "number"] <- data[i[1], "number"] +
        data[i[1] - 1, "number"]
    }
    data[i[1], "depth"] <- data[i[1], "depth"] - 1
    data[i[1], "number"] <- 0
    data <- data[-i[2], , drop = FALSE]
    data[["position"]] <- seq_len(nrow(data))
    data
  }

  can_split <- function(data) {
    any(data[["number"]] > 9)
  }

  split_number <- function(data) {
    i <- which(data[["number"]] > 9)[1]
    rows <- seq_len(nrow(data))
    pre <- data[rows < i, , drop = FALSE]
    post <- data[rows > i, , drop = FALSE]

    new_rows <- data[c(i, i), ,]
    new_rows[["number"]][1] <- floor(new_rows[["number"]][1] / 2)
    new_rows[["number"]][2] <- ceiling(new_rows[["number"]][2] / 2)
    new_rows[["depth"]] <- new_rows[["depth"]] + 1

    data <- rbind(pre, new_rows, post)
    data[["position"]] <- seq_len(nrow(data))
    data
  }

  data <- if (can_explode(data)) {
    explode(data)
  } else if (can_split(data)) {
    split_number(data)
  } else {
    data
  }

  row.names(data) <- NULL
  data
}


f18_snailfish_rows <- function(x) {
  parts <- gsub(pattern = "(\\[|\\d+|,|\\])", "_\\1", x) |>
    strsplit("_") |>
    unlist() |>
    f_filter(function(xs) !is.element(xs, c("", ",")))

  f_replace_if <- function(xs, p, c) ifelse(p(xs), c, xs)
  f_which <- function(xs, p) which(p(xs))

  symbol_depths <- parts |>
    f_replace_if(function(xs) !xs %in% c("[", "]"), 0) |>
    f_replace_if(function(xs) xs == "[", 1) |>
    f_replace_if(function(xs) xs == "]", -1) |>
    as.numeric() |>
    cumsum()

  number_indices <- parts |>
    f_replace_if(function(xs) !xs %in% c("[", "]"), 0)  |>
    f_which(function(xs) xs == "0")

  numbers <- parts[number_indices] |> as.numeric()
  depths <- symbol_depths[number_indices] |> as.numeric()
  positions <- seq_along(number_indices)

  data.frame(
    number = numbers,
    depth = depths,
    position = positions
  )
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_data_18 <- function(example = 1) {
  l <- list(
    a = c(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]"
    ),
    b = c(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]",
      "[5,5]"
    ),
    c = c(
      "[1,1]",
      "[2,2]",
      "[3,3]",
      "[4,4]",
      "[5,5]",
      "[6,6]"
    ),
    d = c(
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
      "[7,[5,[[3,8],[1,4]]]]",
      "[[2,[2,2]],[8,[8,1]]]",
      "[2,9]",
      "[1,[[[9,3],9],[[9,0],[0,7]]]]",
      "[[[5,[7,4]],7],1]",
      "[[[[4,2],2],6],[8,7]]"
    ),
    e = c(
      "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
      "[[[5,[2,8]],4],[5,[[9,9],0]]]",
      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
      "[[[[5,4],[7,7]],8],[[8,3],8]]",
      "[[9,3],[[9,9],[6,[4,9]]]]",
      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    )
  )
  l[[example]]
}
