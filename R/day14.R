#' Day 14: Extended Polymerization
#'
#' [Extended Polymerization](https://adventofcode.com/2021/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' The incredible pressures at this depth are starting to put a strain on
#' your submarine. The submarine has
#' [polymerization](https://en.wikipedia.org/wiki/Polymerization) equipment
#' that would produce suitable materials to reinforce the submarine, and
#' the nearby volcanically-active caves should even have the necessary
#' input elements in sufficient quantities.
#'
#' The submarine manual contains [instructions]{title="HO
#'
#' HO -> OH"} for finding the optimal polymer formula; specifically, it
#' offers a *polymer template* and a list of *pair insertion* rules (your
#' puzzle input). You just need to work out what polymer would result after
#' repeating the pair insertion process a few times.
#'
#' For example:
#'
#'     NNCB
#'
#'     CH -> B
#'     HH -> N
#'     CB -> H
#'     NH -> C
#'     HB -> C
#'     HC -> B
#'     HN -> C
#'     NN -> C
#'     BH -> H
#'     NC -> B
#'     NB -> B
#'     BN -> B
#'     BB -> N
#'     BC -> B
#'     CC -> N
#'     CN -> C
#'
#' The first line is the *polymer template* - this is the starting point of
#' the process.
#'
#' The following section defines the *pair insertion* rules. A rule like
#' `AB -> C` means that when elements `A` and `B` are immediately adjacent,
#' element `C` should be inserted between them. These insertions all happen
#' simultaneously.
#'
#' So, starting with the polymer template `NNCB`, the first step
#' simultaneously considers all three pairs:
#'
#' -   The first pair (`NN`) matches the rule `NN -> C`, so element `C` is
#'     inserted between the first `N` and the second `N`.
#' -   The second pair (`NC`) matches the rule `NC -> B`, so element `B` is
#'     inserted between the `N` and the `C`.
#' -   The third pair (`CB`) matches the rule `CB -> H`, so element `H` is
#'     inserted between the `C` and the `B`.
#'
#' Note that these pairs overlap: the second element of one pair is the
#' first element of the next pair. Also, because all pairs are considered
#' simultaneously, inserted elements are not considered to be part of a
#' pair until the next step.
#'
#' After the first step of this process, the polymer becomes `NCNBCHB`.
#'
#' Here are the results of a few steps using the above rules:
#'
#'     Template:     NNCB
#'     After step 1: NCNBCHB
#'     After step 2: NBCCNBBBCBHCB
#'     After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
#'     After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#'
#' This polymer grows quickly. After step 5, it has length 97; After step
#' 10, it has length 3073. After step 10, `B` occurs 1749 times, `C` occurs
#' 298 times, `H` occurs 161 times, and `N` occurs 865 times; taking the
#' quantity of the most common element (`B`, 1749) and subtracting the
#' quantity of the least common element (`H`, 161) produces
#' `1749 - 161 = 1588`.
#'
#' Apply 10 steps of pair insertion to the polymer template and find the
#' most and least common elements in the result. *What do you get if you
#' take the quantity of the most common element and subtract the quantity
#' of the least common element?*
#'
#' **Part Two**
#'
#' The resulting polymer isn\'t nearly strong enough to reinforce the
#' submarine. You\'ll need to run more steps of the pair insertion process;
#' a total of *40 steps* should do it.
#'
#' In the above example, the most common element is `B` (occurring
#' `2192039569602` times) and the least common element is `H` (occurring
#' `3849876073` times); subtracting these produces `2188189693529`.
#'
#' Apply *40* steps of pair insertion to the polymer template and find the
#' most and least common elements in the result. *What do you get if you
#' take the quantity of the most common element and subtract the quantity
#' of the least common element?*
#'
#' @param x some data
#' @param n number of steps to run
#' @return For Part One, `f14a_grow_polymer(x)` returns a vector of characters
#'   in the final string. For Part Two, `f14b_grow_polymer_fast(x)` returns the
#'   counts of characters in the final string.
#' @export
#' @examples
#' f14a_grow_polymer(example_data_14())
#' f14b_grow_polymer_fast(example_data_14())
f14a_grow_polymer <- function(x, n = 1) {
  # strategy: lookup vectors
  l <- f14_prepare_input(x)
  seed <- l$seed
  mapping <- l$mapping

  str_chars <- function(x) strsplit(x, "")[[1]]
  f_grow_once <- function(x, mapping) {
    x |>
      f14_make_character_pairs() |>
      f14_apply_replacements(mapping = mapping) |>
      str_chars()
  }

  seed |>
    f_loop_n(n, f_grow_once, mapping = mapping)
}


#' @rdname day14
#' @export
f14b_grow_polymer_fast <- function(x, n) {
  # strategy: making a graph and exponentiating the transition matrix
  create_adj_matrix <- function(mapping) {
    all_pairs <- mapping |> names() |> sort()
    m <- matrix(0, ncol = length(all_pairs), nrow = length(all_pairs))
    rownames(m) <- all_pairs
    colnames(m) <- all_pairs
    for (pair in all_pairs) {
      rows <- pair
      cols <- mapping[[pair]]
      m[rows, cols] <- 1
    }
    m
  }

  count_with_matrix_powers <- function(seed, mapping, n = 1) {
    m <- create_adj_matrix(mapping)

    pairs <- f14_make_character_pairs(seed)
    real_pairs <- pairs[-length(pairs)]
    dangling_item <-  pairs[length(pairs)]

    # in the example no pairs are repeated but we might have repeated
    # pairs so we need to use a count
    initial_names <- names(table(real_pairs))
    initial_ns <- table(real_pairs)

    starting_row <- m[1, ]
    starting_row[] <- 0
    starting_row[initial_names] <- initial_ns

    # initialize m to identity matrix so n = 1 works
    mx <- m
    mx[] <- 0
    diag(mx) <- 1

    # Exponentiate n times
    for (i in seq_len(n)) {
      mx <- m %*% mx
    }

    y <- starting_row %*% mx

    # Because end of a pair is a start of another pair we only count the
    # the frequency of the first character in each pair
    char_counts <- colnames(y) |>
      strsplit("") |>
      # lop off second character
      lapply(utils::head, 1) |>
      lapply(table) |>
      f_map2(y, function(x, y) x * y ) |>
      f_reduce(c)

    final_count <- char_counts |>
      split(names(char_counts)) |>
      lapply(sum) |>
      unlist()

    final_count[dangling_item] <- final_count[dangling_item] + 1
    final_count
  }

  l <- f14_prepare_input(x)
  counts <- count_with_matrix_powers(l$seed, l$mapping2, n)
  # for n = 0 case, we don't want to as-yet unseen elements
  counts[counts != 0]
}


f14_make_character_pairs <- function(x) {
  n <- length(x)
  # Include an incomplete pair at the end so that it is
  # easier to reconstruct the string later
  c(paste0(x[seq_len(n - 1)], x[seq_len(n - 1) + 1]), x[n])
}


f14_apply_replacements <- function(x, mapping) {
  mapping[x] |>
    lapply(head, 2) |>
    unlist(use.names = FALSE) |>
    paste0(collapse = "")
}


f14_prepare_input <- function(x) {
  seed <- x[1]
  rules <- x[c(-1, -2)]

  mapping <- rules |>
    lapply(
      function(x) c(substr(x, 1, 1), substr(x, 7, 7), substr(x, 2, 2))
    ) |>
    stats::setNames(substr(rules, 1, 2)) |>
    # Map a single character to itself
    append(stats::setNames(LETTERS, LETTERS))

  # alternative form that is better for part 2
  mapping2 <- rules |>
    lapply(
      function(x) {
        c(
          paste0(substr(x, 1, 1), substr(x, 7, 7)),
          paste0(substr(x, 7, 7), substr(x, 2, 2))
        )
      }
    ) |>
    stats::setNames(substr(rules, 1, 2))

  list(
    seed = strsplit(seed, "")[[1]],
    mapping = mapping,
    mapping2 = mapping2
  )
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = c(
      "NNCB",
      "",
      "CH -> B",
      "HH -> N",
      "CB -> H",
      "NH -> C",
      "HB -> C",
      "HC -> B",
      "HN -> C",
      "NN -> C",
      "BH -> H",
      "NC -> B",
      "NB -> B",
      "BN -> B",
      "BB -> N",
      "BC -> B",
      "CC -> N",
      "CN -> C"
    )
  )
  l[[example]]
}
