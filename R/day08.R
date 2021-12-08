#' Day 08: Seven Segment Search
#'
#' [Seven Segment Search](https://adventofcode.com/2021/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' You barely reach the safety of the cave when the whale smashes into the
#' cave mouth, collapsing it. Sensors indicate another exit to this cave at
#' a much greater depth, so you have no choice but to press on.
#'
#' As your submarine slowly makes its way through the cave system, you
#' notice that the four-digit [seven-segment
#' displays](https://en.wikipedia.org/wiki/Seven-segment_display) in your
#' submarine are malfunctioning; [they must have been
#' damaged]{title="Yes, just the four-digit seven-segment ones. Whole batch must have been faulty."}
#' during the escape. You\'ll be in a lot of trouble without them, so
#' you\'d better figure out what\'s wrong.
#'
#' Each digit of a seven-segment display is rendered by turning on or off
#' any of seven segments named `a` through `g`:
#'
#'       0:      1:      2:      3:      4:
#'      aaaa    ....    aaaa    aaaa    ....
#'     b    c  .    c  .    c  .    c  b    c
#'     b    c  .    c  .    c  .    c  b    c
#'      ....    ....    dddd    dddd    dddd
#'     e    f  .    f  e    .  .    f  .    f
#'     e    f  .    f  e    .  .    f  .    f
#'      gggg    ....    gggg    gggg    ....
#'
#'       5:      6:      7:      8:      9:
#'      aaaa    aaaa    aaaa    aaaa    aaaa
#'     b    .  b    .  .    c  b    c  b    c
#'     b    .  b    .  .    c  b    c  b    c
#'      dddd    dddd    ....    dddd    dddd
#'     .    f  e    f  .    f  e    f  .    f
#'     .    f  e    f  .    f  e    f  .    f
#'      gggg    gggg    ....    gggg    gggg
#'
#' So, to render a `1`, only segments `c` and `f` would be turned on; the
#' rest would be off. To render a `7`, only segments `a`, `c`, and `f`
#' would be turned on.
#'
#' The problem is that the signals which control the segments have been
#' mixed up on each display. The submarine is still trying to display
#' numbers by producing output on signal wires `a` through `g`, but those
#' wires are connected to segments *randomly*. Worse, the wire/segment
#' connections are mixed up separately for each four-digit display! (All of
#' the digits *within* a display use the same connections, though.)
#'
#' So, you might know that only signal wires `b` and `g` are turned on, but
#' that doesn\'t mean *segments* `b` and `g` are turned on: the only digit
#' that uses two segments is `1`, so it must mean segments `c` and `f` are
#' meant to be on. With just that information, you still can\'t tell which
#' wire (`b`/`g`) goes to which segment (`c`/`f`). For that, you\'ll need
#' to collect more information.
#'
#' For each display, you watch the changing signals for a while, make a
#' note of *all ten unique signal patterns* you see, and then write down a
#' single *four digit output value* (your puzzle input). Using the signal
#' patterns, you should be able to work out which pattern corresponds to
#' which digit.
#'
#' For example, here is what you might see in a single entry in your notes:
#'
#'     acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#'     cdfeb fcadb cdfeb cdbaf
#'
#' (The entry is wrapped here to two lines so it fits; in your notes, it
#' will all be on a single line.)
#'
#' Each entry consists of ten *unique signal patterns*, a `|` delimiter,
#' and finally the *four digit output value*. Within an entry, the same
#' wire/segment connections are used (but you don\'t know what the
#' connections actually are). The unique signal patterns correspond to the
#' ten different ways the submarine tries to render a digit using the
#' current wire/segment connections. Because `7` is the only digit that
#' uses three segments, `dab` in the above example means that to render a
#' `7`, signal lines `d`, `a`, and `b` are on. Because `4` is the only
#' digit that uses four segments, `eafb` means that to render a `4`, signal
#' lines `e`, `a`, `f`, and `b` are on.
#'
#' Using this information, you should be able to work out which combination
#' of signal wires corresponds to each of the ten digits. Then, you can
#' decode the four digit output value. Unfortunately, in the above example,
#' all of the digits in the output value (`cdfeb fcadb cdfeb cdbaf`) use
#' five segments and are more difficult to deduce.
#'
#' For now, *focus on the easy digits*. Consider this larger example:
#'
#'     be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
#'     fdgacbe cefdb cefbgd gcbe
#'     edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
#'     fcgedb cgb dgebacf gc
#'     fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
#'     cg cg fdcagb cbg
#'     fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
#'     efabcd cedba gadfec cb
#'     aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
#'     gecf egdcabf bgf bfgea
#'     fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
#'     gebdcfa ecba ca fadegcb
#'     dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
#'     cefg dcbef fcge gbcadfe
#'     bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
#'     ed bcgafe cdgba cbgef
#'     egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
#'     gbdfcae bgc cg cgb
#'     gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
#'     fgae cfgab fg bagce
#'
#' Because the digits `1`, `4`, `7`, and `8` each use a unique number of
#' segments, you should be able to tell which combinations of signals
#' correspond to those digits. Counting *only digits in the output values*
#' (the part after `|` on each line), in the above example, there are `26`
#' instances of digits that use a unique number of segments (highlighted
#' above).
#'
#' *In the output values, how many times do digits `1`, `4`, `7`, or `8`
#' appear?*
#'
#' **Part Two**
#'
#' Through a little deduction, you should now be able to determine the
#' remaining digits. Consider again the first example above:
#'
#'     acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#'     cdfeb fcadb cdfeb cdbaf
#'
#' After some careful analysis, the mapping between signal wires and
#' segments only make sense in the following configuration:
#'
#'      dddd
#'     e    a
#'     e    a
#'      ffff
#'     g    b
#'     g    b
#'      cccc
#'
#' So, the unique signal patterns would correspond to the following digits:
#'
#' -   `acedgfb`: `8`
#' -   `cdfbe`: `5`
#' -   `gcdfa`: `2`
#' -   `fbcad`: `3`
#' -   `dab`: `7`
#' -   `cefabd`: `9`
#' -   `cdfgeb`: `6`
#' -   `eafb`: `4`
#' -   `cagedb`: `0`
#' -   `ab`: `1`
#'
#' Then, the four digits of the output value can be decoded:
#'
#' -   `cdfeb`: `5`
#' -   `fcadb`: `3`
#' -   `cdfeb`: `5`
#' -   `cdbaf`: `3`
#'
#' Therefore, the output value for this entry is `5353`.
#'
#' Following this same process for each entry in the second, larger example
#' above, the output value of each entry can be determined:
#'
#' -   `fdgacbe cefdb cefbgd gcbe`: `8394`
#' -   `fcgedb cgb dgebacf gc`: `9781`
#' -   `cg cg fdcagb cbg`: `1197`
#' -   `efabcd cedba gadfec cb`: `9361`
#' -   `gecf egdcabf bgf bfgea`: `4873`
#' -   `gebdcfa ecba ca fadegcb`: `8418`
#' -   `cefg dcbef fcge gbcadfe`: `4548`
#' -   `ed bcgafe cdgba cbgef`: `1625`
#' -   `gbdfcae bgc cg cgb`: `8717`
#' -   `fgae cfgab fg bagce`: `4315`
#'
#' Adding all of the output values in this larger example produces `61229`.
#'
#' For each entry, determine all of the wire/segment connections and decode
#' the four-digit output values. *What do you get if you add up all of the
#' output values?*
#'
#' @param x some data
#' @return For Part One, `f08a_count_1478s(x)` returns the number of 1, 4, 7,and
#'   8s in the output digits. For Part Two, `f08b_solve_and_sum_output(x)`
#'   returns returns the sum of output digits.
#' @export
#' @examples
#' f08a_count_1478s(example_data_08())
#' f08b_solve_and_sum_output(example_data_08())
f08a_count_1478s <- function(x) {
  # strategy: split-apply-combine
  output <- strsplit(x, " \\| ") |>
    lapply(function(x) x[[2]]) |>
    lapply(strsplit, " ") |>
    lapply(unlist)
  counts <- output |>
    unlist() |>
    nchar() |>
    table()
  number_of_1478s <- counts[c("2", "4", "3" , "7")] |> sum()
  number_of_1478s
}


#' @rdname day08
#' @export
f08b_solve_and_sum_output <- function(x) {
  # strategy: set operations
  characterize <- function(x) {
    unlist(strsplit(x, ""))
  }

  solve_digit_set <- function(d) {
    # find uniquely length-ed characters
    chars <- list()
    chars$c1 <- d[which(nchar(d) == 2)] |> characterize()
    chars$c4 <- d[which(nchar(d) == 4)] |> characterize()
    chars$c7 <- d[which(nchar(d) == 3)] |> characterize()
    chars$c8 <- d[which(nchar(d) == 7)] |> characterize()

    map_mystery <- d |> lapply(characterize)
    positions <- numeric(7)

    # solve for top row
    positions[1] <- chars$c7 |>
      setdiff(chars$c1)

    # if you remove the right side, then all the four unit
    # digits will intersect on top row and bottom_row
    positions[7] <- map_mystery |>
      lapply(setdiff, chars$c1) |>
      f_filter(function(x) length(x) == 4) |>
      f_reduce(intersect) |>
      setdiff(positions[1])

    # middle row: subtract top, bottom and 1's pieces from 3
    positions[4] <- map_mystery |>
      lapply(setdiff, chars$c1) |>
      lapply(setdiff, c(positions[1], positions[7])) |>
      f_filter(function(x) length(x) == 1) |>
      unlist()

    positions[2] <- chars$c4 |>
      setdiff(c(positions[4], chars$c1))

    # find a unique side in 5
    positions[6] <- map_mystery |>
      lapply(setdiff, c(positions[c(1, 2, 4, 7)])) |>
      f_filter(function(x) length(x) == 1) |>
      unlist()

    # find the side not in 1 or 5
    positions[5] <- map_mystery |>
      lapply(setdiff, c(positions, chars$c1)) |>
      f_filter(function(x) length(x) == 1) |>
      unlist() |>
      unique()

    # one unknown side leftover
    positions[3] <- setdiff(letters[1:7], positions)
    positions
  }

  convert_string_to_digit <- function(s, positions) {
    key <- list()
    key$s0 <- c(1, 2, 3,    5, 6, 7)
    key$s1 <- c(      3,       6   )
    key$s2 <- c(1,    3, 4, 5,    7)
    key$s3 <- c(1,    3, 4,    6, 7)
    key$s4 <- c(   2, 3, 4,    6   )
    key$s5 <- c(1, 2,    4,    6, 7)
    key$s6 <- c(1, 2,    4, 5, 6, 7)
    key$s7 <- c(1,    3,       6   )
    key$s8 <- c(1, 2, 3, 4, 5, 6, 7)
    key$s9 <- c(1, 2, 3, 4,    6, 7)

    sides <- characterize(s) |> match(positions) |> sort() |> as.numeric()

    l <- c(unname(key), list(sides)) |>
      anyDuplicated(fromLast = TRUE)
    # zero is in position one
    l - 1
  }

  convert_strings_to_digits <- Vectorize(convert_string_to_digit, "s")

  solve_line <- function(digits, outputs) {
    positions <- solve_digit_set(digits)
    output_digits <- convert_strings_to_digits(outputs, positions)
    sum(output_digits * c(1000, 100, 10, 1))
  }

  # x <- example_data_08()
  digit_set <- strsplit(x, " \\| ") |>
    lapply(strsplit, " ") |>
    lapply(function(x) x[[1]])
  output_set <- strsplit(x, " \\| ") |>
    lapply(strsplit, " ") |>
    lapply(function(x) x[[2]])

  Map(solve_line, digit_set, output_set) |>
    unlist() |>
    sum()
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = c(
      "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    ),
    b = c(
      "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    )
  )
  l[[example]]
}
