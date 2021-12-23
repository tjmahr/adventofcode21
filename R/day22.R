#' Day 22: Reactor Reboot
#'
#' [Reactor Reboot](https://adventofcode.com/2021/day/22)
#'
#' @name day22
#' @rdname day22
#' @details
#'
#' **Part One**
#'
#' Operating at these extreme ocean depths has overloaded the submarine\'s
#' reactor; it needs to be rebooted.
#'
#' The reactor core is made up of a large 3-dimensional grid made up
#' entirely of cubes, one cube per integer 3-dimensional coordinate
#' (`x,y,z`). Each cube can be either *on* or *off*; at the start of the
#' reboot process, they are all *off*. (Could it be an old model of a
#' reactor you\'ve seen [before](/2020/day/17)?)
#'
#' To reboot the reactor, you just need to set all of the cubes to either
#' *on* or *off* by following a list of *reboot steps* (your puzzle input).
#' Each step specifies a [cuboid](https://en.wikipedia.org/wiki/Cuboid)
#' (the set of all cubes that have coordinates which fall within ranges for
#' `x`, `y`, and `z`) and whether to turn all of the cubes in that cuboid
#' *on* or *off*.
#'
#' For example, given these reboot steps:
#'
#'     on x=10..12,y=10..12,z=10..12
#'     on x=11..13,y=11..13,z=11..13
#'     off x=9..11,y=9..11,z=9..11
#'     on x=10..10,y=10..10,z=10..10
#'
#' The first step (`on x=10..12,y=10..12,z=10..12`) turns *on* a 3x3x3
#' cuboid consisting of 27 cubes:
#'
#' -   `10,10,10`
#' -   `10,10,11`
#' -   `10,10,12`
#' -   `10,11,10`
#' -   `10,11,11`
#' -   `10,11,12`
#' -   `10,12,10`
#' -   `10,12,11`
#' -   `10,12,12`
#' -   `11,10,10`
#' -   `11,10,11`
#' -   `11,10,12`
#' -   `11,11,10`
#' -   `11,11,11`
#' -   `11,11,12`
#' -   `11,12,10`
#' -   `11,12,11`
#' -   `11,12,12`
#' -   `12,10,10`
#' -   `12,10,11`
#' -   `12,10,12`
#' -   `12,11,10`
#' -   `12,11,11`
#' -   `12,11,12`
#' -   `12,12,10`
#' -   `12,12,11`
#' -   `12,12,12`
#'
#' The second step (`on x=11..13,y=11..13,z=11..13`) turns *on* a 3x3x3
#' cuboid that overlaps with the first. As a result, only 19 additional
#' cubes turn on; the rest are already on from the previous step:
#'
#' -   `11,11,13`
#' -   `11,12,13`
#' -   `11,13,11`
#' -   `11,13,12`
#' -   `11,13,13`
#' -   `12,11,13`
#' -   `12,12,13`
#' -   `12,13,11`
#' -   `12,13,12`
#' -   `12,13,13`
#' -   `13,11,11`
#' -   `13,11,12`
#' -   `13,11,13`
#' -   `13,12,11`
#' -   `13,12,12`
#' -   `13,12,13`
#' -   `13,13,11`
#' -   `13,13,12`
#' -   `13,13,13`
#'
#' The third step (`off x=9..11,y=9..11,z=9..11`) turns *off* a 3x3x3
#' cuboid that overlaps partially with some cubes that are on, ultimately
#' turning off 8 cubes:
#'
#' -   `10,10,10`
#' -   `10,10,11`
#' -   `10,11,10`
#' -   `10,11,11`
#' -   `11,10,10`
#' -   `11,10,11`
#' -   `11,11,10`
#' -   `11,11,11`
#'
#' The final step (`on x=10..10,y=10..10,z=10..10`) turns *on* a single
#' cube, `10,10,10`. After this last step, `39` cubes are *on*.
#'
#' The initialization procedure only uses cubes that have `x`, `y`, and `z`
#' positions of at least `-50` and at most `50`. For now, ignore cubes
#' outside this region.
#'
#' Here is a larger example:
#'
#'     on x=-20..26,y=-36..17,z=-47..7
#'     on x=-20..33,y=-21..23,z=-26..28
#'     on x=-22..28,y=-29..23,z=-38..16
#'     on x=-46..7,y=-6..46,z=-50..-1
#'     on x=-49..1,y=-3..46,z=-24..28
#'     on x=2..47,y=-22..22,z=-23..27
#'     on x=-27..23,y=-28..26,z=-21..29
#'     on x=-39..5,y=-6..47,z=-3..44
#'     on x=-30..21,y=-8..43,z=-13..34
#'     on x=-22..26,y=-27..20,z=-29..19
#'     off x=-48..-32,y=26..41,z=-47..-37
#'     on x=-12..35,y=6..50,z=-50..-2
#'     off x=-48..-32,y=-32..-16,z=-15..-5
#'     on x=-18..26,y=-33..15,z=-7..46
#'     off x=-40..-22,y=-38..-28,z=23..41
#'     on x=-16..35,y=-41..10,z=-47..6
#'     off x=-32..-23,y=11..30,z=-14..3
#'     on x=-49..-5,y=-3..45,z=-29..18
#'     off x=18..30,y=-20..-8,z=-3..13
#'     on x=-41..9,y=-7..43,z=-33..15
#'     on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
#'     on x=967..23432,y=45373..81175,z=27513..53682
#'
#' The last two steps are fully outside the initialization procedure area;
#' all other steps are fully within it. After executing these steps in the
#' initialization procedure region, `590784` cubes are *on*.
#'
#' Execute the reboot steps. Afterward, considering only cubes in the
#' region `x=-50..50,y=-50..50,z=-50..50`, *how many cubes are on?*
#'
#' **Part Two**
#'
#' Now that the initialization procedure is complete, you can reboot the
#' reactor.
#'
#' Starting with all cubes *off*, run all of the *reboot steps* for all
#' cubes in the reactor.
#'
#' Consider the following reboot steps:
#'
#'     on x=-5..47,y=-31..22,z=-19..33
#'     on x=-44..5,y=-27..21,z=-14..35
#'     on x=-49..-1,y=-11..42,z=-10..38
#'     on x=-20..34,y=-40..6,z=-44..1
#'     off x=26..39,y=40..50,z=-2..11
#'     on x=-41..5,y=-41..6,z=-36..8
#'     off x=-43..-33,y=-45..-28,z=7..25
#'     on x=-33..15,y=-32..19,z=-34..11
#'     off x=35..47,y=-46..-34,z=-11..5
#'     on x=-14..36,y=-6..44,z=-16..29
#'     on x=-57795..-6158,y=29564..72030,z=20435..90618
#'     on x=36731..105352,y=-21140..28532,z=16094..90401
#'     on x=30999..107136,y=-53464..15513,z=8553..71215
#'     on x=13528..83982,y=-99403..-27377,z=-24141..23996
#'     on x=-72682..-12347,y=18159..111354,z=7391..80950
#'     on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
#'     on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
#'     on x=-52752..22273,y=-49450..9096,z=54442..119054
#'     on x=-29982..40483,y=-108474..-28371,z=-24328..38471
#'     on x=-4958..62750,y=40422..118853,z=-7672..65583
#'     on x=55694..108686,y=-43367..46958,z=-26781..48729
#'     on x=-98497..-18186,y=-63569..3412,z=1232..88485
#'     on x=-726..56291,y=-62629..13224,z=18033..85226
#'     on x=-110886..-34664,y=-81338..-8658,z=8914..63723
#'     on x=-55829..24974,y=-16897..54165,z=-121762..-28058
#'     on x=-65152..-11147,y=22489..91432,z=-58782..1780
#'     on x=-120100..-32970,y=-46592..27473,z=-11695..61039
#'     on x=-18631..37533,y=-124565..-50804,z=-35667..28308
#'     on x=-57817..18248,y=49321..117703,z=5745..55881
#'     on x=14781..98692,y=-1341..70827,z=15753..70151
#'     on x=-34419..55919,y=-19626..40991,z=39015..114138
#'     on x=-60785..11593,y=-56135..2999,z=-95368..-26915
#'     on x=-32178..58085,y=17647..101866,z=-91405..-8878
#'     on x=-53655..12091,y=50097..105568,z=-75335..-4862
#'     on x=-111166..-40997,y=-71714..2688,z=5609..50954
#'     on x=-16602..70118,y=-98693..-44401,z=5197..76897
#'     on x=16383..101554,y=4615..83635,z=-44907..18747
#'     off x=-95822..-15171,y=-19987..48940,z=10804..104439
#'     on x=-89813..-14614,y=16069..88491,z=-3297..45228
#'     on x=41075..99376,y=-20427..49978,z=-52012..13762
#'     on x=-21330..50085,y=-17944..62733,z=-112280..-30197
#'     on x=-16478..35915,y=36008..118594,z=-7885..47086
#'     off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
#'     off x=2032..69770,y=-71013..4824,z=7471..94418
#'     on x=43670..120875,y=-42068..12382,z=-24787..38892
#'     off x=37514..111226,y=-45862..25743,z=-16714..54663
#'     off x=25699..97951,y=-30668..59918,z=-15349..69697
#'     off x=-44271..17935,y=-9516..60759,z=49131..112598
#'     on x=-61695..-5813,y=40978..94975,z=8655..80240
#'     off x=-101086..-9439,y=-7088..67543,z=33935..83858
#'     off x=18020..114017,y=-48931..32606,z=21474..89843
#'     off x=-77139..10506,y=-89994..-18797,z=-80..59318
#'     off x=8476..79288,y=-75520..11602,z=-96624..-24783
#'     on x=-47488..-1262,y=24338..100707,z=16292..72967
#'     off x=-84341..13987,y=2429..92914,z=-90671..-1318
#'     off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
#'     off x=-27365..46395,y=31009..98017,z=15428..76570
#'     off x=-70369..-16548,y=22648..78696,z=-1892..86821
#'     on x=-53470..21291,y=-120233..-33476,z=-44150..38147
#'     off x=-93533..-4276,y=-16170..68771,z=-104985..-24507
#'
#' After running the above reboot steps, `2758514936282235` cubes are *on*.
#' (Just for [fun]{title="Well, *I* think it's fun."}, `474140` of those
#' are also in the initialization procedure region.)
#'
#' Starting again with all cubes *off*, execute all reboot steps.
#' Afterward, considering all cubes, *how many cubes are on?*
#'
#' @param x some data
#' @return For Part One, `f22a_flip_cubes_small(x)` returns the number of on
#'   cubes. For Part Two, `f22b(x)` returns ....
#' @export
#' @examples
#' f22a_flip_cubes_small(example_data_22())
#' f22b()
f22a_flip_cubes_small <- function(x) {
  # strategy: matrix subsetting on a 3d array
  d <- x |>
    f22_prepare_input() |>
    filter_rows(~ !too_big)

  a <- array(0, c(101, 101, 101))

  for (i in seq_len(nrow(d))) {
    row <- d[i, ]
    m <- expand.grid(
      x = row$x1:row$x2,
      y = row$y1:row$y2,
      z = row$z1:row$z2
    )
    # add 51 so -50 has index of 1
    a[as.matrix(m) + 51] <- ifelse(row$action == "on", 1, 0)
  }

  sum(a)

}


#' @rdname day22
#' @export
f22b_flip_cubes_big <- function(x) {
  # future::plan("multisession")

  seq_range <- function(r) seq(r[1], r[2])

  range_setdiff <- function(r1, r2) {
    diffs <- setdiff(seq_range(r1), seq_range(r2))
    l <- data.frame(a = numeric(0), b = numeric(0))
    # more than 0 non-overlapping regions
    if (length(diffs)) {
      pivot <- which(diff(diffs) != 1)
      # more than 1 non-overlapping region
      if (length(pivot)) {
        l[1, ] <- diffs[seq(1, pivot)] |> range()
        l[2, ] <- diffs[seq(pivot + 1, length(diffs))] |> range()
      } else {
        l[1, ] <- range(diffs)
      }
    }
    l
  }

  range_intersect <- function(r1, r2) {
    r <- intersect(seq_range(r1), seq_range(r2))
    if (length(r)) range_df(range(r)) else NULL
  }

  range_df <- function(r) {
    data.frame(a = r[1], b = r[2])
  }

  # for filtering rows
  range_overlap <- function(r1a, r1b, r2a, r2b) {
    min2_in <- r1a <= r2a & r2a <= r1b
    max2_in <- r1a <= r2b & r2b <= r1b
    min1_in <- r2a <= r1a & r1a <= r2b
    max1_in <- r2a <= r1b & r1b <= r2b
    min2_in | max2_in | min1_in | max1_in
  }

  compare_rows <- function(current, candidate) {
    if (current$action == "on" && candidate$action == "on") {
      new_rows <- find_new_on_regions(current, candidate)
    }
    if (current$action == "on" && candidate$action == "off") {
      new_rows <- find_new_on_regions(candidate, current)
    }
    new_rows
  }

  find_new_on_regions <- function(current, candidate) {
    x_cand <- unlist(candidate[1, 2:3])
    x_curr <- unlist(current[1, 2:3])
    new_x <- range_setdiff(x_cand, x_curr)
    old_x <- range_intersect(x_cand, x_curr)
    all_x <- range_df(x_cand)

    y_cand <- unlist(candidate[1, 4:5])
    y_curr <- unlist(current[1, 4:5])
    new_y <- range_setdiff(y_cand, y_curr)
    old_y <- range_intersect(y_cand, y_curr)
    all_y <- range_df(y_cand)

    z_cand <- unlist(candidate[1, 6:7])
    z_curr <- unlist(current[1, 6:7])
    new_z <- range_setdiff(z_cand, z_curr)
    old_z <- range_intersect(z_cand, z_curr)
    all_z <- range_df(z_cand)

    try_cbind <- function(a, b, c) {
      rows <- c(NROW(a), NROW(b), NROW(c))
      if (all(rows != 0)) cbind(a, b, c) else NULL
    }

    d <- rbind(
      try_cbind(new_x, all_y, all_z),
      try_cbind(old_x, new_y, all_z),
      try_cbind(old_x, old_y, new_z)
    )
    if (NROW(d)) {
      d <- cbind(action = candidate[["action"]], d)
    } else {
      d <- candidate[0, ]
    }
    names(d) <- names(candidate)
    d
  }

  # x <- example_data_22(2)
  d <- x |>
    f22_prepare_input()
  d$too_big <- NULL


  f_do_call <- function(x, f, ...) {
    do.call(f, x, ...)
  }
  # We are going to successively add nonoverlaping regions to the ranges
  # dataframe from the candidates datafram
  ranges <- d[1, ]
  candidates <- d[-1, ]
  steps <- seq_len(nrow(candidates))
  for (j in steps) {
    message(j)
    candidate <- candidates[1, ]

    if (nrow(candidate) == 0) {
      next
    }

    curr_candidates <- candidate

    ranges_to_check <- ranges |>
      filter_rows(
        ~ range_overlap(x1, x2, candidate$x1, candidate$x2),
        ~ range_overlap(y1, y2, candidate$y1, candidate$y2),
        ~ range_overlap(z1, z2, candidate$z1, candidate$z2)
      )


    if (candidate$action == "off") {
      ranges <- ranges |>
        split(seq_len(nrow(ranges))) |>
        lapply(function(curr) compare_rows(curr, candidate)) |>
        f_do_call(rbind)
    }

    if (candidate$action == "on") {
      # For each range in the set of nonoverlapping ranges,
      # find regions of the candidate that don't overlap. Any new regions
      # discovered row 1 need to be check individually for row 2, and so on.
      # That's why we store our finds as a dataframe of curr_candidates.
      for (row_i in seq_len(nrow(ranges_to_check))) {
        current <- ranges_to_check[row_i, ]
        curr_candidates <- curr_candidates |>
          split(seq_len(nrow(curr_candidates))) |>
          lapply(
            function(cand) compare_rows(current, cand)
          ) |>
          f_do_call(rbind)
      }
      ranges <- rbind(ranges, curr_candidates)
    }

    candidates <- candidates[-1, ]
  }

  span1 <- (ranges$x2 - ranges$x1) + 1
  span2 <- (ranges$y2 - ranges$y1) + 1
  span3 <- (ranges$z2 - ranges$z1) + 1
  sum(span1 * span2 * span3)
}

f22_scratch <- function(x) {


}


f22_prepare_input <- function(x) {
  # chr for character
  chr_remove <- function(x, pattern) gsub(pattern, "", x)
  chr_split_flatten <- function(x, ...) {
    unlist(strsplit(x, ...), use.names = FALSE)
  }

  x |>
    strsplit(" ") |>
    lapply(chr_split_flatten, ",") |>
    lapply(chr_split_flatten, "..", fixed = TRUE) |>
    lapply(chr_remove, ".=") |>
    # remaining steps: vector -> named vector -> list -> data.frame
    lapply(
      stats::setNames,
      c("action", "x1", "x2", "y1", "y2", "z1" , "z2")
    ) |>
    lapply(as.list) |>
    lapply(function(xs) {
      # convert types and check for points outside of -50,50
      xs[2:7] <- lapply(xs[2:7], as.numeric)
      vals <- unlist(xs[2:7], use.names = FALSE)
      xs["too_big"] <- any(abs(vals) > 50)
      xs
    }) |>
    lapply(as.data.frame) |>
    f_reduce(rbind)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day22
#' @export
example_data_22 <- function(example = 1) {
  l <- list(
    a = c(
      "on x=10..12,y=10..12,z=10..12",
      "on x=11..13,y=11..13,z=11..13",
      "off x=9..11,y=9..11,z=9..11",
      "on x=10..10,y=10..10,z=10..10"
    ),
    b = c(
      "on x=-20..26,y=-36..17,z=-47..7",
      "on x=-20..33,y=-21..23,z=-26..28",
      "on x=-22..28,y=-29..23,z=-38..16",
      "on x=-46..7,y=-6..46,z=-50..-1",
      "on x=-49..1,y=-3..46,z=-24..28",
      "on x=2..47,y=-22..22,z=-23..27",
      "on x=-27..23,y=-28..26,z=-21..29",
      "on x=-39..5,y=-6..47,z=-3..44",
      "on x=-30..21,y=-8..43,z=-13..34",
      "on x=-22..26,y=-27..20,z=-29..19",
      "off x=-48..-32,y=26..41,z=-47..-37",
      "on x=-12..35,y=6..50,z=-50..-2",
      "off x=-48..-32,y=-32..-16,z=-15..-5",
      "on x=-18..26,y=-33..15,z=-7..46",
      "off x=-40..-22,y=-38..-28,z=23..41",
      "on x=-16..35,y=-41..10,z=-47..6",
      "off x=-32..-23,y=11..30,z=-14..3",
      "on x=-49..-5,y=-3..45,z=-29..18",
      "off x=18..30,y=-20..-8,z=-3..13",
      "on x=-41..9,y=-7..43,z=-33..15",
      "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877",
      "on x=967..23432,y=45373..81175,z=27513..53682"
    ),
    c = c(
      "on x=-5..47,y=-31..22,z=-19..33",
      "on x=-44..5,y=-27..21,z=-14..35",
      "on x=-49..-1,y=-11..42,z=-10..38",
      "on x=-20..34,y=-40..6,z=-44..1",
      "off x=26..39,y=40..50,z=-2..11",
      "on x=-41..5,y=-41..6,z=-36..8",
      "off x=-43..-33,y=-45..-28,z=7..25",
      "on x=-33..15,y=-32..19,z=-34..11",
      "off x=35..47,y=-46..-34,z=-11..5",
      "on x=-14..36,y=-6..44,z=-16..29",
      "on x=-57795..-6158,y=29564..72030,z=20435..90618",
      "on x=36731..105352,y=-21140..28532,z=16094..90401",
      "on x=30999..107136,y=-53464..15513,z=8553..71215",
      "on x=13528..83982,y=-99403..-27377,z=-24141..23996",
      "on x=-72682..-12347,y=18159..111354,z=7391..80950",
      "on x=-1060..80757,y=-65301..-20884,z=-103788..-16709",
      "on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856",
      "on x=-52752..22273,y=-49450..9096,z=54442..119054",
      "on x=-29982..40483,y=-108474..-28371,z=-24328..38471",
      "on x=-4958..62750,y=40422..118853,z=-7672..65583",
      "on x=55694..108686,y=-43367..46958,z=-26781..48729",
      "on x=-98497..-18186,y=-63569..3412,z=1232..88485",
      "on x=-726..56291,y=-62629..13224,z=18033..85226",
      "on x=-110886..-34664,y=-81338..-8658,z=8914..63723",
      "on x=-55829..24974,y=-16897..54165,z=-121762..-28058",
      "on x=-65152..-11147,y=22489..91432,z=-58782..1780",
      "on x=-120100..-32970,y=-46592..27473,z=-11695..61039",
      "on x=-18631..37533,y=-124565..-50804,z=-35667..28308",
      "on x=-57817..18248,y=49321..117703,z=5745..55881",
      "on x=14781..98692,y=-1341..70827,z=15753..70151",
      "on x=-34419..55919,y=-19626..40991,z=39015..114138",
      "on x=-60785..11593,y=-56135..2999,z=-95368..-26915",
      "on x=-32178..58085,y=17647..101866,z=-91405..-8878",
      "on x=-53655..12091,y=50097..105568,z=-75335..-4862",
      "on x=-111166..-40997,y=-71714..2688,z=5609..50954",
      "on x=-16602..70118,y=-98693..-44401,z=5197..76897",
      "on x=16383..101554,y=4615..83635,z=-44907..18747",
      "off x=-95822..-15171,y=-19987..48940,z=10804..104439",
      "on x=-89813..-14614,y=16069..88491,z=-3297..45228",
      "on x=41075..99376,y=-20427..49978,z=-52012..13762",
      "on x=-21330..50085,y=-17944..62733,z=-112280..-30197",
      "on x=-16478..35915,y=36008..118594,z=-7885..47086",
      "off x=-98156..-27851,y=-49952..43171,z=-99005..-8456",
      "off x=2032..69770,y=-71013..4824,z=7471..94418",
      "on x=43670..120875,y=-42068..12382,z=-24787..38892",
      "off x=37514..111226,y=-45862..25743,z=-16714..54663",
      "off x=25699..97951,y=-30668..59918,z=-15349..69697",
      "off x=-44271..17935,y=-9516..60759,z=49131..112598",
      "on x=-61695..-5813,y=40978..94975,z=8655..80240",
      "off x=-101086..-9439,y=-7088..67543,z=33935..83858",
      "off x=18020..114017,y=-48931..32606,z=21474..89843",
      "off x=-77139..10506,y=-89994..-18797,z=-80..59318",
      "off x=8476..79288,y=-75520..11602,z=-96624..-24783",
      "on x=-47488..-1262,y=24338..100707,z=16292..72967",
      "off x=-84341..13987,y=2429..92914,z=-90671..-1318",
      "off x=-37810..49457,y=-71013..-7894,z=-105357..-13188",
      "off x=-27365..46395,y=31009..98017,z=15428..76570",
      "off x=-70369..-16548,y=22648..78696,z=-1892..86821",
      "on x=-53470..21291,y=-120233..-33476,z=-44150..38147",
      "off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"
    )
  )
  l[[example]]
}
