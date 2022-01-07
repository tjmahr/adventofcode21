#' Day 16: Packet Decoder
#'
#' [Packet Decoder](https://adventofcode.com/2021/day/16)
#'
#' @name day16
#' @rdname day16
#' @details
#'
#' **Part One**
#'
#' As you leave the cave and reach open waters, you receive a transmission
#' from the Elves back on the ship.
#'
#' The transmission was sent using the Buoyancy Interchange Transmission
#' System
#' ([BITS]{title="Just be glad it wasn't sent using the BuoyancY Transmission Encoding System."}),
#' a method of packing numeric expressions into a binary sequence. Your
#' submarine\'s computer has saved the transmission in
#' [hexadecimal](https://en.wikipedia.org/wiki/Hexadecimal) (your puzzle
#' input).
#'
#' The first step of decoding the message is to convert the hexadecimal
#' representation into binary. Each character of hexadecimal corresponds to
#' four bits of binary data:
#'
#'     0 = 0000
#'     1 = 0001
#'     2 = 0010
#'     3 = 0011
#'     4 = 0100
#'     5 = 0101
#'     6 = 0110
#'     7 = 0111
#'     8 = 1000
#'     9 = 1001
#'     A = 1010
#'     B = 1011
#'     C = 1100
#'     D = 1101
#'     E = 1110
#'     F = 1111
#'
#' The BITS transmission contains a single *packet* at its outermost layer
#' which itself contains many other packets. The hexadecimal representation
#' of this packet might encode a few extra `0` bits at the end; these are
#' not part of the transmission and should be ignored.
#'
#' Every packet begins with a standard header: the first three bits encode
#' the packet *version*, and the next three bits encode the packet *type
#' ID*. These two values are numbers; all numbers encoded in any packet are
#' represented as binary with the most significant bit first. For example,
#' a version encoded as the binary sequence `100` represents the number
#' `4`.
#'
#' Packets with type ID `4` represent a *literal value*. Literal value
#' packets encode a single binary number. To do this, the binary number is
#' padded with leading zeroes until its length is a multiple of four bits,
#' and then it is broken into groups of four bits. Each group is prefixed
#' by a `1` bit except the last group, which is prefixed by a `0` bit.
#' These groups of five bits immediately follow the packet header. For
#' example, the hexadecimal string `D2FE28` becomes:
#'
#'     110100101111111000101000
#'     VVVTTTAAAAABBBBBCCCCC
#'
#' Below each bit is a label indicating its purpose:
#'
#' -   The three bits labeled `V` (`110`) are the packet version, `6`.
#' -   The three bits labeled `T` (`100`) are the packet type ID, `4`,
#'     which means the packet is a literal value.
#' -   The five bits labeled `A` (`10111`) start with a `1` (not the last
#'     group, keep reading) and contain the first four bits of the number,
#'     `0111`.
#' -   The five bits labeled `B` (`11110`) start with a `1` (not the last
#'     group, keep reading) and contain four more bits of the number,
#'     `1110`.
#' -   The five bits labeled `C` (`00101`) start with a `0` (last group,
#'     end of packet) and contain the last four bits of the number, `0101`.
#' -   The three unlabeled `0` bits at the end are extra due to the
#'     hexadecimal representation and should be ignored.
#'
#' So, this packet represents a literal value with binary representation
#' `011111100101`, which is `2021` in decimal.
#'
#' Every other type of packet (any packet with a type ID other than `4`)
#' represent an *operator* that performs some calculation on one or more
#' sub-packets contained within. Right now, the specific operations aren\'t
#' important; focus on parsing the hierarchy of sub-packets.
#'
#' An operator packet contains one or more packets. To indicate which
#' subsequent binary data represents its sub-packets, an operator packet
#' can use one of two modes indicated by the bit immediately after the
#' packet header; this is called the *length type ID*:
#'
#' -   If the length type ID is `0`, then the next *15* bits are a number
#'     that represents the *total length in bits* of the sub-packets
#'     contained by this packet.
#' -   If the length type ID is `1`, then the next *11* bits are a number
#'     that represents the *number of sub-packets immediately contained* by
#'     this packet.
#'
#' Finally, after the length type ID bit and the 15-bit or 11-bit field,
#' the sub-packets appear.
#'
#' For example, here is an operator packet (hexadecimal string
#' `38006F45291200`) with length type ID `0` that contains two sub-packets:
#'
#'     00111000000000000110111101000101001010010001001000000000
#'     VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
#'
#' -   The three bits labeled `V` (`001`) are the packet version, `1`.
#' -   The three bits labeled `T` (`110`) are the packet type ID, `6`,
#'     which means the packet is an operator.
#' -   The bit labeled `I` (`0`) is the length type ID, which indicates
#'     that the length is a 15-bit number representing the number of bits
#'     in the sub-packets.
#' -   The 15 bits labeled `L` (`000000000011011`) contain the length of
#'     the sub-packets in bits, `27`.
#' -   The 11 bits labeled `A` contain the first sub-packet, a literal
#'     value representing the number `10`.
#' -   The 16 bits labeled `B` contain the second sub-packet, a literal
#'     value representing the number `20`.
#'
#' After reading 11 and 16 bits of sub-packet data, the total length
#' indicated in `L` (27) is reached, and so parsing of this packet stops.
#'
#' As another example, here is an operator packet (hexadecimal string
#' `EE00D40C823060`) with length type ID `1` that contains three
#' sub-packets:
#'
#'     11101110000000001101010000001100100000100011000001100000
#'     VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
#'
#' -   The three bits labeled `V` (`111`) are the packet version, `7`.
#' -   The three bits labeled `T` (`011`) are the packet type ID, `3`,
#'     which means the packet is an operator.
#' -   The bit labeled `I` (`1`) is the length type ID, which indicates
#'     that the length is a 11-bit number representing the number of
#'     sub-packets.
#' -   The 11 bits labeled `L` (`00000000011`) contain the number of
#'     sub-packets, `3`.
#' -   The 11 bits labeled `A` contain the first sub-packet, a literal
#'     value representing the number `1`.
#' -   The 11 bits labeled `B` contain the second sub-packet, a literal
#'     value representing the number `2`.
#' -   The 11 bits labeled `C` contain the third sub-packet, a literal
#'     value representing the number `3`.
#'
#' After reading 3 complete sub-packets, the number of sub-packets
#' indicated in `L` (3) is reached, and so parsing of this packet stops.
#'
#' For now, parse the hierarchy of the packets throughout the transmission
#' and *add up all of the version numbers*.
#'
#' Here are a few more examples of hexadecimal-encoded transmissions:
#'
#' -   `8A004A801A8002F478` represents an operator packet (version 4) which
#'     contains an operator packet (version 1) which contains an operator
#'     packet (version 5) which contains a literal value (version 6); this
#'     packet has a version sum of `16`.
#' -   `620080001611562C8802118E34` represents an operator packet
#'     (version 3) which contains two sub-packets; each sub-packet is an
#'     operator packet that contains two literal values. This packet has a
#'     version sum of `12`.
#' -   `C0015000016115A2E0802F182340` has the same structure as the
#'     previous example, but the outermost packet uses a different length
#'     type ID. This packet has a version sum of `23`.
#' -   `A0016C880162017C3686B18A3D4780` is an operator packet that contains
#'     an operator packet that contains an operator packet that contains
#'     five literal values; it has a version sum of `31`.
#'
#' Decode the structure of your hexadecimal-encoded BITS transmission;
#' *what do you get if you add up the version numbers in all packets?*
#'
#' **Part Two**
#'
#' **Part Two**
#' Now that you have the structure of your transmission decoded, you can
#' calculate the value of the expression it represents.
#'
#' Literal values (type ID `4`) represent a single number as described
#' above. The remaining type IDs are more interesting:
#'
#' -   Packets with type ID `0` are *sum* packets - their value is the sum
#'     of the values of their sub-packets. If they only have a single
#'     sub-packet, their value is the value of the sub-packet.
#' -   Packets with type ID `1` are *product* packets - their value is the
#'     result of multiplying together the values of their sub-packets. If
#'     they only have a single sub-packet, their value is the value of the
#'     sub-packet.
#' -   Packets with type ID `2` are *minimum* packets - their value is the
#'     minimum of the values of their sub-packets.
#' -   Packets with type ID `3` are *maximum* packets - their value is the
#'     maximum of the values of their sub-packets.
#' -   Packets with type ID `5` are *greater than* packets - their value is
#'     *1* if the value of the first sub-packet is greater than the value
#'     of the second sub-packet; otherwise, their value is *0*. These
#'     packets always have exactly two sub-packets.
#' -   Packets with type ID `6` are *less than* packets - their value is
#'     *1* if the value of the first sub-packet is less than the value of
#'     the second sub-packet; otherwise, their value is *0*. These packets
#'     always have exactly two sub-packets.
#' -   Packets with type ID `7` are *equal to* packets - their value is *1*
#'     if the value of the first sub-packet is equal to the value of the
#'     second sub-packet; otherwise, their value is *0*. These packets
#'     always have exactly two sub-packets.
#'
#' Using these rules, you can now work out the value of the outermost
#' packet in your BITS transmission.
#'
#' For example:
#'
#' -   `C200B40A82` finds the sum of `1` and `2`, resulting in the value
#'     `3`.
#' -   `04005AC33890` finds the product of `6` and `9`, resulting in the
#'     value `54`.
#' -   `880086C3E88112` finds the minimum of `7`, `8`, and `9`, resulting
#'     in the value `7`.
#' -   `CE00C43D881120` finds the maximum of `7`, `8`, and `9`, resulting
#'     in the value `9`.
#' -   `D8005AC2A8F0` produces `1`, because `5` is less than `15`.
#' -   `F600BC2D8F` produces `0`, because `5` is not greater than `15`.
#' -   `9C005AC2F8F0` produces `0`, because `5` is not equal to `15`.
#' -   `9C0141080250320F1802104A08` produces `1`, because `1` + `3` =
#'     `2` \* `2`.
#'
#' *What do you get if you evaluate the expression represented by your
#' hexadecimal-encoded BITS transmission?*
#'
#' @param x some data
#' @return For Part One, `f16a_sum_packet_versions(x)` returns the sum of packet
#'   versions. For Part Two, `f16b_eval_packets(x)` returns the result of the
#'   evaluating the code in the packets.
#' @export
#' @examples
#' f16a_sum_packet_versions(example_data_16()[1])
#' f16b_eval_packets(example_data_16()[6])
f16a_sum_packet_versions <- function(x) {
  # this day was the last finished because the changes from part 1 to part 2
  # broke part 1. there is a lot of redundancy here that I do not like. I wish
  # base R had a rapply() that was more user friendly. I couldn't figure it out.
  #
  # I also made a mistake of not combining the literal values into a single
  # number before the evaluation. the examples all used small numbers where
  # this was not an issue, so I didn't detect the problem until too late.

  # strategy: recursion
  bits <- f16_prepare_bits(x)
  packets <- f16_parse_packets(bits)
  p <- unlist(packets)
  p <- p[grepl("version", names(p))]
  p |>
    unlist() |>
    strtoi(2) |>
    sum()
}


#' @rdname day16
#' @export
f16b_eval_packets <- function(x) {
  # strategy: construct and evaluate R code

  # recursively construct commands
  make_code <- function(packets) {
    commands <- character(length = length(packets))

    for (packet_i in seq_along(packets)) {
      this_packet <- packets[[packet_i]]
      if (is.list(this_packet)) {
        # FUN(ARGS): if we have subpackets, then the op is FUN and ARGS are the
        # recursive calls on the subpackets. otherwise, we have literal value
        if (!is.null(this_packet$subpackets)) {
          command <- sprintf(
            "%s(%s)",
            this_packet$op,
            make_code(this_packet$subpackets)
          )
        } else {
          command <- this_packet$op
        }
        commands[packet_i] <- command
      }
    }
    paste0(commands, collapse = ", ")
  }
  v_sum <- function(...) sum(c(...))
  v_prod <- function(...) prod(c(...))
  v_min <- function(...) min(c(...))
  v_max <- function(...) max(c(...))
  v_gt <- function(...) as.numeric(..1 > ..2)
  v_lt <- function(...) as.numeric(..1 < ..2)
  v_eq <- function(...) as.numeric(..1 == ..2)

  bits <- f16_prepare_bits(x)
  packets <- f16_parse_packets(bits)
  code <- make_code(packets)
  result <- eval(parse(text = code))
  result
}


f16_prepare_bits <- function(x) {
  apply_mapping <- function(xs) {
    mapping <- c(
      "0" = "0000",
      "1" = "0001",
      "2" = "0010",
      "3" = "0011",
      "4" = "0100",
      "5" = "0101",
      "6" = "0110",
      "7" = "0111",
      "8" = "1000",
      "9" = "1001",
      "A" = "1010",
      "B" = "1011",
      "C" = "1100",
      "D" = "1101",
      "E" = "1110",
      "F" = "1111"
    )
    paste0(mapping[xs], collapse = "")
  }

  x |> strsplit("") |> unlist() |> apply_mapping()
}


f16_parse_packets <- function(x) {
  is_terminated <- function(x) {
    has_a_1 <- grepl("1", x)
    is_empty <- x == ""
    !has_a_1 || is_empty
  }

  set_op <- function(data) {
    map <- c(
      "000" = "v_sum",
      "001" = "v_prod",
      "010" = "v_min",
      "011" = "v_max",
      "100" = "v_val",
      "101" = "v_gt",
      "110" = "v_lt",
      "111" = "v_eq"
    )
    data$op <- map[data$type]
    if (data$op == "v_val") {
      # peel off the leading bit
      bits <- substr(data$literal_value, 2, 5) |>
        paste0(collapse = "") |>
        as.character() |>
        strsplit("") |>
        unlist() |>
        as.numeric()
      # manual binary
      powers <- bits |> seq_along() |> rev()
      powers_to_use <- (powers - 1)[bits == 1]
      data$op <- sum(2 ^ powers_to_use)
    }
    data
  }

  chomp_literal_value <- function(x) {
    starts <- seq(1, nchar(x), by = 5)
    leads <- substr(rep(x, length(starts)), starts, starts)
    subgroups <- Position(function(x) x == "0", leads)

    starts <- starts[seq_len(subgroups)]
    ends <- starts + 4
    parts <- substr(rep(x, length(starts)), starts, ends)

  }

  chomp_type_0 <- function(x) {
    length_field <- substr(x, 1, 15)
    length <- strtoi(length_field, 2)

    subpackets <- substr(x, 15 + 1, 15 + length)
    rest <- substr(x, 15 + length + 1, nchar(x))
    packets <- list()
    while (!is_terminated(subpackets)) {
      l <- chomp_one(subpackets)
      packets <- c(packets, list(l))
      subpackets <- l$rest
    }

    list(
      subpackets = packets,
      sub_length = length,
      rest = rest
    )
  }

  chomp_type_1 <- function(x) {
    length_field <- substr(x, 1, 11)
    length <- strtoi(length_field, 2)
    rest <- substr(x, 12, nchar(x))

    l <- as.list(seq_len(length))
    for (i in seq_len(length)) {
      l[[i]] <- chomp_one(rest)
      rest <- l[[i]][["rest"]]
    }

    list(
      subpackets = l,
      sub_length = NA,
      num_sub = length,
      rest = tail(l, 1)[[1]][["rest"]]
    )
  }

  chomp_operator_packet <- function(type_id, x) {
    if (type_id == "0") {
      chomp_type_0(x)
    } else if (type_id == "1") {
      chomp_type_1(x)
    }
  }

  # for when we want to chomp a fixed number of subpackets
  chomp_one <- function(bits) {
    data <- list()
    data$version <- substr(bits, 1, 3)
    data$type <- substr(bits, 4, 6)
    is_literal_value <- data$type == "100"

    if (is_literal_value) {
      data$rest <- substr(bits, 7, nchar(bits))
      data$literal_value <- chomp_literal_value(data$rest)
      data <- set_op(data)
      data$rest <- substr(
        bits,
        7 + sum(nchar(data$literal_value)),
        nchar(bits)
      )
      rest <- data$rest
    }

    if (!is_literal_value) {
      data <- set_op(data)
      data$literal_value <- NA
      length_type_id <- substr(bits, 7, 7)
      data$rest <- substr(bits, 8, nchar(bits))
      results <- chomp_operator_packet(length_type_id, data$rest)
      data$num_sub <- results$num_sub
      data$subpackets <- results$subpackets
      data$rest <- results$rest
    }

    data
  }

  # recursive function that consumes a whole string
  chomp <- function(bits, history = list()) {
    data <- list()
    data$version <- substr(bits, 1, 3)
    data$type <- substr(bits, 4, 6)
    is_literal_value <- data$type == "100"

    if (is_literal_value) {
      data$rest <- substr(bits, 7, nchar(bits))
      data$literal_value <- chomp_literal_value(data$rest)
      data <- set_op(data)
      data$rest <- substr(
        bits,
        7 + sum(nchar(data$literal_value)),
        nchar(bits)
      )
      rest <- data$rest
    }

    if (!is_literal_value) {
      data <- set_op(data)
      data$literal_value <- NA
      length_type_id <- substr(bits, 7, 7)
      data$rest <- substr(bits, 8, nchar(bits))
      results <- chomp_operator_packet(length_type_id, data$rest)
      data$subpackets <- results$subpackets
      rest <- results$rest
    }

    history <- c(history, list(data))

    if (!is_terminated(rest)) {
      chomp(rest, history)
    } else {
      history
    }
  }

  chomp(x)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day16
#' @export
example_data_16 <- function(example = 1) {
  l <- list(
    a = c(
      "D2FE28",
      "38006F45291200",
      "EE00D40C823060",
      "8A004A801A8002F478",
      "620080001611562C8802118E34",
      "C0015000016115A2E0802F182340",
      "A0016C880162017C3686B18A3D4780"
    ),
    b = c(
      "C200B40A82",
      "04005AC33890",
      "880086C3E88112",
      "CE00C43D881120",
      "D8005AC2A8F0",
      "F600BC2D8F",
      "9C005AC2F8F0",
      "9C0141080250320F1802104A08"
    )
  )
  l[[example]]
}
