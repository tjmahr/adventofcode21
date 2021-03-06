% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day16.R
\name{day16}
\alias{day16}
\alias{f16a_sum_packet_versions}
\alias{f16b_eval_packets}
\alias{example_data_16}
\title{Day 16: Packet Decoder}
\usage{
f16a_sum_packet_versions(x)

f16b_eval_packets(x)

example_data_16(example = 1)
}
\arguments{
\item{x}{some data}

\item{example}{Which example data to use (by position or name). Defaults to
1.}
}
\value{
For Part One, \code{f16a_sum_packet_versions(x)} returns the sum of packet
versions. For Part Two, \code{f16b_eval_packets(x)} returns the result of the
evaluating the code in the packets.
}
\description{
\href{https://adventofcode.com/2021/day/16}{Packet Decoder}
}
\details{
\strong{Part One}

As you leave the cave and reach open waters, you receive a transmission
from the Elves back on the ship.

The transmission was sent using the Buoyancy Interchange Transmission
System
([BITS]{title="Just be glad it wasn't sent using the BuoyancY Transmission Encoding System."}),
a method of packing numeric expressions into a binary sequence. Your
submarine\'s computer has saved the transmission in
\href{https://en.wikipedia.org/wiki/Hexadecimal}{hexadecimal} (your puzzle
input).

The first step of decoding the message is to convert the hexadecimal
representation into binary. Each character of hexadecimal corresponds to
four bits of binary data:\preformatted{0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111
}

The BITS transmission contains a single \emph{packet} at its outermost layer
which itself contains many other packets. The hexadecimal representation
of this packet might encode a few extra \code{0} bits at the end; these are
not part of the transmission and should be ignored.

Every packet begins with a standard header: the first three bits encode
the packet \emph{version}, and the next three bits encode the packet \emph{type
ID}. These two values are numbers; all numbers encoded in any packet are
represented as binary with the most significant bit first. For example,
a version encoded as the binary sequence \code{100} represents the number
\code{4}.

Packets with type ID \code{4} represent a \emph{literal value}. Literal value
packets encode a single binary number. To do this, the binary number is
padded with leading zeroes until its length is a multiple of four bits,
and then it is broken into groups of four bits. Each group is prefixed
by a \code{1} bit except the last group, which is prefixed by a \code{0} bit.
These groups of five bits immediately follow the packet header. For
example, the hexadecimal string \code{D2FE28} becomes:\preformatted{110100101111111000101000
VVVTTTAAAAABBBBBCCCCC
}

Below each bit is a label indicating its purpose:
\itemize{
\item The three bits labeled \code{V} (\code{110}) are the packet version, \code{6}.
\item The three bits labeled \code{T} (\code{100}) are the packet type ID, \code{4},
which means the packet is a literal value.
\item The five bits labeled \code{A} (\code{10111}) start with a \code{1} (not the last
group, keep reading) and contain the first four bits of the number,
\code{0111}.
\item The five bits labeled \code{B} (\code{11110}) start with a \code{1} (not the last
group, keep reading) and contain four more bits of the number,
\code{1110}.
\item The five bits labeled \code{C} (\code{00101}) start with a \code{0} (last group,
end of packet) and contain the last four bits of the number, \code{0101}.
\item The three unlabeled \code{0} bits at the end are extra due to the
hexadecimal representation and should be ignored.
}

So, this packet represents a literal value with binary representation
\code{011111100101}, which is \code{2021} in decimal.

Every other type of packet (any packet with a type ID other than \code{4})
represent an \emph{operator} that performs some calculation on one or more
sub-packets contained within. Right now, the specific operations aren\'t
important; focus on parsing the hierarchy of sub-packets.

An operator packet contains one or more packets. To indicate which
subsequent binary data represents its sub-packets, an operator packet
can use one of two modes indicated by the bit immediately after the
packet header; this is called the \emph{length type ID}:
\itemize{
\item If the length type ID is \code{0}, then the next \emph{15} bits are a number
that represents the \emph{total length in bits} of the sub-packets
contained by this packet.
\item If the length type ID is \code{1}, then the next \emph{11} bits are a number
that represents the \emph{number of sub-packets immediately contained} by
this packet.
}

Finally, after the length type ID bit and the 15-bit or 11-bit field,
the sub-packets appear.

For example, here is an operator packet (hexadecimal string
\verb{38006F45291200}) with length type ID \code{0} that contains two sub-packets:\preformatted{00111000000000000110111101000101001010010001001000000000
VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
}
\itemize{
\item The three bits labeled \code{V} (\code{001}) are the packet version, \code{1}.
\item The three bits labeled \code{T} (\code{110}) are the packet type ID, \code{6},
which means the packet is an operator.
\item The bit labeled \code{I} (\code{0}) is the length type ID, which indicates
that the length is a 15-bit number representing the number of bits
in the sub-packets.
\item The 15 bits labeled \code{L} (\code{000000000011011}) contain the length of
the sub-packets in bits, \code{27}.
\item The 11 bits labeled \code{A} contain the first sub-packet, a literal
value representing the number \code{10}.
\item The 16 bits labeled \code{B} contain the second sub-packet, a literal
value representing the number \code{20}.
}

After reading 11 and 16 bits of sub-packet data, the total length
indicated in \code{L} (27) is reached, and so parsing of this packet stops.

As another example, here is an operator packet (hexadecimal string
\code{EE00D40C823060}) with length type ID \code{1} that contains three
sub-packets:\preformatted{11101110000000001101010000001100100000100011000001100000
VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
}
\itemize{
\item The three bits labeled \code{V} (\code{111}) are the packet version, \code{7}.
\item The three bits labeled \code{T} (\code{011}) are the packet type ID, \code{3},
which means the packet is an operator.
\item The bit labeled \code{I} (\code{1}) is the length type ID, which indicates
that the length is a 11-bit number representing the number of
sub-packets.
\item The 11 bits labeled \code{L} (\code{00000000011}) contain the number of
sub-packets, \code{3}.
\item The 11 bits labeled \code{A} contain the first sub-packet, a literal
value representing the number \code{1}.
\item The 11 bits labeled \code{B} contain the second sub-packet, a literal
value representing the number \code{2}.
\item The 11 bits labeled \code{C} contain the third sub-packet, a literal
value representing the number \code{3}.
}

After reading 3 complete sub-packets, the number of sub-packets
indicated in \code{L} (3) is reached, and so parsing of this packet stops.

For now, parse the hierarchy of the packets throughout the transmission
and \emph{add up all of the version numbers}.

Here are a few more examples of hexadecimal-encoded transmissions:
\itemize{
\item \verb{8A004A801A8002F478} represents an operator packet (version 4) which
contains an operator packet (version 1) which contains an operator
packet (version 5) which contains a literal value (version 6); this
packet has a version sum of \code{16}.
\item \verb{620080001611562C8802118E34} represents an operator packet
(version 3) which contains two sub-packets; each sub-packet is an
operator packet that contains two literal values. This packet has a
version sum of \code{12}.
\item \code{C0015000016115A2E0802F182340} has the same structure as the
previous example, but the outermost packet uses a different length
type ID. This packet has a version sum of \code{23}.
\item \code{A0016C880162017C3686B18A3D4780} is an operator packet that contains
an operator packet that contains an operator packet that contains
five literal values; it has a version sum of \code{31}.
}

Decode the structure of your hexadecimal-encoded BITS transmission;
\emph{what do you get if you add up the version numbers in all packets?}

\strong{Part Two}

\strong{Part Two}
Now that you have the structure of your transmission decoded, you can
calculate the value of the expression it represents.

Literal values (type ID \code{4}) represent a single number as described
above. The remaining type IDs are more interesting:
\itemize{
\item Packets with type ID \code{0} are \emph{sum} packets - their value is the sum
of the values of their sub-packets. If they only have a single
sub-packet, their value is the value of the sub-packet.
\item Packets with type ID \code{1} are \emph{product} packets - their value is the
result of multiplying together the values of their sub-packets. If
they only have a single sub-packet, their value is the value of the
sub-packet.
\item Packets with type ID \code{2} are \emph{minimum} packets - their value is the
minimum of the values of their sub-packets.
\item Packets with type ID \code{3} are \emph{maximum} packets - their value is the
maximum of the values of their sub-packets.
\item Packets with type ID \code{5} are \emph{greater than} packets - their value is
\emph{1} if the value of the first sub-packet is greater than the value
of the second sub-packet; otherwise, their value is \emph{0}. These
packets always have exactly two sub-packets.
\item Packets with type ID \code{6} are \emph{less than} packets - their value is
\emph{1} if the value of the first sub-packet is less than the value of
the second sub-packet; otherwise, their value is \emph{0}. These packets
always have exactly two sub-packets.
\item Packets with type ID \code{7} are \emph{equal to} packets - their value is \emph{1}
if the value of the first sub-packet is equal to the value of the
second sub-packet; otherwise, their value is \emph{0}. These packets
always have exactly two sub-packets.
}

Using these rules, you can now work out the value of the outermost
packet in your BITS transmission.

For example:
\itemize{
\item \code{C200B40A82} finds the sum of \code{1} and \code{2}, resulting in the value
\code{3}.
\item \verb{04005AC33890} finds the product of \code{6} and \code{9}, resulting in the
value \code{54}.
\item \verb{880086C3E88112} finds the minimum of \code{7}, \code{8}, and \code{9}, resulting
in the value \code{7}.
\item \code{CE00C43D881120} finds the maximum of \code{7}, \code{8}, and \code{9}, resulting
in the value \code{9}.
\item \code{D8005AC2A8F0} produces \code{1}, because \code{5} is less than \code{15}.
\item \code{F600BC2D8F} produces \code{0}, because \code{5} is not greater than \code{15}.
\item \verb{9C005AC2F8F0} produces \code{0}, because \code{5} is not equal to \code{15}.
\item \verb{9C0141080250320F1802104A08} produces \code{1}, because \code{1} + \code{3} =
\code{2} \* \code{2}.
}

\emph{What do you get if you evaluate the expression represented by your
hexadecimal-encoded BITS transmission?}
}
\examples{
f16a_sum_packet_versions(example_data_16()[1])
f16b_eval_packets(example_data_16()[6])
}
