test_that("day 16", {
  example_data_16()[4] |>
    f16a_sum_packet_versions() |>
    expect_equal(16)

  example_data_16()[5] |>
    f16a_sum_packet_versions() |>
    expect_equal(12)

  example_data_16()[6] |>
    f16a_sum_packet_versions() |>
    expect_equal(23)

  example_data_16()[7] |>
    f16a_sum_packet_versions() |>
    expect_equal(31)

  example_data_16(2)[1] |>
    f16b_eval_packets() |>
    expect_equal(3)

  example_data_16(2)[2] |>
    f16b_eval_packets() |>
    expect_equal(54)

  example_data_16(2)[3] |>
    f16b_eval_packets() |>
    expect_equal(7)

  example_data_16(2)[4] |>
    f16b_eval_packets() |>
    expect_equal(9)

  example_data_16(2)[5] |>
    f16b_eval_packets() |>
    expect_equal(1)

  example_data_16(2)[6] |>
    f16b_eval_packets() |>
    expect_equal(0)

  example_data_16(2)[7] |>
    f16b_eval_packets() |>
    expect_equal(0)

  example_data_16(2)[8] |>
    f16b_eval_packets() |>
    expect_equal(1)

})
