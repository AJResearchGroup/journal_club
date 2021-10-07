# Goal: to determine the extent of the OR
#
# Experiment:
#   - determine the difference between log-transformed serum levels for
#     - a candidate at approx 25% percentile
#     - a candidate at approx 75% percentile
#  - determine the effect
#
oestradiol_levels <- tibble::tibble(
  pmol_per_liter = c(
    # 14588 Maximum
    896.25,
    615,
    474.3,
    382,
    313,
    261.9,
    228.1,
    205.9,
    190
    # 175 Minimum
  )
)
# Note that a median hormonal cycle goes from 80 to 950 pg/mL,
# which is beyond this range

oestradiol_levels$log_transformed <- log(oestradiol_levels$pmol_per_liter)

ggplot2::ggplot(oestradiol_levels, ggplot2::aes(log_transformed)) +
  ggplot2::geom_histogram(binwidth = 0.01)

# There are 9 deciles, I will take the 3rd and 6th
delta_log_transformed <- oestradiol_levels$log_transformed[3] -
  oestradiol_levels$log_transformed[6]

# OR is express in 0.01 unit increase in log-transformed serum E2 levels (in pmol/L)
n_deltas <- delta_log_transformed / 0.01

# Values = the likelier you are to get [that phenotype]
# when changing
# from the 3rd to the 6th decile
testthat::expect_equal(1.10 ^ n_deltas, 287.22189)
testthat::expect_equal(1.09 ^ n_deltas, 166.983933)
testthat::expect_equal(1.02 ^ n_deltas, 3.2414898)
testthat::expect_equal(1.01 ^ n_deltas, 1.80566247)

