
wp <- tibble::tribble(
  ~wp, ~Hours, ~Cost,
  "010.1", 10, 120*10,
  "010.2", 20, 120*20,
  "040.1", 5, 120*5,
  "080.1", 10, 120*10
)

d <- create_snf_proportions_table(wp, 3)

test_that("create_snf_proportions_table example works", {
  expect_equal(ncol(d), 4)
  expect_equal(nrow(d), 3)
})
