test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

wp <- tibble::tribble(
  ~wp, ~Hours, ~Cost,
  "010.1", 10, 120*10,
  "010.2", 20, 120*20,
  "040.1", 5, 120*5,
  "080.1", 10, 120*10
)

create_snf_proportions_table(wp, 3)


