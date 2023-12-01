





test_that("snf_cost_table calculates correctly", {
  wp <- tibble::tribble(
    ~wp, ~Hours, ~Cost,
    "050.1", 5, 500,
    "050.1", 6, 600,
    "100.1", 2, 200,
    "100.1", 7, 700,
    "080.1", 10, 1000,
  )
  props <- tibble::tribble(
    ~Year1, ~Year2, ~Year3,
    0.5, 0.3, 0.2,
    0.2, 0.3, 0.5,
    0.3, 0.3, 0.4
  )

  tab <- snf_cost_table(wp, props)

  expect_equal(tab$Year1, sprintf("%1.1f hours <br/> CHF %1.0f",
                                  c((5 + 6) * .5, (2 + 7) * .2, 10 * .3),
                                  c((500 + 600) * .5, (200 + 700) * .2, 1000 * .3)))

  expect_equal(tab$Year1[3], tab$Year2[3])

  expect_equal(nrow(tab), 3)
  expect_equal(ncol(tab), 3)

})



