





test_that("snf_cost_table calculates correctly", {
  wp <- tibble::tribble(
    ~wp, ~Hours, ~Cost,
    1, 5, 500,
    1, 6, 600,
    2, 2, 200,
    2, 7, 700,
  )
  props <- tibble::tribble(
    ~Year1, ~Year2, ~Year3,
    0.5, 0.3, 0.2,
    0.2, 0.3, 0.5,
  )

  tab <- snf_cost_table(wp, props)

  expect_equal(tab$Year1, sprintf("%1.1f hours <br/> CHF %1.0f",
                                  c((5 + 6) * .5, (2 + 7) * .2),
                                  c((500 + 600) * .5, (200 + 700) * .2)))

  expect_equal(nrow(tab), 2)
  expect_equal(ncol(tab), 3)

})



