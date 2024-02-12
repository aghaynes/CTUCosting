

workpackages <- tibble::tribble(
  ~wp, ~Hours, ~Cost,
  "080.1", 10, 120*10,
  "080.1", 20, 120*20,
  "080.2", 5, 120*5,
  "080.2", 10, 120*10
)
proportions <- data.frame(Year1 =     c(.2, .5),
                          Year2 =     c(.2, .2),
                          Year3 =     c(.6, .3),
                          `Row Sum` = c(1, 1))
cost_tab <- snf_cost_table(workpackages, proportions)

test_that("snf_cost_table", {
  expect_equal(length(cost_tab), 3)
  expect_equal(names(cost_tab), c("shiny", "hours", "cost"))
  expect_equal(names(cost_tab), c("shiny", "hours", "cost"))
  expect_equal(nrow(cost_tab$hours), nrow(cost_tab$cost))
  expect_equal(nrow(cost_tab$shiny), nrow(cost_tab$cost))
  expect_equal(sum(cost_tab$cost[,1:3]), sum(workpackages$Cost))
  expect_equal(sum(cost_tab$hours[,1:3]), sum(workpackages$Hours))
})
