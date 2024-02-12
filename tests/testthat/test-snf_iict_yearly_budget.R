d <- get_testdata_r1()
meta <- get_testdata_meta()
wps <- get_workpackage_data(d, meta)

info <- costing_info(d, meta$metadata)

summ_workpackages <- summarize_by_wp(wps)
expenses <- d$expenses |> #names
  expenses_prep(meta)

props <- create_snf_proportions_table(summ_workpackages, info$duration)
props[1, 1:5] <- c(.9, .1, 0, 0, 0)
props[2, 1:5] <- c(.25, .25, .25, .25, 0)
props[3, 1:5] <- c(0, 0, 0, 0, 1)
props[4, 1:5] <- c(.5, .25, .25, 0, 0)
props[5, 1:5] <- c(0, .25, .25, .25, .25)
props[6, 1:5] <- c(.025, .1, .125, .25, .5)

exp_props <- create_snf_expense_proportions_table(expenses, 5)

snf_costs <- snf_cost_table(summ_workpackages, props)
snf_expenses <- snf_expenses_cost_table(expenses, exp_props)

file <- snf_iict_yearly_budget(
  hours = snf_costs$hours,
  expenses = snf_expenses$cost,
  info = info,
  debug = TRUE
)

test_that("output produced", {
  expect_true(file |> file.exists())
})

# TODO: add additional tests to check that the numbers are correct
