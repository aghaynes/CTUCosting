skip_on_ci()

# TEST WITH SOME INFO FROM COSTING 7
token <- Sys.getenv("CTUCosting_token")
meta <- get_metadata(token = token)
record <- "TEST"
d <- get_data(record, 7, token)
info <- costing_info(d, meta$metadata)
wp <- get_workpackage_data(d, meta) |> summarize_by_wp()
e7 <- expenses_prep(d$expenses, meta)

test_that("variables from select_for_admin", {
  s <- select_for_admin(wp)
  expect_equal(names(s), c("Service", "wp", "wp_lab", "Hours", "Rate", "Cost","comment"))
})
test_that("variables from select_for_pdf", {
  s <- select_for_pdf(wp)
  expect_equal(names(s), c("Service", "Task", "Description", "Hours", "Cost"))
})
test_that("variables from select_for_fte", {
  fte <- d |> get_ftes(meta, TRUE)
  s <- select_fte_for_admin(fte$costs)
  expect_equal(names(s), c("Role", "Description", "FTE", "Years", "ApproxCost"))
})

d <- get_data(record, 5, token)
e <- d$expenses |> expenses_prep(meta)
test_that("variables from select_expenses_for_admin", {
  s <- select_expenses_for_admin(e)
  expect_equal(names(s), c("wp_number", "wp_lab", "author", "Division", "Description", "Amount", "Units", "Cost per unit"))
})
test_that("variables from select_expenses_for_pdf", {
  s <- select_expenses_for_pdf(e)
  expect_equal(names(s), c("Division", "Description", "Cost (CHF)"))
  s7 <- select_expenses_for_pdf(e7)
  expect_null(s7)
})
