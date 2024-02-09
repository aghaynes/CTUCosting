skip_on_ci()

# TEST WITH SOME INFO FROM COSTING 7
token <- Sys.getenv("CTUCosting_token")
meta <- get_metadata(token = token)
record <- "TEST"
d <- get_data(record, 7, token)
info <- costing_info(d, meta$metadata)
wp <- get_workpackage_data(d, meta) |> summarize_by_wp()

test_that("variables from select_for_admin", {
  s <- select_for_admin(wp)
  expect_equal(names(s), c("Service", "wp", "wp_lab", "Hours", "Rate", "Cost"))
})
test_that("variables from select_for_pdf", {
  s <- select_for_pdf(wp)
  expect_equal(names(s), c("Service", "Task", "Description", "Hours", "Cost"))
})
test_that("variables from select_for_fte", {
  fte <- d |> get_ftes(meta, TRUE)
  s <- select_fte_for_admin(fte$costs)
  expect_equal(names(s), c("Description", "FTE", "Years", "ApproxCost"))
})

d <- get_data(record, 5, token)
d$expenses |> expenses_prep(meta)
