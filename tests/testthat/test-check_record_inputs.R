skip_on_ci()

# TEST WITH SOME INFO FROM COSTING 7
token <- Sys.getenv("CTUCosting_token")
meta <- get_metadata(token = token)
record <- "TEST"
d <- get_data(record, 7, token)

test_that("correct response to record existence", {
  expect_true(record_costing_exists("TEST", 7, token))
  expect_true(!record_costing_exists("RIMMER", 1, token))
})

d <- get_data(record, 9, token)
test_that("correct response to empty costing", {
  expect_true(!record_costings_exist(d))
})
