skip_on_ci()

# TEST WITH SOME INFO FROM COSTING 7
token <- Sys.getenv("CTUCosting_token")
meta <- get_metadata(token = token)
record <- "TEST"
d <- get_data(record, 7, token)
info <- costing_info(d, meta$metadata)

test_that("no error", {
  expect_no_error(get_ftes(d, meta, TRUE))
})
dat <- get_ftes(d, meta, TRUE)
test_that("Correct output", {
  expect_true(dat$fte)
  expect_equal(dim(dat$costs), c(6, 5))
  expect_equal(dat$div, "Statistics")
  expect_equal(dat$role, "Arnold Rimmer")
  expect_equal(dat$costs$desc[c(1,6)], c("Year 1", "Year 5 (last 6 months)"))
})

