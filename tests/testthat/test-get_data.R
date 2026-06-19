
skip_on_ci()

token <- Sys.getenv("CTUCosting_token")
record <- "TEST"
d <- get_data(record, 1, token)

meta <- get_metadata(token = token)
wps <- get_workpackage_data(d, meta)
info <- costing_info(d, meta$metadata)
exp <- d$expenses

test_that("number of tasks", {
  expect_equal(nrow(wps), 20) # Kim changed to 20 instead of 17. 17/6/2026
})

summ_wps <- summarize_by_wp(wps)
summ_div <- summarize_by_div(wps)

test_that("number of workpackages", {
  expect_equal(nrow(summ_wps), 9) # Kim changed to 9 instead of 7. 17/6/2026
})
test_that("summarized by work package", {
  expect_equal(summ_wps$Hours, c(10, 3, 10, 2, 90, 1, 62, 260, 10)) # Kim added 90, 1 instead of absent. 17/6/2026
  expect_equal(summ_wps$Cost, c(1200, 360, 1200, 240, 10800, 60, 7440, 31200, 1200)) # Kim added 10800, 60 instead of absent. 17/6/2026
})
#
# calc_discount(summ_wps,
#               info$initcosting,
#               info$discount_db,
#               snf = info$snf,
#               dlf = info$dlf)
# calc_discount(summ_wps,
#               info$initcosting,
#               info$discount_db,
#               snf = TRUE,
#               dlf = TRUE)
# totals(wps, exp,
#        calc_discount(summ_wps,
#                      info$initcosting,
#                      info$discount_db,
#                      snf = info$snf,
#                      dlf = info$dlf),
#        overhead = overhead(wps),
#        internal = info$internal,
#        dlf = info$dlf)

