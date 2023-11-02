

token <- Sys.getenv("CTUCosting_token")
record <- "TEST_DLF"
d <- get_data(record, 1, token)

meta <- get_metadata(token = token)
wps <- get_workpackage_data(d, meta)
info <- costing_info(d, meta$metadata)
exp <- d$expenses

test_that("number of tasks", {
  expect_equal(nrow(wps), 6)
})

summ_wps <- summarize_by_wp(wps)

test_that("number of workpackages", {
  expect_equal(nrow(summ_wps), 3)
})
test_that("summarized by work package", {
  expect_equal(summ_wps$Hours, c(43.4, 9, 7))
  expect_equal(summ_wps$Cost, c(5208, 1080, 840))
})

calc_discount(summ_wps,
              info$initcosting,
              info$discount_db,
              snf = info$snf,
              dlf = info$dlf)
calc_discount(summ_wps,
              info$initcosting,
              info$discount_db,
              snf = TRUE,
              dlf = TRUE)
totals(wps, exp,
       calc_discount(summ_wps,
                     info$initcosting,
                     info$discount_db,
                     snf = info$snf,
                     dlf = info$dlf),
       overhead = overhead(wps),
       internal = info$internal,
       dlf = info$dlf)

