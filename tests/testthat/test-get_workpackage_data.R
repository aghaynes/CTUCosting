d <- get_testdata_r1()
meta <- get_testdata_meta()

wp <- get_workpackage_data(d, meta)
test_that("get_workpackage_data - check variable names", {

  expect_equal(names(wp),
               c("service", "item", "desc", "Units", "Hours", "wp", "Service",
                 "div", "form", "rate_name", "Rate", "wp_lab", "Cost"))

})

test_that("get_workpackage_data - content", {
  # number of tasks
  expect_equal(nrow(wp), 17)
  # involved services
  expect_true(all(c("dml", "sta") %in% wp$service))

})

test_that("get_workpackage_data - rates", {
  expect_true(all(wp$Rate == 120))

  # only expected rates present
  expect_true(all(c("rate_dm", "rate_sta") %in% wp$rate_name))
  expect_true(all(wp$rate_name %in% c("rate_dm", "rate_sta")))

})




