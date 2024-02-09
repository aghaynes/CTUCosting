
d <- get_testdata_r1()
meta <- get_testdata_meta()
info <- costing_info(d, meta$metadata)

test_that("no error", {
  expect_no_error(info_to_dataframe(info))
})

infodf <- info_to_dataframe(info)

test_that("general info", {
  expect_equal(ncol(infodf), 3)
  expect_equal(names(infodf), c("name", "value", "source"))
  expect_equal(infodf$value[infodf$name == "acronym"], "UNIT-TESTING")
  expect_true(infodf$value[infodf$name == "initcosting"])
  expect_equal(infodf$value[infodf$name == "complexity"], "low")
  expect_true(is.na(infodf$source[infodf$name == "complexity"]))
})

