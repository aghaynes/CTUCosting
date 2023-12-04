
d <- get_testdata_r1()
meta <- get_testdata_meta()

test_that("costing_info extracts correct information", {
  info <- costing_info(d, meta$metadata)
  expect_equal(info$consultingnum, "xxxx")
  expect_equal(info$studyname, "This is record for unit tests. Please do not delete.")
  expect_equal(info$acronym, "UNIT-TESTING")
  expect_true(info$initcosting)
  expect_true(!info$snf)
  expect_equal(info$ratelab, "Internal")
  head <- clinic_heads |> #head()
    filter(Clinic == info$sponsor) |>
    pull("Head.of.clinic")
  expect_equal(info$sign, head)
})



