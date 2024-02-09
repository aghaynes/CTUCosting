
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


skip_on_ci()

# TEST WITH SOME INFO FROM COSTING 8
token <- Sys.getenv("CTUCosting_token")
meta <- get_metadata(token = token)
record <- "TEST"
d <- get_data(record, 8, token)
info <- costing_info(d, meta$metadata)

test_that("BIHAM signatory is correctly identified (example with umlaut)", {
  expect_equal(info$sign, "Prof. Dr. med. Nicolas Rodondi")
})
test_that("sponsor responsible", {
  expect_equal(info$sponsor_responsible, "Someone FromBIHAM")
})
test_that("ratelabel correctly identified", {
  expect_equal(info$ratelab, "SNF")
})

# TEST WITH SOME INFO FROM COSTING 4
d <- get_data(record, 4, token)
info <- costing_info(d, meta$metadata)

test_that("Allgemeine Innere Med signatory is correctly identified", {
  expect_equal(info$sign, "Prof. Dr. Drahomir Aujesky")
})
test_that("sponsor responsible", {
  expect_equal(info$sponsor_responsible, "Someone FromInsel")
})
test_that("ratelabel correctly identified", {
  expect_equal(info$ratelab, "Internal")
})
test_that("design correctly identified", {
  expect_equal(info$design, "Prospective study (HRO)")
})

