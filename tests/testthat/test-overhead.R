

wp <- tibble::tribble(
  ~wp, ~Hours, ~Cost, ~Units,
  "045.0",  100,  1000, 1,
  "100.0",  100,  1000, 1,
  "100.1",  100,  1000, 1,
  "100.2",  100,  1000, 1,
)

disc <- calc_discount(wp, TRUE, NA, FALSE)

test_that("overhead calculates 10%", {
  o <- overhead(disc)
  expect_equal(o$overhead, 391) # (4000 - (3000 * .03)) * .1
  expect_equal(o$pm, 391) # (4000 - (3000 * .03)) * .1
})
