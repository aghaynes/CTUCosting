

wp <- tibble::tribble(
  ~wp, ~Hours, ~Cost,
  "045.0",  100,  1000,
  "100.0",  100,  1000,
  "100.1",  100,  1000,
  "100.2",  100,  1000,
)


test_that("overhead calculates 10%", {
  o <- overhead(wp)
  expect_equal(overhead$overhead, sum(wp$Cost) * .1)
})
