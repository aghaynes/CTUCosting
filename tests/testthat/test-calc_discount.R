
# test that calc_discount returns appropriate result
wp <- tibble::tribble(
  ~wp, ~Hours, ~Cost, ~Units,
  "045.0",  100,  1000, 1,
  "100.0",  100,  1000, 1,
  "100.1",  100,  1000, 1,
  "100.2",  100,  1000, 1,
)

wp2 <- tibble::tribble(
  ~wp, ~Hours, ~Cost, ~Units,
  "100.0",  100,  1000, 1,
  "100.1",  100,  1000, 1,
  "100.2",  100,  1000, 1,
)

test_that("calc_discount returns appropriate result for initial", {

  disc <- calc_discount(wp, TRUE, 3, FALSE)
  disc2 <- calc_discount(wp2, TRUE, 3, FALSE)

  expect_equal(disc$discount_perc, 3)
  expect_equal(disc$discount_amount, 3000 * 0.03)
  expect_equal(disc$new_amount, 4000 - 3000 * 0.03)

  expect_equal(disc2$discount_perc, 3)
  expect_equal(disc2$discount_amount, 3000 * 0.03)
  expect_equal(disc2$new_amount, 3000 * 0.97)

})

test_that("calc_discount returns appropriate result for amendment", {

  disc <- calc_discount(wp, FALSE, 5, FALSE)
  disc2 <- calc_discount(wp2, FALSE, 5, FALSE)

  expect_equal(disc$discount_perc, 5)
  expect_equal(disc$discount_amount, 3000 * 0.05)
  expect_equal(disc$new_amount, 4000 - 3000 * 0.05)

  expect_equal(disc2$discount_perc, 5)
  expect_equal(disc2$discount_amount, 3000 * 0.05)
  expect_equal(disc2$new_amount, 3000 * 0.95)

})

test_that("calc_discount returns appropriate result for SNF", {

  disc <- calc_discount(wp, FALSE, 5, TRUE)

  expect_equal(disc$discount_perc, 0)
  expect_equal(disc$discount_amount, 0)
  expect_equal(disc$new_amount, 4000)

})






