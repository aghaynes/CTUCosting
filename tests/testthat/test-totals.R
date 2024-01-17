
wp <- tibble::tribble(
  ~wp, ~Hours, ~Cost, ~Units,
  "045.0",  100,  1000, 1,
  "100.0",  100,  1000, 1,
  "100.1",  100,  1000, 1,
  "100.2",  100,  1000, 1,
)

exp <- tibble::tribble(
  ~wp, ~Amount,
  "045.0",  1000,
)

disc <- calc_discount(wp, TRUE, NA, FALSE)
disc2 <- calc_discount(wp, FALSE, 5, FALSE)

fte <- list(fte = FALSE)
fte2 <- list(fte = TRUE,
             costs = tibble::tribble(
               ~desc, ~prop, ~units, ~cost,
               "A", 0.5, 1, 100,
             ))

oh <- overhead(wp)

test_that("internal totals", {
  tot <- totals(wp,
                expenses = exp,
                discount = disc,
                overhead = oh,
                internal = TRUE,
                fte = fte)

  expect_equal(nrow(tot), 5)
  expect_equal(unlist(tot[2,2]), "  -90.00", ignore_attr = TRUE)
  expect_equal(unlist(tot[5,2]), "5,310.00", ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot$Description)
  expect_true(!"University overhead (10%)" %in% tot$Description)

  tot2 <- totals(wp,
                 expenses = exp,
                 discount = disc2,
                 overhead = oh,
                 internal = TRUE,
                 fte = fte)
  expect_equal(nrow(tot2), 5)
  expect_equal(unlist(tot2[2,2]), " -150.00", ignore_attr = TRUE)
  expect_equal(unlist(tot2[5,2]), "5,250.00", ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot2$Description)
  expect_true(!"University overhead (10%)" %in% tot2$Description)
})

test_that("external totals", {
  tot3 <- totals(wp,
                expenses = exp,
                discount = disc,
                overhead = oh,
                internal = FALSE,
                fte = fte)
  expect_equal(nrow(tot3), 6)
  expect_equal(unlist(tot3[2,2]), "  -90.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[6,2]), "5,710.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[5,2]), unlist(tot3[3,2]), ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot3$Description)
  expect_true("University overhead (10%)" %in% tot3$Description)


  tot4 <- totals(wp,
                 expenses = exp,
                 discount = disc2,
                 overhead = oh,
                 internal = FALSE,
                 fte = fte)
  expect_equal(nrow(tot4), 6)
  expect_equal(unlist(tot4[2,2]), " -150.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[6,2]), "5,650.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[5,2]), unlist(tot4[3,2]), ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot4$Description)
  expect_true("University overhead (10%)" %in% tot4$Description)


})

test_that("fte totals", {
  tot3 <- totals(wp,
                expenses = exp,
                discount = disc,
                overhead = oh,
                internal = FALSE,
                fte = fte2)
  expect_equal(nrow(tot3), 7)
  expect_equal(unlist(tot3[2,2]), "  -90.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[7,2]), "5,810.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[5,2]), unlist(tot3[3,2]), ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot3$Description)
  expect_true("University overhead (10%)" %in% tot3$Description)


  tot4 <- totals(wp,
                 expenses = exp,
                 discount = disc2,
                 overhead = oh,
                 internal = TRUE,
                 fte = fte2)
  expect_equal(nrow(tot4), 6)
  expect_equal(unlist(tot4[2,2]), " -150.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[6,2]), "5,350.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[5,2]), "  100.00", ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot4$Description)
  expect_true(!"University overhead (10%)" %in% tot4$Description)
  expect_true("FTE costs" %in% tot4$Description)

})
