
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

oh <- overhead(disc)

test_that("internal totals", {
  tot <- totals(wp,
                expenses = exp,
                discount = disc,
                overhead = oh,
                internal = TRUE,
                fte = fte)

  expect_equal(nrow(tot), 5)
  expect_equal(unlist(tot[grepl("number of hours", tot$Description),2]),
               "  -90.00", ignore_attr = TRUE)
  expect_equal(unlist(tot[grepl("Total", tot$Description),2]), "5,301.00", ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot$Description)
  expect_true(!"University overhead (10%)" %in% tot$Description)

  tot2 <- totals(wp,
                 expenses = exp,
                 discount = disc2,
                 overhead = oh,
                 internal = TRUE,
                 fte = fte)
  expect_equal(nrow(tot2), 5)
  expect_equal(unlist(tot2[grepl("number of hours", tot$Description),2]), " -150.00", ignore_attr = TRUE)
  expect_equal(unlist(tot2[grepl("Total", tot2$Description),2]), "5,241.00", ignore_attr = TRUE)
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
  expect_equal(unlist(tot3[grepl("number of hours", tot3$Description),2]), "  -90.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[grepl("Total", tot3$Description),2]), "5,692.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[grepl("University overhead", tot3$Description),2]),
               unlist(tot3[grepl("Internal project management", tot3$Description),2]), ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot3$Description)
  expect_true("University overhead (10%)" %in% tot3$Description)


  tot4 <- totals(wp,
                 expenses = exp,
                 discount = disc2,
                 overhead = oh,
                 internal = FALSE,
                 fte = fte)
  expect_equal(nrow(tot4), 6)
  expect_equal(unlist(tot4[grepl("number of hours", tot4$Description),2]), " -150.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[grepl("Total", tot4$Description),2]), "5,632.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[grepl("University overhead", tot4$Description),2]), unlist(tot4[3,2]), ignore_attr = TRUE)
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
  expect_equal(unlist(tot3[grepl("number of hours", tot3$Description),2]), "  -90.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[grepl("Total", tot3$Description),2]), "5,792.00", ignore_attr = TRUE)
  expect_equal(unlist(tot3[grepl("University overhead", tot3$Description),2]),
               unlist(tot3[grepl("Internal project management", tot3$Description),2]), ignore_attr = TRUE)
  expect_equal(unlist(tot3[grepl("FTE", tot3$Description),2]), "  100.00", ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot3$Description)
  expect_true("University overhead (10%)" %in% tot3$Description)


  tot4 <- totals(wp,
                 expenses = exp,
                 discount = disc2,
                 overhead = oh,
                 internal = TRUE,
                 fte = fte2)
  expect_equal(nrow(tot4), 6)
  expect_equal(unlist(tot4[grepl("number of hours", tot4$Description),2]), " -150.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[grepl("Total", tot4$Description),2]), "5,341.00", ignore_attr = TRUE)
  expect_equal(unlist(tot4[grepl("FTE", tot4$Description),2]), "  100.00", ignore_attr = TRUE)
  expect_true("Internal project management (10%)" %in% tot4$Description)
  expect_true(!"University overhead (10%)" %in% tot4$Description)
  expect_true("FTE costs" %in% tot4$Description)

})
