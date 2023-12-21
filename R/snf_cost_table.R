#' function to convert from total costs per workpackage to cost per year per workpackage
#' @param workpackages typically the output from selected_workpackages
#' @param proportions  the matrix of proportions of hours per year per workpackage
#' @importFrom dplyr across mutate group_by left_join starts_with
#' @export
snf_cost_table <- function(workpackages, proportions){
  wp <- snf_section <- Hours <- Cost <- NULL

  summ <- workpackages |>
    left_join(snf_division_lkup |>
                mutate(wp = sprintf("%05.1f", wp)), by = "wp") |>
    group_by(snf_section) |>
    collapse::fsummarize(across(c(Hours, Cost), sum))

  # print(summ)

  dat <- proportions |> select(starts_with("Year"))

  # print(proportions)

  cost <- dat * summ$Cost
  hours <- dat * summ$Hours
  nam <- names(dat)

  # print(cost)
  # print(hours)

  tmp <- sapply(seq_along(hours), function(x){
    paste0(sprintf('%1.1f', hours[, x]), " hours <br/> CHF ", cost[, x])
  }) |>
    as.data.frame() |>
    magrittr::set_names(names(hours)) |>
    magrittr::set_rownames(row.names(hours))

  return(tmp)

}

# workpackages <- tibble::tribble(
#   ~wp, ~Hours, ~Cost,
#   "a", 10, 120*10,
#   "a", 20, 120*20,
#   "b", 5, 120*5,
#   "b", 10, 120*10
# )
# proportions <- data.frame(Year1 =     c(.2, .5),
#                           Year2 =     c(.2, .2),
#                           Year3 =     c(.6, .3),
#                           `Row Sum` = c(1,1))
# snf_cost_table(workpackages, proportions)
