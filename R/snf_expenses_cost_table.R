#' function to convert from total costs per workpackage to cost per year per workpackage
#' @param expenses typically the output from selected_expenses
#' @param proportions  the matrix of proportions of hours per year per workpackage
#' @importFrom dplyr across mutate group_by left_join starts_with
#' @export
snf_expenses_cost_table <- function(expenses, proportions){

  # print(summ)

  dat <- proportions |> select(starts_with("Year"))

  if(nrow(expenses) > 0){
    # print(proportions)

    cost <- dat * expenses$Amount
    cost <- bind_rows(cost, apply(cost, 2, sum))
    nam <- names(dat)

    # print(cost)
    # print(hours)

    tmp <- sapply(seq_along(cost), function(x){
      paste0("CHF ", cost[, x])
    }) |> as.data.frame()
    print(tmp)
    tmp <- tmp |>
      as.data.frame() |>
      magrittr::set_names(names(cost)) |>
      magrittr::set_rownames(c(expenses$Description, "Total"))
  } else {
    tmp <- proportions[0, -ncol(proportions)]
    cost <- NA
  }
  return(list(shiny = tmp,
              cost = cost))

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
