#' @export
totals <- function(workpackages, expenses, discount, overhead, internal, dlf){

  total <- tibble::tribble(
    ~Description, ~`Cost (CHF)`,
    "Work packages", sum(workpackages$Cost),
    "Expenses", sum(expenses$exp_cost),
    paste0("Discount (", discount$discount, "%)"), -sum(discount$discount_amount),
    "Internal project management (10%)", overhead$pm,
    "University overhead (10%)", overhead$overhead
  )

  if(internal){
    total <- total %>%
      filter(Description != "University overhead")
  }
  if(dlf){
    dlf_relevant_workpackages <- c("045.0", "045.1", "045.2", "045.3",
                                   "050.1", "050.2", "050.3", "050.4")
    dm_sum <- workpackages |>
      filter(wp %in% dlf_relevant_workpackages) |>
      pull(Cost) |> sum()

    dm_discount <- -1 * min(3000, dm_sum * 1.1)
    total <- rbind(total,
                   tibble::tribble(
                     ~Description, ~`Cost (CHF)`,
                     "DLF discount", dm_discount
                   ))

  }

  total <- rbind(total,
                 tibble::tribble(
                   ~Description, ~`Cost (CHF)`,
                   "Total", sum(total$`Cost (CHF)`)
                 )) |>
    mutate(`Cost (CHF)` = format(`Cost (CHF)`, big.mark = ",", nsmall = 2))

  return(total)

}
