#' @export
totals <- function(workpackages, expenses, discount, overhead, internal){

  total <- tibble::tribble(
    ~Description, ~`Cost (CHF)`,
    "Work packages", sum(workpackages$Cost),
    "Expenses", sum(expenses$total_cost),
    paste0("Discount (", discount$discount, "%)"), -sum(discount$discount_amount),
    "Internal project management (10%)", overhead$pm,
    "University overhead (10%)", overhead$overhead
  )

  if(internal){
    total <- total %>%
      filter(Description != "University overhead")
  }

  total <- rbind(total,
                 tibble::tribble(
                   ~Description, ~`Cost (CHF)`,
                   "Total", sum(total$`Cost (CHF)`)
                 )) |>
    mutate(`Cost (CHF)` = format(`Cost (CHF)`, big.mark = ",", nsmall = 2))

  return(total)

}
