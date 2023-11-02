#' @export
totals <- function(workpackages, expenses, discount, overhead, internal, dlf = FALSE){

  total <- tibble::tribble(
    ~Description, ~`Cost (CHF)`,
    "Work packages", sum(workpackages$Cost),
    "Expenses", sum(expenses$exp_cost),
    paste0("Discount due to number of hours (", discount$discount_perc, "%)"), -sum(discount$discount_amount),
    "Internal project management (10%)", overhead$pm,
  )

  if(!internal){
    total <- total %>%
      bind_rows(
        tibble::tribble(
          ~Description, ~`Cost (CHF)`,
          "University overhead (10%)", overhead$overhead
        )
      )
  }

  # if(dlf){
  #   total <- rbind(total,
  #                  tibble::tribble(
  #                    ~Description, ~`Cost (CHF)`,
  #                    "DLF discount", -1 * discount$dlf
  #                  ))
  # }

  total <- rbind(total,
                 tibble::tribble(
                   ~Description, ~`Cost (CHF)`,
                   "Total", sum(total$`Cost (CHF)`)
                 )) |>
    mutate(`Cost (CHF)` = format(`Cost (CHF)`, big.mark = ",", nsmall = 2))

  return(total)

}
