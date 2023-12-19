#' Calculate totals
#'
#' pass the output from the relevant functions to this function to calculate the totals.
#'
#' @param workpackages A tibble with the workpackages
#' @param expenses A tibble with the expenses
#' @param discount A tibble with the discount
#' @param overhead A tibble with the overhead
#' @param internal A boolean indicating whether the project is internal or not
#' @param dlf A boolean indicating whether the project is a DLF project or not (unused)
#'
#' @importFrom dplyr bind_rows mutate
#' @export
totals <- function(workpackages, expenses, discount, overhead, internal, dlf = FALSE){
  `Cost (CHF)` <- NULL
  total <- tibble::tribble(
    ~Description, ~`Cost (CHF)`,
    "Work packages", sum(workpackages$Cost),
    "Expenses", sum(expenses$Amount),
    paste0("Discount due to number of hours (", discount$discount_perc, "%)"), -sum(discount$discount_amount),
    "Internal project management (10%)", overhead$pm,
  )

  if(!internal){
    total <- total |>
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
