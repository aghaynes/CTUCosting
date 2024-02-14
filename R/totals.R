#' Calculate totals
#'
#' pass the output from the relevant functions to this function to calculate the totals.
#'
#' @param workpackages A tibble with the workpackages
#' @param expenses A tibble with the expenses
#' @param discount A tibble with the discount
#' @param overhead A tibble with the overhead
#' @param fte fte cost data
#' @param snf A boolean indicating whether the project is an SNF project
#' @param rate A string indicating the rate (internal, external for-profit, external non-profit)
#' @param dlf A boolean indicating whether the project is a DLF project or not (unused)
#'
#' @importFrom dplyr bind_rows mutate
#' @export
totals <- function(workpackages, expenses, discount, overhead, fte, snf, rate, dlf = FALSE){
  `Cost (CHF)` <- NULL

  print("totals(): FTE:")
  print(fte)
  ftes <- fte$fte

  total <- tibble::tribble(
    ~Description, ~`Cost (CHF)`,
  )

  if(nrow(workpackages) > 0){
    print("totals(): workpackages loop")
    total <- total |>
      bind_rows(
        tibble::tribble(
          ~Description, ~`Cost (CHF)`,
          "Tasks billed by the hour", sum(workpackages$Cost),
        )
      )
  }

  if(!snf){
    print("totals(): snf loop")
    total <- total |>
      bind_rows(
        tibble::tribble(
          ~Description, ~`Cost (CHF)`,
          paste0("Discount due to number of hours (", discount$discount_perc, "%)"), -sum(discount$discount_amount),
          "Internal project management (10%)", overhead$pm,
        )
      )
  }

  if(rate == "External for-profit"){
    print("totals(): not internal loop")

    total <- total |>
      bind_rows(
        tibble::tribble(
          ~Description, ~`Cost (CHF)`,
          "University overhead (10%)", overhead$overhead
        )
      )
  }

  if(ftes){
    print("totals(): fte loop")

    total <- total |>
      bind_rows(
        tibble::tribble(
          ~Description, ~`Cost (CHF)`,
          "FTE costs", sum(fte$costs$cost),
        )
      )
  }

  if(nrow(expenses) > 0){
    print("totals(): expenses loop")
    total <- total |>
      bind_rows(
        tibble::tribble(
          ~Description, ~`Cost (CHF)`,
          "Expenses", sum(expenses$Amount),
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
                   "Total", sum(ceiling(total$`Cost (CHF)`))
                 )) |>
    mutate(`Cost (CHF)` = ceiling(`Cost (CHF)`),
           `Cost (CHF)` = format(`Cost (CHF)`, big.mark = "'", nsmall = 0))

  return(total)

}
