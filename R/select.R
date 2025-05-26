# select variables for specific purposes
#' select relevant workpackage variables for admin
#' @param data dataframe
#' @importFrom dplyr select
#' @export
select_for_admin <- function(data){
  Service <- wp <- wp_lab <- Hours <- Rate <- Cost <- NULL
  data |>
    select(Service, wp, wp_lab, Hours, Rate, Cost) |>
    mutate(comment = ifelse(wp == "045.3", "!!Fixed price, not hourly!!", ""))

}

#' select relevant workpackage variables for PDF
#' @param data dataframe
#' @importFrom dplyr select ungroup
#' @export
select_for_pdf <- function(data){
  Service <- wp_lab <- Description <- Hours <- Cost <- NULL
  data |>
    ungroup() |>
    select(Service, Task = wp_lab, Description, Hours, Cost)
}


#' select relevant expenses variables for PDF
#' @param data dataframe
#' @importFrom dplyr select ungroup
#' @export
select_expenses_for_pdf <- function(data){
  Division <- Description <- Amount <- NULL
  if(nrow(data) > 0){
    out <- data |>
      ungroup() |>
      select(Division, Description, Amount) |>
      rename(`Cost (CHF)` = Amount)
  } else {
    out <- NULL
  }
  return(out)
}

#' select relevant expenses variables for admin
#' @param data dataframe
#' @importFrom dplyr select ungroup
#' @export
select_expenses_for_admin <- function(data){
  exp_pf <- wp_lab <- exp_author <- Division <- Description <- Amount <- NULL
  cat(names(data))
  out <- data |>
    ungroup() |>
    select(wp_number = exp_pf, wp_lab, author = exp_author, Division, Description,
           Amount, Units = exp_units, `Cost per unit` = exp_cost)
  return(out)
}

#' select relevant FTE variables for admin
#' @param data dataframe
#' @export
select_fte_for_admin <- function(data){
  desc <- prop <- units <- cost <- NULL
  data |>
    select(Role = fte_role, Description = desc, FTE = prop, Years = units, ApproxCost = cost)
}
