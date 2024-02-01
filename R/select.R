# select variables for specific purposes
# for admin - needs more info to set up PF
#' @importFrom dplyr select
#' @export
select_for_admin <- function(data){
  Service <- wp <- wp_lab <- Hours <- Rate <- Cost <- NULL
  data |>
    select(Service, wp, wp_lab, Hours, Rate, Cost)
}

# for pdf
#' @importFrom dplyr select ungroup
#' @export
select_for_pdf <- function(data){
  Service <- wp_lab <- Description <- Hours <- Cost <- NULL
  data |>
    ungroup() |>
    select(Service, Task = wp_lab, Description, Hours, Cost)
}


# expenses for pdf
#' @importFrom dplyr select ungroup
#' @export
select_expenses_for_pdf <- function(data){
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

# expenses for admin
#' @importFrom dplyr select ungroup
#' @export
select_expenses_for_admin <- function(data){
    out <- data |>
      ungroup() |>
      select(wp_number = exp_pf, wp_lab, author = exp_author, Division, Description, Amount)
  return(out)
}
