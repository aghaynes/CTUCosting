# select variables for specific purposes
# for admin - needs more info to set up PF
#' @importFrom dplyr select
select_for_admin <- function(data){
  Service <- wp <- wp_lab <- Hours <- Rate <- Cost <- NULL
  data |>
    select(Service, wp, wp_lab, Hours, Rate, Cost)
}

# for pdf
#' @importFrom dplyr select ungroup
select_for_pdf <- function(data){
  Service <- wp_lab <- Description <- Hours <- Cost <- NULL
  data |>
    ungroup() |>
    select(Service, Task = wp_lab, Description, Hours, Cost)
}
