#' @importFrom dplyr starts_with everything filter select
rates_fn <- function(data){

  redcap_repeat_instance <- NULL

  data |>
    filter(is.na(redcap_repeat_instance)) |>
    select(starts_with("rate_")) |>
    select(!ends_with("note")) |>
    pivot_longer(everything(),
                 names_to = "rate_name",
                 values_to = "rate")
}
