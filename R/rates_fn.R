
rates_fn <- function(data){
  data |>
    filter(is.na(redcap_repeat_instance)) |>
    select(starts_with("rate_")) |>
    select(!ends_with("note")) |>
    pivot_longer(everything(),
                 names_to = "rate_name",
                 values_to = "rate")
}
