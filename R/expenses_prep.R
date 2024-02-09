#' prepare expenses data
#' @param data expenses data.frame
#' @param meta metadata list
#' @export
#' @importFrom dplyr mutate left_join relocate

expenses_prep <- function(data, meta){
  data |>
    mutate(wp = sprintf("%05.1f", exp_pf)) |>
    left_join(wp_codes(meta$metadata), by = c(wp = "val")) |> #names
    left_join(redcaptools::singlechoice_opts(meta$metadata) |>  #names()
                filter(var == "exp_budget_pos") |>
                select(val, lab) |>
                mutate(val = as.numeric(val)),
              by = c(exp_budget_pos = "val")) |>
    mutate(total_cost = exp_units * exp_cost) |>
    dplyr::relocate(Division = lab, Description = exp_desc, Amount = total_cost, wp_lab)

  }
