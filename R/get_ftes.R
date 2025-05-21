#' Extract FTE data from downloaded data
#' @param d downloaded data
#' @param meta metadata
#' @param include include FTE data in the costing?
#' @importFrom dplyr join_by
#' @export

get_ftes <- function(d, meta, include = TRUE){

  if(nrow(d$full_time_equivalent) > 0 & include){
    long <- d$full_time_equivalent |>
      mutate(across(everything(), as.character), row = 1:n()) |> #names()
      # select(-c(record_id, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance),
      #        -matches("date|total_hours|cost|notes|author|complete")) %>%
      # select(matches("_[[:digit:]]{1,2}")) |> names()
      select(matches("row|fte_role|(desc|prop|units|cost)_")) |>
      pivot_longer(matches("(desc|prop|units|cost)_"),
                   names_sep = "_", names_to = c("service", "var", "item")) |> #View()
      pivot_wider(names_from = var) |> #View()
      # select(fte_div, fte_) |>
      mutate(across(c(prop, units, cost), as.numeric)) |>
      select(fte_role, desc, prop, units, cost)

    long <- long |>
      filter(!is.na(desc) & desc != "" & !is.na(cost) & cost > 0)

    print("FTEs long:")
    print(long)

    div <- specific_option(meta$metadata, d$full_time_equivalent, "fte_div")
    role <- d$full_time_equivalent$fte_role
    fte <- nrow(long) > 0
  } else {
    fte <- FALSE
    long <- tibble::tribble(
      ~desc, ~prop, ~units, ~cost,
    )
    div <- NA
    role <- NA
  }


  print(paste("FTE:", fte))

  return(
    list(
      fte = fte,
      costs = long,
      div = div,
      role = role
    ))

}

