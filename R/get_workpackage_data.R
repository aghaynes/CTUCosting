#' get all work package data from redcap data
#'
#' This functions wraps the functions \code{\link{get_wp_df}} and \code{\link{get_generic_df}} to create a harmonised structure.
#'
#' @param d data from redcap
#' @param meta metadata from redcap
#'
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom dplyr mutate select filter rename left_join row_number if_else
#' @importFrom dplyr group_by summarize starts_with everything bind_rows across
#' @importFrom dplyr matches
#' @seealso get_wp_df, get_generic_df
#'
#' @export
get_workpackage_data <- function(d, meta){

  value <- name <- name1 <- hours <- Units <- Hours <- rate <- NULL

  forms <- c("clinical_study_management", "document_development",
             "regulatory_support", "dm_redcap_light", "dm_full_services",
             "dm_reporting", "study_website", "sharefile_cloud",
             "monitoring_onsite_remote", "central_data_monitoring",
             "research_assistant", "statistics", "quality_management",
             "clinical_investigation_unit", "patient_and_public_involvement"
             )

  workpackages <- lapply(d[forms], get_wp_df) |> # [5:6]: expected 3 pieces. additional pieces discarded in 29, 89
    data.table::rbindlist()

  if(nrow(workpackages) == 0){
    workpackages <- data.frame(
      service = character(),
      item = character(),
      desc = character(),
      Units = numeric(),
      Hours = numeric(),
      wp = character(),
      Service = character(),
      div = character(),
      form = character(),
      rate_name = character(),
      Rate = integer(),
      wp_lab = character(),
      Cost = numeric()
    )
  } else {


    if(nrow(d$generic) > 0){
      n <- d$generic |>
        select(starts_with("gen_units"), starts_with("gen_hours")) |>
        pivot_longer(cols = everything(), names_prefix = "gen_",
                     names_sep = "_()", names_to = c("name", "name1")) |>
        summarize(value = sum(value), .by = c(name, name1)) |>
        pivot_wider(names_from = "name", values_from = "value") |>
        mutate(n = units * hours) |>
        pull(n) |>
        any()
      if(n){
        workpackages <- workpackages |>
          bind_rows(lapply(seq_along(1:nrow(d$generic)),
                                       function(x) {
                                         # print(x)
                                         d$generic[x,] |> get_generic_df(meta = meta)
                                       }) |>
                                  data.table::rbindlist())
      }
    }
    workpackages <- workpackages |>
      left_join(servicenames) |>
      left_join(ratenames, by = c("service")) |>
      left_join(rates_fn(d[[1]]),
                "rate_name") |> #str() #View()
      left_join(wp_codes(meta$metadata), by = c(wp = "val")) |>  #View()#names()
      mutate(Cost = Units * Hours * rate) |>  #View()
      rename(
        Rate = rate
      ) |>
      filter(Units > 0) |>
      filter(Hours > 0)
  }

  return(workpackages)
}

#' get workpackage data
#'
#' @param d dataframe
#'
#' @export
get_wp_df <- function(d){

  var <- wp <- hours <- service <- desc <- NULL

  # print(d)
  if(nrow(d) > 0){
    d |>
      mutate(across(everything(), as.character)) |>
      # select(-c(record_id, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance),
      #        -matches("date|total_hours|cost|notes|author|complete")) %>%
      select(matches("_[[:digit:]]{1,2}")) |>
      pivot_longer(matches("(hours|wp|desc|units)_"),
                   names_sep = "_", names_to = c("service", "var", "item")) |>
      pivot_wider(names_from = var) |>
      mutate(across(c("hours", "units", "wp"), as.numeric),
             wp = sprintf("%05.1f", wp)) |>
      rename(Units = units,
             Hours = hours) |>
      select(service:wp) |>
      filter(!is.na(desc) & desc != "")
  }
}

#' extract generic workpackage data
#'
#' @param d dataframe
#' @param meta metadata
#' @export
get_generic_df <- function(d, meta){
  if(nrow(d) > 0){
    # print(d)
    tmp <- d |>
      get_wp_df() |>
      mutate(service = specific_option(meta$metadata, d, "gen_div"),
             service = tolower(service),
             service = if_else(wp == 135.1, "gen", service))
    # if(nrow(tmp) > 0){
    #   tmp |>
    #     mutate(
    #       # service = d$gen_div
    #       rate =
    #       )
    # }
    tmp
  }
}


