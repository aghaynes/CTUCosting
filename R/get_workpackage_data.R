


#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom dplyr mutate select filter rename left_join row_number if_else group_by summarize
#' @export
# get_workpackage_data <- function(data, metadata){
#   print(dim(data))
#   x <- data %>%
#     mutate(across(.fns = as.character)) %>%
#     select(starts_with("redcap"),
#            matches("(units|desc|hours|wp)_[[:digit:]]{1}")) %>%
#     pivot_longer(cols = matches("(units|desc|hours|wp)_[[:digit:]]{1}")) %>% #View()#names
#
#     # ------------------------
#   # remove miriams test form
#   # select(-ends_with("_v2")) %>%
#   # ------------------------
#
#   separate(name, sep = "_", c("service", "type", "nth")) %>% #View()
#     filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == "") %>% #View()
#     select(-redcap_event_name) %>%
#     mutate(row = row_number()) %>% #View
#     filter(!is.na(value)) %>%
#     pivot_wider(id_cols = c(service, nth, redcap_repeat_instrument, redcap_repeat_instance),
#                 names_from = type,
#                 values_from = value) %>% #View()
#     filter(!is.na(wp))
#   print(x)
#   x %>%
#     mutate(wp = as.numeric(wp),
#            wp = sprintf("%05.1f", wp),
#            units = as.numeric(units),
#            units = if_else(is.na(units), 0, units),
#            hours = as.numeric(hours),
#            hours = if_else(is.na(hours), 0, hours),
#     ) %>%
#     left_join(servicenames) %>%
#     rename(Hours = hours,
#            Units = units,
#            Description = desc) %>%
#     left_join(
#       tibble::tribble(
#         ~service, ~ra, ~rate_name,
#         "docdev", 0, "rate_mon",
#         "dml",    0, "rate_dm",
#         # "dml",    1, "rate_ra",
#         "dmf",    0, "rate_dm",
#         # "dmf",    1, "rate_ra",
#         "sta",    0, "rate_sta",
#         # "sta",    1, "rate_ra",
#         "csm",    0, "rate_csm",
#         # "csm",    1, "rate_ra",
#         "qm",     0, "rate_qm",
#         # "qm",     1, "rate_ra",
#         "ci",     0, "rate_ci",
#         # "ci",     1, "rate_ra",
#         "admin",  0, "rate_admin",
#         "sen",    1, "rate_senior",
#       ),
#       by = c("service")
#     ) %>%
#     left_join(rates_fn(data),
#               "rate_name") %>%
#     left_join(wp_codes(metadata), by = c(wp = "val")) %>%
#     mutate(Cost = Units * Hours * rate) %>%
#     rename(
#       Rate = rate
#     ) %>%
#     filter(Units > 0)
# }

get_workpackage_data <- function(d, meta){
  workpackages <- lapply(d[2:13], get_wp_df) %>% # [5:6]: expected 3 pieces. additional pieces discarded in 29, 89
    bind_rows()

  if(nrow(d$generic) > 0){
    workpackages <- workpackages %>%
      bind_rows(lapply(seq_along(1:nrow(d$generic)),
                       function(x) {
                         # print(x)
                         d$generic[x,] %>% get_generic_df()
                       }) %>%
                  bind_rows())
  }
  workpackages <- workpackages %>%
    left_join(servicenames) %>%
    left_join(ratenames, by = c("service")) %>%
    left_join(rates_fn(d[[1]]),
              "rate_name") %>% #View()
    left_join(wp_codes(meta$metadata), by = c(wp = "val")) %>% #View()#names()
    mutate(Cost = Units * Hours * rate) %>% #View()
    rename(
      Rate = rate
    ) %>%
    filter(Units > 0)

  return(workpackages)
}

#' @export
summarize_by_wp <- function(data){
  data %>%
    group_by(Service, wp, wp_lab) %>%
    summarize(Description = paste(desc, collapse = ", "),
              Hours = sum(Hours),
              Rate = mean(Rate),
              Cost = sum(Cost),
    )
}


#' @export
get_wp_df <- function(d){
  # print(d)
  if(nrow(d) > 0){
    d %>%
      mutate(across(everything(), as.character)) %>%
      # select(-c(record_id, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance),
      #        -matches("date|total_hours|cost|notes|author|complete")) %>%
      select(matches("_[[:digit:]]{1,2}")) %>%
      pivot_longer(matches("(hours|wp|desc|units)_"),
                   names_sep = "_", names_to = c("service", "var", "item")) %>%
      pivot_wider(names_from = var) %>%
      mutate(across(c("hours", "units", "wp"), as.numeric),
             wp = sprintf("%05.1f", wp)) %>%
      rename(Units = units,
             Hours = hours) %>%
      select(service:wp) %>%
      filter(!is.na(desc) & desc != "")
  }
}

#' @export
get_generic_df <- function(d){
  if(nrow(d) > 0){
    # print(d)
    d %>%
      get_wp_df() %>%
      mutate(service = d$gen_div)
  }
}


