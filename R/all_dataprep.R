
#' @importFrom dplyr bind_rows ungroup relocate
#' @importFrom vctrs vec_cast
all_dataprep <- function(record = 1, costing = 1){

  d <- get_data(record = record, costing = costing)
  meta <- get_metadata()

  info <- costing_info(d, meta$metadata, costing)

  # work packages ----
  # wp <- get_workpackage_data(d, d$meta)
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

  summ_workpackages <- summarize_by_wp(workpackages) #%>%
    # filter(Service %in% services_to_keep)

  summ_discount <- summ_workpackages %>%
    ungroup() %>%
    summarize(Hours = sum(Hours),
              Cost = sum(Cost)) %>%
    mutate(Service = "CTU",
           Description = "Total",
           discount = as.numeric(as.character(cut(Hours, c(seq(0,1000,100), Inf), seq(0,1000,100)/100))),
           discount = if_else(costing == 1, discount, vec_cast(info$discount_db, double())),
           discount_amount = Cost * (discount / 100),
           new_amount = Cost - discount_amount) %>%
    select(-costing)

  summ_overhead <- summ_workpackages %>%
    ungroup() %>%
    summarize(Cost = sum(Cost),
              pm = Cost * .1,
              overhead = pm)

  # expenses ----
  expenses <- d$expenses %>% #names
    mutate(wp = sprintf("%05.1f", exp_pf)) %>%
    left_join(wp_codes(meta$metadata), by = c(wp = "val")) %>% #names
    left_join(singlechoice_opts(meta$metadata) %>% #names()
                filter(var == "exp_budget_pos") %>%
                select(val, lab) %>%
                mutate(val = as.numeric(val)),
              by = c(exp_budget_pos = "val")) %>%
    mutate(total_cost = exp_units * exp_cost) %>%
    relocate(division = lab, exp_desc, total_cost, wp_lab) #%>%
    # filter(exp_desc %in% expenses_to_keep)

  # totals ----
  total <- tibble::tribble(
    ~Description, ~`Cost (CHF)`,
    "Work packages", sum(summ_workpackages$Cost),
    "Expenses", sum(expenses$total_cost),
    paste0("Discount (", summ_discount$discount, "%)"), -sum(summ_discount$discount_amount),
    "Internal project management (10%)", summ_overhead$pm,
    "University overhead (10%)", summ_overhead$overhead
  )

  if(info$internal){
    total <- total %>%
      filter(Description != "University overhead")
  }

  total <- rbind(total,
                 tibble::tribble(
                   ~Description, ~`Cost (CHF)`,
                   "Total", sum(total$`Cost (CHF)`)
                 )) |>
    mutate(`Cost (CHF)` = format(`Cost (CHF)`, big.mark = ",", nsmall = 2))

  # workpackages <- workpackages |>
  #   mutate(Cost = format(Cost, big.mark = ",", nsmall = 2))

  out <- info
  out[["workpackages"]] <- summ_workpackages
  out[["summ_discount"]] <- summ_discount
  out[["discount"]] <- sum(summ_discount$discount_amount)
  out[["expenses"]] <- expenses
  out[["total"]] <- total

  return(out)

}

# prepped <- all_dataprep(1,1)
