#' Calculate discount for project
#'
#' @importFrom vctrs vec_cast
#' @param workpackages work package data (dataframe)
#' @param initcosting binary, initial costing for project?
#' @param discount_db previous discount applied to project, from REDCap
#' @param snf binary, is this an SNF project?
#' @param dlf binary, is this a project with DLF support? (unused)
#' @importFrom dplyr ungroup summarize mutate filter
#' @export
calc_discount <- function(workpackages, initcosting, discount_db,
                          snf = FALSE, dlf = FALSE){

  wp <- Hours <- DiscountableHours <- discount <- discount_perc <- discount_amount <-
    Cost <- NULL

  summ_discount <- workpackages  |>
    # remove fixed price units
    # filter(!str_detect(Description, "fee")) |> # this is covered by the work packages
    # or based on work packages?
    mutate(
      all_hours = Hours * Units,
      DiscountableHours = if_else(!wp %in% c("045.0", "050.0", # DM setup
                                             "045.3", "050.3", # DM lock archive
                                             "060.3" # Website hosting/domain
      ), all_hours, 0),
      DiscountableCost = if_else(!wp %in% c("045.0", "050.0", # DM setup
                                            "045.3", "050.3", # DM lock archive
                                            "060.3" # Website hosting/domain
      ), Cost, 0))

  # Discussion with ST, 21.8.2023 - DLF discount no longer available. Discount
  #   through other sources

  # dlf_discount <- 0

  # if DLF support, subtract appropriate CHF from Cost (no discount on the DLF part)
  # if(dlf){
  #   dlf_relevant_workpackages <- c("045.0", "045.1", "045.2", "045.3",
  #                                  "050.1", "050.2", "050.3", "050.4")
  #
  #   dlf_amount <- 3000
  #   summ_discount_dlf <- workpackages |>
  #     filter(wp %in% dlf_relevant_workpackages) |>
  #     # group_by(Service, wp, wp_lab) |>
  #     summarize(DiscountableHours = sum(Hours),
  #               Hours = sum(Hours),
  #               Cost = sum(Cost),
  #               Rate = mean(Rate)) |>
  #     mutate(DiscountableHours = max(DiscountableHours - (dlf_amount / Rate), 0)
  #            )
  #   dlf_discount <- min(dlf_amount, summ_discount_dlf$Cost * 1.1)
  # }

  summ_discount <- summ_discount |>
    # summarize remaining packages
    ungroup()  |>
    summarize(Hours = sum(Hours * Units),
              DiscountableHours = sum(DiscountableHours),
              Cost = sum(Cost),
              DiscountableCost = sum(DiscountableCost)) |>
    mutate(Service = "CTU",
           Description = "Total",
           discount = as.numeric(
             as.character(
               cut(DiscountableHours,
                   c(-Inf, seq(0,1000,100)-1, Inf),
                   c(0, seq(0,1000,100)/100))
               )
             ),
           discount_perc = vec_cast(discount_db, double()))



  # No discount for SNF projects
  if(snf){
    summ_discount <- summ_discount |>
      mutate(discount_perc = 0)
    # dlf_discount <- 0
  }
  # if(dlf){
  #   summ_discount <- summ_discount |>
  #     mutate(dlf = dlf_discount)
  # }

  summ_discount |>
    mutate(discount_amount = DiscountableCost * (discount_perc / 100),
           new_amount = Cost - discount_amount #- dlf
           )

}
