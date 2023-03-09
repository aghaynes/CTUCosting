

#' @importFrom vctrs vec_cast
#' @export
calc_discount <- function(workpackages, costing, discount_db){

  summ_discount <- workpackages  |>
    # remove fixed price units
    # filter(!str_detect(Description, "fee")) |> # this is covered by the work packages
    # or based on work packages?
    filter(!wp %in% c("045.0", "050.0", # DM setup
                      "045.3", "050.3", # DM lock archive
                      "060.3" # Website hosting/domain
                      )) |>
    # summarize remaining packages
    ungroup()  |>
    summarize(Hours = sum(Hours),
              Cost = sum(Cost))  |>

    # if DLF support, subtract appropriate CHF from Cost (no discount on the DLF part)?

    mutate(Service = "CTU",
           Description = "Total",
           discount = as.numeric(as.character(cut(Hours, c(seq(0,1000,100), Inf), seq(0,1000,100)/100))),
           discount = if_else(costing == 1, discount, vec_cast(discount_db, double())),
           discount_amount = Cost * (discount / 100),
           new_amount = Cost - discount_amount)  |>
    select(-costing)

}
