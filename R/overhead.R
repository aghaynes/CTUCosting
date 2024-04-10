#' calculate overhead from workpackages
#'
#' @param discount the output from the calc_discount function
#'
#' @importFrom dplyr ungroup summarize
#' @export
overhead <- function(discount){
  new_amount <- NULL

  if(nrow(discount) > 0){
    out <- discount |>
      mutate(pm = new_amount * .1,
             overhead = pm)
  } else {
    out <- data.frame(
      Hours = 0,
      DiscountableHours = 0,
      Cost = 0,
      DiscountableCost = 0,
      Service = "CTU",
      Description = "Total",
      discount = 0,
      discount_percent = 0,
      discount_amount = 0,
      new_amount = 0,
    )
  }
  return(out)
}
