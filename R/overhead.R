#' calculate overhead from workpackages
#'
#' @param discount the output from the calc_discount function
#'
#' @importFrom dplyr ungroup summarize
#' @export
overhead <- function(discount){
  new_amount <- NULL

  discount |>
    mutate(pm = new_amount * .1,
           overhead = pm)
}
