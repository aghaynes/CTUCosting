#' calculate overhead from workpackages
#'
#' @param workpackages A tibble with the workpackages
#'
#' @importFrom dplyr ungroup summarize
#' @export
overhead <- function(discount){
  new_amount <- NULL

  discount |>
    mutate(pm = new_amount * .1,
           overhead = pm)
}
