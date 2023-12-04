#' calculate overhead from workpackages
#'
#' @param workpackages A tibble with the workpackages
#'
#' @importFrom dplyr ungroup summarize
#' @export
overhead <- function(workpackages){
  Cost <- pm <- NULL

  workpackages |>
    ungroup() |>
    summarize(Cost = sum(Cost),
              pm = Cost * .1,
              overhead = pm)
}
