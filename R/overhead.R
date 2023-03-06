
#' @export
overhead <- function(workpackages){
  workpackages %>%
    ungroup() %>%
    summarize(Cost = sum(Cost),
              pm = Cost * .1,
              overhead = pm)
}
