#' Replace NA values with "NA" value
#'
#' This utility function catches NA values inside structural inputs and 
#' replaces them with an "NA".
#'
#' @param x A vector or value to check for NA.
#'
#' @return "NA" if input was NA.
#' @export
#'
#' @examples
#' replace_na(NA)
#' replace_na("Valid String")
replace_na <- function(x){
  if(is.na(x)) x <- "NA"
  return(x)
}
