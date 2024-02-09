#' create SNF table for entering proportions of hours per year
#' @export
#' @param expenses expenses data
#' @param years number of years

create_snf_expense_proportions_table <- function(expenses, years){

  if(is.na(years)) years <- 5
  years <- ceiling(years)

  nrow <- nrow(expenses)
  ncol <- years + 1

  df <- as.data.frame(matrix(rep(0, ncol * nrow), nrow = nrow, ncol = ncol))
  names(df) <- c(paste("Year", 1 : years), "Row sum")
  rownames(df) <- expenses$Description

  df

}
