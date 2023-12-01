#' Import the SNF IICT excel template
#'
#' @return
#' @export
#'
#' @examples
iict_import_xl <- function(){
  readxl::read_xlsx(
    system.file("ext/IICT-Offer-CTUservices2023.xlsx", package = "CTUCosting"),
    sheet = 2,
    col_names = FALSE)
}


