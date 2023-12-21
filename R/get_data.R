

# token <- readLines("O:/tokens/costing.txt")
url <- "https://redcap.ctu.unibe.ch/api/"


#' get data from redcap
#'
#' @param record record_id from redcap
#' @param costing costing number (event number) from redcap
#' @param token API token
#' @importFrom redcaptools redcap_export_tbl redcap_export_byform redcap_export_meta
#' @importFrom glue glue
#' @export
get_data <- function(record, costing, token){
  record_id <- NULL

  d <- redcap_export_byform(token, url,
                       records = record,
                       events = paste0("costing_", costing, "_arm_1"))

  lapply(d, function(x){
    x |> filter(record_id == record)
  })

  # redcap_export_tbl(token, url, "record",
  #                   records = record,
  #                   events = glue("costing_{costing}_arm_1"))
}

#' get metadata from redcap
#'
#' @param token API token
#' @export
get_metadata <- function(token) redcap_export_meta(token, url)


