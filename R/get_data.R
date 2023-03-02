# get data from redcap

# token <- readLines("O:/tokens/costing.txt")
url <- "https://redcap.ctu.unibe.ch/api/"


#' @importFrom redcaptools redcap_export_tbl redcap_export_byform redcap_export_meta
#' @importFrom glue glue
get_data <- function(record, costing){
  redcap_export_byform(token, url,
                       records = record,
                       events = glue("costing_{costing}_arm_1"))

  # redcap_export_tbl(token, url, "record",
  #                   records = record,
  #                   events = glue("costing_{costing}_arm_1"))
}

get_metadata <- function() redcap_export_meta(token, url)


