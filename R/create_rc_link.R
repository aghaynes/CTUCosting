#' create a link to redcap for a specific costing
#'
#' @param record record_id to link to
#' @param costing costing number
#' @param token redcap token
#'
#' @export
create_rc_link <- function(record, costing, token){

  event <- event_ids()[costing]
  # version <- redcap_export_tbl(token,
  #                              "https://redcap.ctu.unibe.ch/api/",
  #                              "version") |>
  #                    names() |>
  #   (function(x)gsub("^X", "", x))()

  paste0("https://redcap.ctu.unibe.ch/redcap_v", redcap_version(),
         "/DataEntry/index.php?pid=1132&id=", record,
         "&event_id=", event, "&page=meta_information")

}


