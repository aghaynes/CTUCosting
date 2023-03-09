#' create a link to redcap for a specific costing
#' @export
create_rc_link <- function(record, costing, token){

  event <- event_ids()[costing]
  version <- redcap_export_tbl(token,
                               "https://redcap.ctu.unibe.ch/api/",
                               "version") |>
                     names() |>
    (function(x)gsub("^X", "", x))()

  glue("https://redcap.ctu.unibe.ch/redcap_v{version}/DataEntry/index.php?pid=1132&id={record}&event_id={event}&page=meta_information")

}


