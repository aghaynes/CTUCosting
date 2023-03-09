
#' @export
record_costing_exists <- function(record, costing, token){
  ids <- redcap_export_tbl(token, "https://redcap.ctu.unibe.ch/api/", "record", fields = "record_id")
  n <- ids |>
    filter(record_id == record & redcap_event_name == glue("costing_{costing}_arm_1")) |>
    nrow()
  n >= 1
}

