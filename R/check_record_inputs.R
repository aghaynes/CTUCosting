
#' @export
record_costing_exists <- function(record, costing, token){
  ids <- redcap_export_tbl(token, "https://redcap.ctu.unibe.ch/api/", "record", fields = "record_id")
  n <- ids |>
    filter(record_id == record & redcap_event_name == glue("costing_{costing}_arm_1")) |>
    nrow()
  n >= 1
}

#' @export
record_meta_enough <- function(datalist){
  md <- datalist$meta_information
  nrow(md) > 0 && apply(md, 1, function(x) sum(!is.na(x) | x != "", na.rm = TRUE)) > 15
}

#' @export
record_costings_exist <- function(datalist){
  sw <- datalist$study_website
  fte <- datalist$full_time_equivalent
  z <- datalist[-which(names(datalist) %in% c("meta_information", "study_website", "full_time_equivalent"))]
  any(sapply(z[1:(length(z) - 1)], nrow) > 0,
    fte$full_time_equivalent_complete > 0,
    sw$study_website_complete > 0)
}

