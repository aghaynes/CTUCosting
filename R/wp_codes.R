#' Get projectfacts work package codes
#'
#' @param metadata A data frame with columns val, lab. I.e. meta$metadata
#'
#' @importFrom redcaptools singlechoice_opts
#' @export
wp_codes <- function(metadata){
  var <- val <- lab <- NULL
  singlechoice_opts(metadata) |>  #names
    filter(grepl("_wp_", var)) |>
    select(val, lab) |>
    unique() |>
    rename(wp_lab = lab)
}
