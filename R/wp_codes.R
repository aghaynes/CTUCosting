#' @importFrom redcaptools singlechoice_opts
#' @export
wp_codes <- function(metadata){
  singlechoice_opts(metadata) %>% #names
    filter(grepl("_wp_", var)) %>%
    select(val, lab) %>%
    unique() %>%
    rename(wp_lab = lab)
}
