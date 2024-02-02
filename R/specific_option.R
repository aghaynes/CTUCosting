#' Given a specific variable, return the label of the option selected
#'
#' @param metadata metadata/data dictionary from redcap
#' @param data dataset from redcap
#' @param var the variable of interest
#'
#' @export
specific_option <- function(metadata, data, var){
  field_name <- val <- lab <- NULL
  reference <- data[[var]]
  singlechoice_opts(metadata |>
                      filter(field_name == var)) |>
    filter(val == reference) |>
    pull(lab)
}
