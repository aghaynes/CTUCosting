#' run the shiny app
#'
#' @param opts list of options to pass to shiny::shinyAppDir
#'
#' @export

run_costing_app <- function(opts = list()){
  shiny::shinyAppDir(system.file(package = "CTUCosting",
                          "app"),
                     options = opts)
}

