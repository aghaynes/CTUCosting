# run app
#' @export

run_costing_app <- function(...){
  shiny::shinyAppDir(system.file(package = "CTUCosting",
                          "app"),
                     ...)
}

