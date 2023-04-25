# run app
#' @export

run_costing_app <- function(opts = list()){
  shiny::shinyAppDir(system.file(package = "CTUCosting",
                          "app"),
                     options = opts)
}

