#' Record TEST, costing 1, from REDCap for unit testing purposes
#'
#' The datasets are generated and save via the code in data-raw/generate_test_datasets.R
#'
#' @format A list of data frames, mostly empty
# "d"
get_testdata_r1 <- function() readRDS(system.file("intdata", "record1.rds", package = "CTUCosting"))


#' Record TEST, costing 1, from REDCap for unit testing purposes
#'
#' The datasets are generated and save via the code in data-raw/generate_test_datasets.R
#'
#' @format A list of 5 data frames
# "meta"
get_testdata_meta <- function() readRDS(system.file("intdata", "meta.rds", package = "CTUCosting"))

