

#
# tmpdir <- tempdir()
# filename <- "IICT-Offer-CTUservices2023.xlsx"
# file <- system.file("ext", file, package = "CTUCosting")
# file.copy(file, tmpdir)
#
# library(openxlsx)
#
# wb <- loadWorkbook(file.path(tmpdir, filename))
#
# writeData(wb, sheet = "Budget", x = 3300, startRow = 21, startCol = 4)
# saveWorkbook(wb, file.path(tmpdir, filename), overwrite = TRUE)
#
#
# hours <- tabs$hours
# rates <- d$meta_information |> select(starts_with("rate"))
# rates

#' Fill out the SNF IICT budget file, distributing hours over years
#'
#' SNF has a specific budget form for IICT budgets.
#'
#' @param hours A data frame with the hours per year (result from snf_cost_table)
#' @param info A list with the project information
#' @param debug return extra logging information for debugging
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook
#' @importFrom purrr walk
#' @export
snf_iict_yearly_budget <- function(hours, info, debug = FALSE){

  # excel file to use

  tmpdir <- tempdir()
  filename <- "IICT-Offer-CTUservices2023.xlsx"
  file <- system.file("ext", filename, package = "CTUCosting")
  file.copy(file, tmpdir)
  wb <- loadWorkbook(file.path(tmpdir, filename))

  # project info ----
  if(debug) print("SNF budget: study information")
  # name/acronym
  writeData(wb, sheet = "Budget",
            x = paste0(info$studyname, " (", info$acronym, ")"),
            startRow = 3, startCol = 2)
  # contact
  writeData(wb, sheet = "Budget",
            x = info$contact,
            startRow = 4, startCol = 2)
  # type of intervention
  int <- case_when(info$intervention == "Medicinal product" ~ "pharmaceutical",
            info$intervention == "Medical device" ~ "medical device",
            info$intervention == "Other intervention (ClinO chapter 4)" ~ "other (ClinO chapter 4)",
            )
  writeData(wb, sheet = "Budget",
            x = int,
            startRow = 6, startCol = 2)
  # number of participants
  writeData(wb, sheet = "Budget",
            x = info$participants,
            startRow = 7, startCol = 2)
  # number of sites
  writeData(wb, sheet = "Budget",
            x = info$sites,
            startRow = 8, startCol = 2)
  # location
  loc <- case_when(
    info$location == "National" ~ "Switzerland",
    info$location == "Europe (geographically)" ~ "Europe",
    info$location == "Global" ~ "Global",
  )
  writeData(wb, sheet = "Budget",
            x = loc,
            startRow = 9, startCol = 2)
  # durations
  writeData(wb, sheet = "Budget",
            x = paste(info$duration_enrol * 12, "months"),
            startRow = 10, startCol = 2)
  writeData(wb, sheet = "Budget",
            x = paste(info$duration * 12, "months"),
            startRow = 11, startCol = 2)
  # complexity
  writeData(wb, sheet = "Budget",
            x = info$complexity,
            startRow = 12, startCol = 2)

  # # add 0s ----
  # if(debug) print("SNF budget: zeros")
  # time_rows <- c(21, 23, 27, 29, 33, 35, 37, 39, 41, 43, 47, 49, 51,
  #   53, 57, 59, 61, 63, 67, 69, 71, 73, 77, 79, 81, 83,
  #   87, 89, 91, 93, 95, 97, 101, 103, 105, 107, 109, 111)
  # empty_time_rows <- time_rows[!time_rows %in% hours$row]
  # walk(time_rows, function(row){
  #   walk(1:(ncol(hours))-1, function(col, row){
  #     writeData(wb, sheet = "Budget",
  #               x = 0,
  #               startRow = row,
  #               startCol = col + 3)
  #   }, row = row)
  # })

  # add hours ---
  print("SNF budget: hours")
  walk(seq_along(rownames(hours)),
       function(hoursrow){
         excelrow <- hours$row[hoursrow]
         walk(1:(ncol(hours)-1),
              function(hourscol, hoursrow){
                value <- hours[hoursrow, hourscol]
                writeData(wb, sheet = "Budget",
                          x = value,
                          startRow = excelrow,
                          startCol = hourscol + 3)
                writeData(wb, sheet = "Budget",
                          x = 0,
                          startRow = excelrow + 2,
                          startCol = hourscol + 3)
              }, hoursrow = hoursrow)
         })

  # add rates ----
  # if(debug) print("SNF budget: rates")
  # ## CSM and Regulatory affairs (= CSM)
  # walk(c(22, 24, 28, 30), function(row){
  #   writeData(wb, sheet = "Budget",
  #             x = info$rate_csm,
  #             startRow = row,
  #             startCol = 2)
  # })
  # ## data management
  # walk(c(34, 36, 38, 40, 42, 44), function(row){
  #   writeData(wb, sheet = "Budget",
  #             x = info$rate_dm,
  #             startRow = row,
  #             startCol = 2)
  # })
  # ## monitoring
  # walk(c(48, 50, 52, 54, 58, 60, 62, 64), function(row){
  #   writeData(wb, sheet = "Budget",
  #             x = info$rate_mon,
  #             startRow = row,
  #             startCol = 2)
  # })
  # ## QM/QA
  # walk(c(68, 70, 72, 74), function(row){
  #   writeData(wb, sheet = "Budget",
  #             x = info$rate_qm,
  #             startRow = row,
  #             startCol = 2)
  # })
  # ## stats
  # walk(c(88, 90, 92, 94, 96, 98), function(row){
  #   writeData(wb, sheet = "Budget",
  #             x = info$rate_qm,
  #             startRow = row,
  #             startCol = 2)
  # })
  # ## services we dont provide - safety event management, study personnel
  # walk(c(78, 80, 82, 84, 102, 104, 106, 108, 110, 112), function(row){
  #   writeData(wb, sheet = "Budget",
  #             x = 0,
  #             startRow = row,
  #             startCol = 2)
  # })

  # save workbook
  if(debug) print("SNF budget: saving")
  saveWorkbook(wb, file.path(tmpdir, filename), overwrite = TRUE)

  return(file.path(tmpdir, filename))

}


