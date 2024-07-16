

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
#' @param expenses A data frame with the expenses per year (result from snf_expenses_cost_table)
#' @param info A list with the project information
#' @param debug return extra logging information for debugging
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook
#' @importFrom purrr walk
#' @importFrom dplyr case_when
#' @export
snf_iict_yearly_budget <- function(hours, expenses, info, debug = FALSE){

  # excel file to use

  tmpdir <- tempdir()
  filename <- "IICT-Offer-CTUservices2024.xlsx"
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
            x = info$sponsor_responsible,
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

  # add expenses ----
  print("SNF budget: expenses")
  # expenses total only!
  print(expenses)
  if(class(expenses) == "data.frame"){
    walk(1:ncol(expenses),
         function(column){
           value <- expenses[nrow(expenses), column]
           writeData(wb, sheet = "Budget",
                     x = value,
                     startRow = 113,
                     startCol = column + 3)
         }
    )
  }
  # save workbook
  if(debug) print("SNF budget: saving")
  saveWorkbook(wb, file.path(tmpdir, filename), overwrite = TRUE)

  return(file.path(tmpdir, filename))

}


