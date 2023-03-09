
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "CTU Costing"),
  dashboardSidebar(
    # sidebarPanel(
      passwordInput("token", "Load data via API token:", value = "4D6B7745D1AC51AE7FBDAFFC466E54F7"),
      textInput("record_id", "Record to export:", value = "2"),
      textInput("costing", "Costing number:", value = "1"),
      uiOutput("rc_link"),
      hr(),
      uiOutput("select_workpackages"),
      uiOutput("select_expenses"),
      hr(),
      textInput("cturep", "Your name"),
      selectInput("costing_type", "Costing type", c("CTU standard", "SNF")),
      downloadButton("pdf", "Generate PDF"),
      downloadButton("admin", "Generate admin info")
    ),
    dashboardBody(
      uiOutput("bad_record"),
      uiOutput("costing")
    ),
  skin = "red"
)



