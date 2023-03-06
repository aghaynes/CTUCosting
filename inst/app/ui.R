
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "CTU Costing"),
  dashboardSidebar(
    # sidebarPanel(
      passwordInput("token", "Load data via API token:", value = "4D6B7745D1AC51AE7FBDAFFC466E54F7"),
      textInput("record_id", "Record to export:", value = "2"),
      textInput("costing", "Costing number:", value = "1"),
      actionButton("go", "Load Data"),
      textInput("cturep", "Your name"),
      hr(),
      # menuItem("", tabName = "dashboard"),
      uiOutput("select_workpackages"),
      uiOutput("select_expenses"),
      downloadButton("pdf", "Generate PDF"),
      downloadButton("admin", "Generate admin info")
    ),
    dashboardBody(
      uiOutput("costing")
    ),
  skin = "red"
)



