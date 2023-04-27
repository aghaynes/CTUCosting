
library(shinydashboard)
token <- readLines("O:/tokens/costing.txt")
dashboardPage(
  dashboardHeader(title = "CTU Costing"),
  dashboardSidebar(
    # sidebarPanel(
      # passwordInput("token", "Load data via API token:", value = token),
      textInput("record_id", "Record to export:", value = "2"),
      textInput("costing", "Costing number:", value = "1"),
      uiOutput("rc_link"),
      hr(),
      uiOutput("select_workpackages"),
      uiOutput("select_expenses"),
      hr(),
      selectInput("costing_type", "Costing type", c("CTU standard", "SNF")),

      hr(),
      "Generate costing outputs",
      textInput("cturep", "Your name"),
      div(style="display:inline-block", downloadButton("pdf", "Generate PDF"), style="align:right"),
      tags$br(),
      downloadButton("admin", "Generate admin info")
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
        .skin-red .main-header .logo {
          background-color: #e4003c;
        }
        .skin-red .main-header .navbar {
          background-color: #e4003c;
        }
        .bg-red {
        background-color: #e4003c !important;
        }
        .skin-red .sidebar a {
        color: #333;
        }
      '))),
      uiOutput("bad_record"),
      uiOutput("costing")
    ),
  skin = "red"
)



