
library(shinydashboard)
# token <- readLines("O:/tokens/costing.txt")
dashboardPage(
  dashboardHeader(title = "CTU Costing"),
  dashboardSidebar(
    # sidebarPanel(
      # passwordInput("token", "Load data via API token:", value = token),
      textInput("record_id", "Record to export:", value = "2"),
      textInput("costing", "Costing number:", value = "1"),
      actionButton("go", "Download data"),
      uiOutput("rc_link"),
      hr(),
      div(style = "margin-left: 15px;", tags$u(tags$b("Filter tasks and expenses"))),
      div(style = "margin-left: 15px;", "where necessary (e.g. versions with both full and light DM services were requested"),
      uiOutput("select_workpackages"),
      uiOutput("select_expenses"),
      hr(),
      selectInput("costing_type", "Costing type", c("CTU standard", "SNF")),
      div(style = "margin-left: 15px;", "For SNF projects, select SNF and complete the table that appears on the right"),
      hr(),
      div(style = "margin-left: 15px;", tags$u(tags$b("Generate costing outputs"))),
      textInput("cturep", "Your name"),
      div(style = "margin-left: 15px;", "Your name is included in the PDF instead of a signature"),
      downloadButton("pdf", "Generate PDF"),
      tags$style(type='text/css', "#pdf { margin-left: 15px; margin-bottom: 9px;}"),
      tags$br(),
      downloadButton("admin", "Generate admin info"),
      tags$br(),
      tags$br(),
      tags$style(type='text/css', "#admin { margin-left: 15px; margin-bottom: 9px;}"),
      div(style = "margin-left: 15px;", tags$u(tags$b("PDF customisations"))),
      div(style = "margin-left: 15px;",
          "Longer tables, those that are too long for the page, do not always render correctly.",
          "Breaking them into smaller pieces can remedy this.",
          "If this is required, enter breaks as comma separated values below (e.g. 3,5)"),
      textInput("break_tasks", "Break tasks table in PDF after line(s)",
                placeholder = "enter values separated by commas (e.g. 3,4)"),
      div(style = "margin-left: 15px;",
          "Other page breaks might also be desirable:"),
      checkboxInput("break_totals", "Insert page break before totals section?"),
      checkboxInput("break_notes", "Insert page break before notes section?")

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
      uiOutput("bad_meta"),
      uiOutput("bad_costings"),
      uiOutput("costing")
    ),
  skin = "red"
)



