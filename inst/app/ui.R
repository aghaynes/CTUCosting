
library(bslib)

page_navbar(
  theme = bs_theme(
    primary = "#E4003C",
    # # bg = "#fff",
    "navbar-bg" = "#E4003C",
    # ".bslib-value-box .value-box-area" = "font-size: 1.5rem !important"
    preset="bootstrap"
  ),
  tags$head(
    tags$style(HTML("
       .bslib-value-box .value-box-area > :nth-child(2) {
         font-size: 1.5rem !important;
       }

       .bslib-value-box .value-box-grid {
         grid-template-columns: 80px 70% !important ;
       }
       "))

  ),

  # costing information pane
  nav_panel(
    "Create an offer",
    uiOutput("bad_record"),
    uiOutput("bad_meta"),
    uiOutput("bad_costings"),
    uiOutput("costing"),
    ),

  # SNF costings
  nav_panel("SNF costings",
            uiOutput("snf_ui")),

  # instruction pane
  nav_panel(
    "Instructions",
    div(
      tags$h3("Instructions"),
      "We use REDCap API tokens to enhance security of our data.",
      "You can find the API token by clicking 'API' under the applications menu in REDCap.",
      actionButton("toRedcapAPI", HTML("Click here to go to the API page in REDCap"),
                   onclick = paste0("window.open('https://redcap.ctu.unibe.ch/redcap_v",
                                    CTUCosting::redcap_version(),
                                    "/API/project_api.php?pid=1132', '_blank')")),
      tags$br(),
      "Enter the record ID from REDCap in to ", tags$em('Record to export'), " on the left.",
      tags$br(),
      "Within each record, it is possible to add multiple costings, configured as events in REDCap.",
      "Decide which costing number you need and enter this in ", tags$em('Costing number.'),
      "(There is also a link in REDCap that will directly open the shiny app and enter the values for the open costing.)",
      tags$br(),
      "Click ", tags$em('Download data'), "to export the data from REDCap to the shiny app.",
      tags$br(),
      "Once the user interface has loaded (on the Create a costing tab), it is possible to remove specific tasks or expenses from the costing using the controls in the 'Filter tasks and expenses section'.",
      "This is useful if, for example, two versions of a costing were requested, one with a task and another without it.",
      "It is not necessary to create two costings in REDCap, one is sufficient.",
      tags$br(),
      tags$h4("Compiling the PDF report"),
      "Once the relevant tasks and expenses are selected, produce the PDF costing by clicking ", tags$em('Generate PDF'), ".",
      "Depending on your browser settings, the PDF might open or you might be prompted to save the PDF.",
      tags$br(),
      "Inspect the PDF.",
      tags$br(),
      "Some customisation is possible. If e.g. the table containing the tasks is long, it might be better being split in a different location,",
      " the row number after which to split can be entered in ", tags$em('Break tasks table in PDF'), "as a comma separated string ",
      "(e.g. 4,8 will break the table after the 4th and 8th lines). Only the white rows of the table should be counted.",
      tags$br(),
      "It is also possible to remove specific parts of the general information, or insert page breaks at particular locations.",
      "Set the relevant check boxes as appropriate.",
      tags$br(),
      tags$h4("SNF"),
      "For SNF projects, the SNF require a specific form of budget in an excel file. This can be generated on the",
      tags$em('SNF costings'), "tab at the top of the page. ",
      tags$br(),
      "The first table on the page allows you to distribute the hours across the different tasks, as defined by the SNF",
      "(each row of the table; ProjectFacts work packages have already been assigned to a particular SNF task). ",
      "Double click on the 0s and set them to the proportion of hours that should be assigned to each task in that year (values between 0 and 1).",
      "When the values in a row sum up to 1, the right hand column will turn green.",
      tags$br(),
      "The second table shows the number of hours and the value of that amount of time, as entered in the first table.",
      tags$br(),
      "Expenses can be distributed in the next table, with the total value in the final table.",
      tags$br(),
      "The excel file can then be exported via the ", tags$em('Download SNF budget'), "button.",
      tags$br(),
      "This file should then be opened in Excel so that the formulas can be recalculated.",
      )
  ),


  # nav_panel("Console",
  #           actionButton("updateLog", "Update Log"),
  #           verbatimTextOutput("console")),

  title = "CTU Costing",

  sidebar = sidebar(
    passwordInput("token", "Copy your API token from REDCap and paste it here:",
              placeholder = "copy it from the API application in REDCap"),
    actionButton("toRedcapAPI", HTML("REDCap API page"),
                 onclick = paste0("window.open('https://redcap.ctu.unibe.ch/redcap_v",
                                  CTUCosting::redcap_version(),
                                  "/API/project_api.php?pid=1132', '_blank')")),
    textInput("record_id", "Record to export:", value = "2"),
    textInput("costing", "Costing number:", value = "1"),
    actionButton("go", "Download data"),
    uiOutput("rc_link"),
    hr(),
    tags$u(tags$b("Filter tasks and expenses")),
    HTML(
      '<h7 style="font-size:10pt;">where necessary (e.g. versions with both full and light DM services were requested)</h7>',
      '<h7 style="font-size:10pt;">To remove them, click them and press delete.</h7>',
      '<h7 style="font-size:10pt;">To restore them, click in the white area and those missing will pop up and can be selected.</h7>',
    ),
    uiOutput("select_workpackages"),
    uiOutput("select_tasks"),
    uiOutput("select_expenses"),
    checkboxInput("include_fte", "Include FTE positions?", value = TRUE),
    hr(),
    # selectInput("costing_type", "Costing type", c("CTU standard", "SNF")),
    HTML(
      '<h7 style="font-size:10pt;">For SNF projects, go to the SNF costings tab and  complete the tables. </h7>'
    ),
    hr(),
    tags$u(tags$b("Generate costing outputs")),
    # actionButton("pdf", "Generate PDF"),
#
#     HTML('<a href="ub_16pt-cmyk.pdf"> fdsdf </a>'),
#
    downloadButton("pdf", "Generate PDF"),
    downloadButton("admin", "Generate admin info"),
    tags$u(tags$b("PDF customisations")),
    checkboxInput("include_design", "Include detail on study design?", value = TRUE),
    checkboxInput("include_int", "Include detail on interventional type?", value = TRUE),
    checkboxInput("include_participants", "Include details on participants?", value = TRUE),
    div(style = "font-size:10pt;",
        "Longer tables, those that are too long for the page, do not always render correctly.",
        "Breaking them into smaller pieces can remedy this.",
        "If this is required, enter breaks as comma separated values below (e.g. 3,5)"),
    textInput("break_tasks", "Break tasks table in PDF after line(s)",
              placeholder = "e.g. 3,5"),
    div(style = "font-size:10pt;",
        "Other page breaks might also be desirable:"),
    checkboxInput("break_projinfo", "Insert page break before 'general project information' section?",
                  value = TRUE),
    checkboxInput("break_ftes", "Insert page break before FTEs section?"),
    checkboxInput("break_totals", "Insert page break before totals section?"),
    checkboxInput("break_notes", "Insert page break before notes section?"),
    div(style = "font-size:6pt;",
        textOutput("pkgVersion")),

  )

)
