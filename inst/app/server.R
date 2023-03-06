
library(CTUCosting)
library(magrittr)
library(shinydashboard)
library(glue)
library(DT)
library(dplyr)
library(shinybusy)
token <- "4D6B7745D1AC51AE7FBDAFFC466E54F7"

function(input, output){
  d <- reactive({
    print(paste("RECORD =", input$record_id, "COSTING =", input$costing))
    get_data(record = input$record_id, costing = input$costing, token = input$token)
  })
  meta <- reactive(get_metadata(token = input$token))

  info <- reactive({
    print(d()$meta_information)
    costing_info(d(), meta()$metadata)
  })


  output$costing <- renderUI({
    fluidPage(
      fluidRow(
        tags$h4(glue("{info()$acronym} ({info()$study})")),
        # glue("Costing {input$costing}   Rate: {info()$ratelab}   Duration: {info()$duration} years")
        infoBoxOutput("vb_costing"),
        infoBoxOutput("vb_rate"),
        infoBoxOutput("vb_duration"),
        infoBoxOutput("vb_total"),
        infoBoxOutput("vb_discount")
      ),
      fluidRow(
        box(
          dataTableOutput("dt_workpackages"),
          title = "Work packages",
          width = 12
        )
      ),
      fluidRow(
        box(
          dataTableOutput("dt_expenses"),
          title = "Expenses",
          width = 12
        )
      ),
      fluidRow(
        box(
          dataTableOutput("dt_totals"),
          title = "Total",
          width = 12
        )
      )

    )
  })

  output$vb_costing <- renderInfoBox({
    infoBox(input$costing,
            title = "Costing",
            icon = icon("hashtag"),
            color = "red")
  })
  output$vb_rate <- renderInfoBox({
    infoBox(info()$ratelab,
            title = "Rate", icon =
            icon("money-bill-trend-up"),
            color = "red")
  })
  output$vb_duration <- renderInfoBox({
    infoBox(info()$duration,
            title = "Study duration",
            icon = icon("clock"),
            color = "red")
  })
  output$vb_total <- renderInfoBox({
    infoBox(total_cost()$`Cost (CHF)`[nrow(total_cost())],
            title = "Total cost",
            icon = icon("dollar-sign"),
            color = "red")
  })
  output$vb_discount <- renderInfoBox({
    infoBox(discount()$discount,
            title = "Discount percentage",
            subtitle = ifelse(info()$initcosting,
                          "Enter this value in the database",
                          "(from the initial costing)"),
            icon = icon("clock"),
            color = "red")
  })


  # work packages ----
  wp <- reactive(get_workpackage_data(d(), meta()))

  summ_workpackages <- reactive(summarize_by_wp(wp()))

  output$select_workpackages <- renderUI({
    print(summ_workpackages()$Service)
    selectInput("selected_workpackages",
                label = "Select services for inclusion in the costing",
                choices = unique(summ_workpackages()$Service),
                selected = unique(summ_workpackages()$Service),
                multiple = TRUE
    )
  })

  selected_workpackages <- reactive({
    print(summ_workpackages() |> names())
    summ_workpackages() |>
      dplyr::filter(Service %in% input$selected_workpackages)})

  output$dt_workpackages <- renderDataTable(selected_workpackages(),
                                            rownames = FALSE)

  # expenses ----
  expenses <- reactive({
    # print(d()$expenses)
    d()$expenses %>% #names
      mutate(wp = sprintf("%05.1f", exp_pf)) %>%
      left_join(wp_codes(meta()$metadata), by = c(wp = "val")) %>% #names
      left_join(redcaptools::singlechoice_opts(meta()$metadata) %>% #names()
                  filter(var == "exp_budget_pos") %>%
                  select(val, lab) %>%
                  mutate(val = as.numeric(val)),
                by = c(exp_budget_pos = "val")) %>%
      mutate(total_cost = exp_units * exp_cost) %>%
      relocate(Division = lab, Description = exp_desc, Amount = total_cost, wp_lab) #%>%
    # filter(exp_desc %in% expenses_to_keep)
  })
  output$select_expenses <- renderUI({
    # print(expenses() |> names())
    if(nrow(expenses()) > 0){
      selectInput("selected_expenses",
                  label = "Select expenses for inclusion in the costing",
                  choices = unique(expenses()$Description),
                  selected = unique(expenses()$Description),
                  multiple = TRUE
      )
    }
  })
  selected_expenses <- reactive({
    expenses() |>
      dplyr::filter(Description %in% input$selected_expenses)
  })
  output$dt_expenses <- renderDataTable(
    selected_expenses() |>
      select(Division, Description, Amount, wp_lab),
    rownames = FALSE
  )

  # calculate discount
  discount <- reactive({
    print(paste("Costing: ", info()$initcosting))
    print(paste("discount_db: ", info()$discount_db))
    calc_discount(selected_workpackages(),
                  costing = 1 * info()$initcosting,
                  discount_db = info()$discount_db)})
  output$dt_discount <- renderDataTable({
      print(discount())
      discount()
    },
    rownames = FALSE
  )
  overhead_tab <- reactive(overhead(selected_workpackages()))

  # totals
  total_cost <- reactive({
    totals(workpackages = selected_workpackages(),
           expenses = selected_expenses(),
           discount = discount(),
           overhead = overhead_tab(),
           internal = info()$internal)
  })
  output$dt_totals <- renderDataTable({
    print(total_cost())
    total_cost()
  },
  rownames = FALSE
  )


  # downloads
  ## PDF
  output$pdf <- downloadHandler(
    filename = function(cons_num = info()$consultingnum,
                        studyname = info()$acronym){
      paste0("Costing_", cons_num, "_", studyname, "_", Sys.Date(), ".pdf")
    },
    content = function(file){

      print(info())
      # dot <- reactiveValuesToList(info())

      inputs <- info()
      inputs[["workpackages"]] <- selected_workpackages()
      inputs[["summ_discount"]] <- discount()
      inputs[["discount"]] <- sum(discount()$discount_amount)
      inputs[["expenses"]] <- selected_expenses()
      inputs[["total"]] <- total_cost()
      inputs[["cturep"]] <- input$cturep

      print(str(input))

      show_modal_spinner(text = "Compiling PDF",
                         spin = "folding-cube")

      gen_pdf(
        output = file,
        inputs = inputs
      )

      remove_modal_spinner()
    }
  )

  ## admin
  output$admin <- downloadHandler(
    filename = function(cons_num = info()$consultingnum,
                        studyname = info()$acronym){
      paste0("Costing_", cons_num, "_", studyname, "_", Sys.Date(), ".xlsx")
    },
    content = function(file){

      dfs <- list(
        info = info() |>
          as.data.frame() |>
          mutate(across(everything(), as.character)) |>
          tidyr::pivot_longer(everything())
        , workpackages = selected_workpackages()
        # , discount <- sum(discount()$discount_amount)
        , expenses = selected_expenses()
        , total = total_cost()
      )

      print(str(input))

      show_modal_spinner(text = "Compiling file",
                         spin = "folding-cube")

      writexl::write_xlsx(dfs, file)

      remove_modal_spinner()
    }
  )


}


