
library(CTUCosting)
library(magrittr)
library(shinydashboard)
library(glue)
library(DT)
library(dplyr)
library(shinybusy)
library(shinyalert)
token <- Sys.getenv("CTUCosting_token")

function(input, output){

  # check valid record and costing
  record_ok <- reactive(
    record_costing_exists(record = input$record_id,
                          costing = input$costing,
                          token = token)
  )

  output$bad_record <- renderUI({
    if(!record_ok()){
      # showNotification(ui = "Check record and costing IDs - at least one of them does not exist in REDCap",
      #                  type = "error",
      #                  )
      shinyalert("Oops!", "Please check your record and costing IDs... at least one does not exist in REDCap", type = "error")
      fluidRow(span("Check your record and costing IDs", style="color:red"))
    }
  })

  output$rc_link <- renderUI({
    req(record_ok())
    actionButton("toRedcap", HTML("Click here to go to this <br/>costing in REDCap"),
                 onclick = glue("window.open('{create_rc_link(record = input$record_id,
                                 costing = input$costing,
                                 token = token)}', '_blank')"))
    # tags$a(href = create_rc_link(record = input$record_id,
    #                              costing = input$costing,
    #                              token = token),
    #        "Click here to go to this costing in REDCap", target = "_blank")
  })

  d <- reactive({
    req(record_ok())
    print(paste("RECORD =", input$record_id, "COSTING =", input$costing))
    show_modal_spinner(text = "Downloading data")
    x <- get_data(record = input$record_id, costing = input$costing, token = token)
    remove_modal_spinner()
    return(x)
  })
  meta <- reactive(get_metadata(token = token))
  notes <- reactive(get_notes(d()))

  info <- reactive({
    print(d()$meta_information)
    costing_info(d(), meta()$metadata)
  })


  output$costing <- renderUI({
    fluidPage(
      fluidRow(
        # tags$h4(glue("{info()$acronym} ({info()$study})")),
        # glue("Costing {input$costing}   Rate: {info()$ratelab}   Duration: {info()$duration} years")
        infoBoxOutput("vb_costing"),
        infoBoxOutput("vb_inst"),
        infoBoxOutput("vb_costingtxt"),
        infoBoxOutput("vb_duration"),
        infoBoxOutput("vb_rate"),
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
      ),
      uiOutput("snf_tab")

    )
  })

  output$vb_costing <- renderInfoBox({
    infoBox(glue("{info()$acronym} ({info()$study})"),
            title = "Project",
            icon = icon("signature"),
            color = "red")
  })
  output$vb_inst <- renderInfoBox({
    infoBox(info()$sponsor,
            title = "Institute",
            icon = icon("building-columns"),
            color = "red")
  })
  output$vb_costingtxt <- renderInfoBox({
    infoBox(info()$init_or_amendment_txt,
            title = "Consulting or Project",
            icon = icon(ifelse(info()$initcosting, "ticket", "folder-open")),
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
            subtitle = "years",
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
                          "Enter this value in REDCap",
                          "(from the initial costing)"),
            icon = icon("percent"),
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
                  initcosting = info()$initcosting,
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

  # SNF format
  ## init table
  snf_table <- reactiveValues(data = NULL)
  observe({
    wp <- paste(selected_workpackages()$Service, selected_workpackages()$wp_lab, sep = ": ")
    nrow <- length(wp)
    ncol <- info()$duration + 1 # + 1 for rowsums
    df <- as.data.frame(matrix(rep(0, ncol * nrow), nrow = nrow, ncol = ncol))
    names(df) <- c(paste("Year", 1 : info()$duration), "Row sum")
    rownames(df) <- wp
    # add a column for rowsum?
    snf_table$data <- df
  })
  ## edit table
  observeEvent(input$snf_proportions_cell_edit, {
    info <- input$snf_proportions_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    snf_table$data[i, j] <- isolate(coerceValue(v, snf_table$data[i, j]))
    # update rowsums?
    snf_table$data[, "Row sum"] <- apply(snf_table$data[, 1:info()$duration], 1, sum)
  })

  output$snf_proportions <- renderDataTable(
    snf_table$data |>
      datatable(editable = TRUE) |>
      formatStyle("Row sum",
                  backgroundColor = styleInterval(c(0.999, 1.001),
                                                  c("#fc4c4c", "#60fa48", "#fc4c4c"))),
    editable = TRUE)

  proxy <- dataTableProxy("snf_proportions")

  snf_costs <- reactive({

    print(selected_workpackages())
    summ <- selected_workpackages() |>
      group_by(Service) |>
      summarize(across(c(Hours, Cost), sum))

    # print(summ)
    # print(snf_table$data)
    dat <- snf_table$data[, 1 : info()$duration]

    cost <- dat * summ$Cost
    hours <- dat * summ$Hours
    nam <- names(dat)
    print(nam)
    # print(cost)
    # print(hours)
    tmp <- sapply(seq_along(hours), function(x){
      glue("{hours[, x]} hours <br/> CHF {cost[, x]}")
    }) |>
      as.data.frame() |>
      set_names(names(hours)) |>
      set_rownames(row.names(hours))
    # names(tmp) <- paste("Year", 1 : ncol(tmp))
    print(names(tmp))
    tmp
    # cost
  })

  output$snf_cost <- renderDataTable(snf_costs(), escape = FALSE)

  output$snf_tab <- renderUI({
    if(input$costing_type == "SNF"){
      fluidRow(
        box(title = "Proportion of hours per year",
            "Enter proportion of hours expected for each year and each work package",
            dataTableOutput("snf_proportions"),
            width = 12),
        box(title = "SNF costs",
            dataTableOutput("snf_cost"),
            width = 12)
      )
    }
  })

  # pass snf_cost into pdf function, together with input$costing_type



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
      inputs$workpackages <- selected_workpackages()
      inputs$summ_discount <- discount()
      inputs$discount <- sum(discount()$discount_amount)
      inputs$expenses <- selected_expenses()
      inputs$total <- total_cost()
      inputs$cturep <- input$cturep
      inputs$notes <- concat_notes(notes())
      # print(concat_notes(notes()))

      print(str(inputs))

      show_modal_spinner(text = "Compiling PDF",
                         spin = "pixel")

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

      # print(str(inputs))

      # show_modal_spinner(text = "Compiling file")
      print("xl")

      writexl::write_xlsx(dfs, file)

      # remove_modal_spinner()
    }
  )


}


