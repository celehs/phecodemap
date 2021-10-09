

library(collapsibleTree)
library(dplyr)
library(DT)
library(htmltools)
library(plotly)
library(RColorBrewer)
library(readr)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)


source("utils.R")
load("../data/phecodemap.RData")


ui <- dashboardPage(
  dashboardHeader(title = "PheCode Map with ICD",
                  leftUi = tagList(
                    includeCSS("www/style.css"),
                    downloadButton("instruct", " About",
                                   icon = icon("book"),
                                   class="btn btn-primary header-button",
                                   width = "100px",
                                   style = "padding: 6px 20px 6px 20px;"),
                    actionButton("help", " Help",
                                 icon = icon("question"),
                                 class="btn btn-primary header-button",
                                 width = "100px",
                                 style = "padding: 6px 20px 6px 20px;")
                  ),
                  titleWidth = "200pt"
  ),
  dashboardSidebar(width = "0px", minified = FALSE),
  dashboardBody(
    introjsUI(),
    tags$head(
      tags$style(HTML("
      .collapsibleTree .node circle {
        stroke-opacity: 0;
      }
      "))
    ),
    box(
      width = 8, id = "box_table",
      title = "PheCode Mapping with ICD-9 and ICD-10-cm Codes",
      status = "primary",
      align = "center", collapsible = TRUE,
      # collapsed = TRUE,
      uiOutput("ui_table")
    ),
    box(
      width = 4, id = "box_legend",
      title = "Legend", status = "info",
      align = "center", collapsible = TRUE,
      # collapsed = TRUE,
      uiOutput("ui_legend")
    ),

    #  sunburst   --------------------

    box(
      width = 6, id = "box_sunburst",
      title = "Sunburst", status = "warning",
      align = "center", collapsible = TRUE,
      dropdownMenu = boxDropdown(
        id = "box_dropdown_sunb",
        div(
          radioButtons("maxd_sunburst", "Max Depth of Sunburst:",
            choices = 3:9, selected = 7
          ),
          align = "center"
        ),
        icon = icon("cog")
      ),
      uiOutput("ui_sunb")
    ),
    #  tree   --------------------

    box(
      width = 6, id = "box_tree",
      align = "center",
      title = "Tree", status = "success",
      collapsible = TRUE, # closable = TRUE,
      dropdownMenu = boxDropdown(
        id = "box_dropdown_tree",
        div(
          radioButtons("maxd_tree", "Max Depth of Tree:",
            choices = 3:9, selected = 5
          ),
          align = "center"
        ),
        icon = icon("cog")
      ),
      uiOutput("ui_tree")
    ),

    bsModal(
      id = "instruction", title = "Instruction", trigger = "instruct",
      size = "large",
      includeMarkdown("doc/Documentation.md")
    )

  )
)





server <- function(input, output, session) {
  
  steps <- read_tsv("doc/steps.tsv")
  observeEvent(input$help, {
    introjs(session,
            options = list(steps=steps[, -1],
                           showBullets = FALSE))})


  output$ui_table <- renderUI({
    withSpinner(DTOutput("table_phe"), type = 5)
  })

  phe_id <- reactive({
    url_vars <- session$clientData$url_search
    gsub(".*\\?phecode=([\\d\\.]*)$", "\\1", url_vars, perl = TRUE)
  })

  inputrow <- reactive({
    phecode$row[match(paste0("PheCode:", phe_id()), phecode$Phecode)]
  })

  output$table_phe <- renderDT(
    datatable(icdmap[, c(4, 5, 1:3)],
      extensions = "Scroller",
      colnames = c(
        "ICD Description" = "ICD_str",
        "ICD code" = "ICD_id",
        "ICD version" = "ICD_version"
      ),
      rownames = FALSE,
      width = "100%",
      filter = "top",
      options = list(
        deferRender = TRUE,
        pageLength = 8,
        dom = "tp",
        columns = list(
          list(width = "80px" ),
          NULL,
          list(width = "80px" ),
          list(width = "80px" ),
          NULL
        ),
        scrollCollapse = TRUE
      ),
      selection = list(mode = "single",
                       selected = ifelse(is.na(inputrow()), 1, inputrow()))
    ),
    server = TRUE
  )




  # got rootid -----------------------

  rootid <- reactive({
    s_line <- input$table_phe_rows_selected
    if (is.null(s_line)) {
      s_line <- 1
    }
    gsub("\\..+", "", icdmap$Phecode[s_line], perl = TRUE)
  })


  # render plot -----------------------

  nodes_list <- reactive({
    addClass(rootid(), icdmap, df_highlight)
  })


  height_tree <- reactive({
    node <- nodes_list()[[1]]
    filtered_node <- node[sapply(node$ids, filterNode, input$maxd_tree), ]
    paste0(sqrt(nrow(filtered_node)) * 150, "px")
  })



  output$ui_sunb <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
      textOutput({
        "Select 1 row in the table, Please."
      })
    } else {
      withSpinner(plotlyOutput("sunburst",
        width = "100%", height = "700px"
      ), type = 5)
    }
  })


  output$ui_tree <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
      textOutput({
        "Select 1 row in the table, Please."
      })
    } else {
      withSpinner(collapsibleTreeOutput("tree",
        width = "100%",
        height = height_tree()
      ), type = 5)
    }
  })



  output$tree <- renderCollapsibleTree({
    treePlot(nodes_list(), input$maxd_tree)
  })

  df_sunb <- reactive({
    dfSunburst(nodes_list())
  })

  output$sunburst <- renderPlotly({
    sunburstPlotly(df_sunb(), input$maxd_sunburst)
  })




  output$ui_legend <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
      textOutput({
        "Select 1 row in the table, Please."
      })
    } else {
      withSpinner(plotOutput("out_legend", height = "450px"), type = 5)
    }
  })

  output$out_legend <- renderPlot(legends(df_sunb()))
}


shinyApp(ui = ui, server = server)
