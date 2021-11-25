#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' 
#'     DO NOT REMOVE.
#'     
#' @noRd
app_server <- function(Rdata_path){

  server <- function(input, output, session) {
    load(Rdata_path)
    
    steps <- readr::read_tsv(app_sys("app/doc/steps.tsv"))
    observeEvent(input$help, {
      rintrojs::introjs(session,
              options = list(steps=steps[, -1],
                             showBullets = FALSE))})
    
    # # observeEvent(input$instruct, {
    #   output$bs_about <- renderUI({
    #     shinyBS::bsModal(
    #       id = "instruction", title = "Instruction", trigger = "instruct",
    #       size = "large",
    #       h2("test")
    #       # includeMarkdown(docFile("Documentation.md"))
    #     )
    #   })
    # # })
      
      # output$bs_about <- renderUI({
      #   tagList(
      #     shinyBS::bsModal("modal", "foo", trigger = "a", "bar"),
      #     actionButton("a", "Show modal")
      #   )
      # })  
    
    
    output$ui_table <- renderUI({
      shinycssloaders::withSpinner(DT::DTOutput("table_phe"), type = 5)
    })
    
    phe_id <- reactive({
      url_vars <- session$clientData$url_search
      gsub(".*\\?phecode=([\\d\\.]*)$", "\\1", url_vars, perl = TRUE)
    })
    
    inputrow <- reactive({
      phecode$row[match(paste0("PheCode:", phe_id()), phecode$Phecode)]
    })
    
    output$table_phe <- DT::renderDT(
      DT::datatable(icdmap[, c(4, 5, 1:3)],
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
        shinycssloaders::withSpinner(plotly::plotlyOutput("sunburst",
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
        shinycssloaders::withSpinner(collapsibleTree::collapsibleTreeOutput("tree",
                                          width = "100%",
                                          height = height_tree()
        ), type = 5)
      }
    })
    
    
    
    output$tree <- collapsibleTree::renderCollapsibleTree({
      treePlot(nodes_list(), input$maxd_tree)
    })
    
    df_sunb <- reactive({
      dfSunburst(nodes_list())
    })
    
    output$sunburst <- plotly::renderPlotly({
      sunburstPlotly(df_sunb(), input$maxd_sunburst)
    })
    
    
    
    
    output$ui_legend <- renderUI({
      if (is.null(input$table_phe_rows_selected)) {
        textOutput({
          "Select 1 row in the table, Please."
        })
      } else {
        shinycssloaders::withSpinner(plotOutput("out_legend", height = "450px"), type = 5)
      }
    })
    
    output$out_legend <- renderPlot(legends(df_sunb()))
  }

return(server)
}
