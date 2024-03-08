#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' 
#'     DO NOT REMOVE.
#'    
#' @importFrom DT %>% 
#' @import dplyr
#' @noRd
app_server <- function(Uniq_id, url_va){
    
  server <- function(input, output, session) {
  # steps <- readr::read_tsv(app_sys("app/doc/steps.tsv"), show_col_types = FALSE)
    steps <- data.table::fread(app_sys("app/doc/steps.tsv"))
    observeEvent(input$help, {
    rintrojs::introjs(session,
            options = list(steps=steps[, -1],
                           showBullets = FALSE))})
  
  # uniq id ====
  url_vars <- reactive({ session$clientData$url_search })
  
  uniq_id <- reactive({
    if(!is.null(Uniq_id)){
      # utils::read.csv(Uniq_id, header = TRUE, colClasses = c("character", "character"))
      getUqid(data.table::fread(Uniq_id))
    }
  })
  
  url_node <- reactive({
    if(grepl('uqid=', url_vars())){
      id = gsub(".+?uqid=(.+)", "\\1", url_vars(), perl = TRUE)
      id = strsplit(id, "&")[[1]]
      if ((id %in% phecode$Phecode)) {
        id
      } else if(!is.null(uniq_id())){
        if((id %in% uniq_id()$uqid)[1]){
          uniq_id()$id[uniq_id()$uqid == id]
        }
      }
    } else if(grepl('phecode=', url_vars())){
      id = gsub(".+?phecode=(.+)", "\\1", url_vars(), perl = TRUE)
      paste0("PheCode:", id)
    }
  })
  
  phe_id <- reactive({
    if(isTruthy(url_node())){
        url_node()
      } else {
        c("PheCode:008")
      }
  })
  
  inputrow <- reactive({
    phecode$row[match(phe_id(), phecode$Phecode)]
  })
  
  # input table ====
  
  output$ui_table <- renderUI({
    shinycssloaders::withSpinner(
      DT::DTOutput("table_phe"), 
      type = 5)
  })
  
  output$table_phe <- DT::renderDT(
    DT::datatable({
      df <- icdmap[, c(4, 5, 1:3, 6)]
      df$Rollup <- as.character(df$Rollup)
      df
      },
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
                # stateSave = TRUE,
                # displayStart = inputrow(),
                displayStart = inputrow() - 1,
                dom = "tp",
                columns = list(
                  list(width = "80px" ),
                  NULL,
                  list(width = "80px" ),
                  list(width = "80px" ),
                  NULL,
                  list(width = "80px" )
                ),
                scrollCollapse = TRUE
              ),
      selection = list(mode = 'single', 
                       selected = inputrow(), 
                       target = 'row')
    ),
    server = TRUE
  )
  
  # observeEvent(inputrow(), {
  #   print(paste("inputrow()", inputrow(), "selectPage", inputrow() %/% 8 + 1))
  #   DT::dataTableProxy("table_phe") %>%
  #     DT::selectRows(inputrow()) %>%
  #     DT::selectPage(inputrow() %/% 8 + 1)
  # })
  
  # got rootid -----------------------
  
  rootid <- reactive({
    s_line <- input$table_phe_rows_selected
    if (is.null(s_line)) {
      s_line <- 1
    }
    gsub("\\..+", "", icdmap$Phecode[s_line], perl = TRUE)
  })
  
  selected_icd <- reactive({
    req(input$table_phe_rows_selected)
    icdmap$ICD_id[input$table_phe_rows_selected]
  })
  
  nodes_list <- reactive({
    print(rootid())
    addClass(rootid(), icdmap, dict_icd, df_highlight)
  })
  
  
  ## sunb ====
  
  output$ui_sunb <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
        "Select 1 row in the table, Please."
    } else {
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("sunburst",
                             width = "100%", 
                             height = "700px"
      ), type = 5)
    }
  })
  
  df_sunb <- reactive({
    print(selected_icd())
    dfSunburst(nodes_list(), selected_icd())
  })
  
  output$sunburst <- plotly::renderPlotly({
    sunburstPlotly(rootid(), df_sunb(), input$maxd_sunburst)
  })
  
  
  ### clicked ====
  dedupe <- function(rexpr, domain = getDefaultReactiveDomain()) {
    force(rexpr)
    x <- reactiveVal()
    observe({
      x(tryCatch(
        {
          list(success = rexpr())
        },
        error = function(e) {
          list(error = e)
        }
      ))
    }, domain = domain)
    
    reactive({
      result <- x()
      if (!is.null(result$success))
        result$success
      else
        stop(result$error)
    })
  }
  
  
  clicked <- reactive({
    if(isTruthy(input$treenode) && length(input$treenode) >= 4 && isTruthy(input$treenode$`4`)){
      input$treenode$`4`
    } else {
      ""
    }
  }) %>% dedupe()
  
  ## tree ====
  observe({
    if(is.null(input$treenode) | (isTruthy(clicked()) & !clicked() %in% "root")){
      
      df_plot_tree <- dfPlot(nodes_list(), selected_icd(), plot = "tree", clicked(), input$maxd_tree)
      
      height_tree <- paste0(sqrt(nrow(df_plot_tree)) * 150, "px")
      
      output$ui_tree <- renderUI({
        if (is.null(input$table_phe_rows_selected)) {
          "Select 1 row in the table, Please."
        } else {
          shinycssloaders::withSpinner(
            collapsibleTree::collapsibleTreeOutput(
              "tree",
              width = "100%",
              height = height_tree
            ), type = 5)
        }
      })
      
      output$tree <- collapsibleTree::renderCollapsibleTree({
        treePlot(df_plot_tree, clicked(), input$maxd_tree)
      })
      
      
      plotly::plotlyProxy("sunburst", session) %>%
          plotly::plotlyProxyInvoke(
              "update",
              list(
                level = clicked()
              )
            )
      
    } 
  })
  
  
  ## legend ====
  output$ui_legend <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
      "Select 1 row in the table, Please."
    } else {
      legends(df_sunb(), icdmap$Phenotype[input$table_phe_rows_selected])
    }
  })
  
  
  output$box_title <- renderUI({
    if(is.null(Uniq_id) || is.null(input$table_phe_rows_selected)){
      "Legend"
    } else {
      center <- paste0("PheCode:", icdmap$Phecode[input$table_phe_rows_selected])
      href <- paste0(url_va, uniq_id()$uqid[uniq_id()$id == center])
      
      htmltools::p(center,
        actionButton("tova",
                     class = "btn-primary active", width = "157px",
                     icon = icon("share"),
                     title = "Link back to CIPHER.",
                     # style = "margin: 0 0 0 20px;",
                     tags$a("View in CIPHER",
                            href = href,
                            target = "_blank")))

    }
  })

  
  # read uqid file ====
  
  getUqid <- function(df){
    df_uqid <- NULL
    for (i in 1:ncol(df)){
      if(sum(!grepl("^[A-Za-z0-9]+$", df[[i]])) == 0){
        df_uqid$uqid <- as.character(df[[i]])
      } else if(sum(!grepl("^[\\w\\:\\.\\-]+$", df[[i]], perl = TRUE)) == 0){
        df_uqid$id <- df[[i]]
        df_uqid$id[grepl("_", df_uqid$id)] <- gsub("Phe_", "PheCode:", df_uqid$id[grepl("_", df_uqid$id)])
        df_uqid$id[grepl("_", df_uqid$id)] <- gsub("_", ".", df_uqid$id[grepl("_", df_uqid$id)])
      }
    }
    as.data.frame(df_uqid)
  }
  
  }
  return(server)
}
