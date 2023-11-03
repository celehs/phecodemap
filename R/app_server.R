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
      print(paste("inputrow()", inputrow(), "selectPage", inputrow() %/% 8 + 1))
      icdmap[, c(4, 5, 1:3)]},
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
  
  # render plot -----------------------
  
  nodes_list <- reactive({
    print(rootid())
    addClass(rootid(), icdmap, dict_icd, df_highlight)
  })
  
  
  height_tree <- reactive({
    node <- nodes_list()[[1]]
    filtered_node <- node[sapply(
      node$ids, filterNode, input$maxd_tree), ]
    paste0(sqrt(nrow(filtered_node)) * 150, "px")
  })
  
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

  output$ui_tree <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
        "Select 1 row in the table, Please."
    } else {
      shinycssloaders::withSpinner(
        collapsibleTree::collapsibleTreeOutput(
          "tree",
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
    sunburstPlotly(rootid(), df_sunb(), input$maxd_sunburst)
  })

  output$ui_legend <- renderUI({
    if (is.null(input$table_phe_rows_selected)) {
        "Select 1 row in the table, Please."
    } else {
      shinycssloaders::withSpinner(
        plotOutput("out_legend"), 
        type = 5)
    }
  })
  
  output$out_legend <- renderPlot(legends(df_sunb()))
  
  # box title ====
  
  # output$box_title <- renderText({
  #   if(is.null(input$table_phe_rows_selected)){
  #     "Legend"
  #   } else {
  #     paste0("PheCode:", icdmap$Phecode[input$table_phe_rows_selected])
  #   }
  # })
  # 
  # output$legend_btn <- renderUI({
  #   if(is.null(Uniq_id) || is.null(input$table_phe_rows_selected)){
  #     "Legend"
  #   } else {
  #     center <- paste0("PheCode:", icdmap$Phecode[input$table_phe_rows_selected])
  #     href <- paste0(url_va, uniq_id()$uqid[uniq_id()$id == center])
  #     
  #    actionButton("tova",
  #                 class = "btn-primary active", width = "157px",
  #                 icon = icon("share"),
  #                 title = "Link back to CIPHER.",
  #                 style = "margin: 0 0 0 20px;",
  #                 tags$a("View in CIPHER", 
  #                        href = href, 
  #                        target = "_blank"))
  #   }
  #     
  #   
  # })
  
  output$box_title <- renderUI({
    if(is.null(Uniq_id) || is.null(input$table_phe_rows_selected)){
      "Legend"
    } else {
      center <- paste0("PheCode:", icdmap$Phecode[input$table_phe_rows_selected])
      href <- paste0(url_va, uniq_id()$uqid[uniq_id()$id == center])
      # HTML(paste0(center, " ",
      #             "<p><a href=\"", href, "\">View in CIPHER</a></p>"))
      desc_center <- icdmap$Phenotype[match(center, paste0("PheCode:", icdmap$Phecode))]
      htmltools::p(desc_center,
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
  
  # btn back to VA ====
  
  # observeEvent(input$table_phe_rows_selected, {
  #     uqid <- uniq_id()$uqid[uniq_id()$id == paste0("PheCode:", icdmap$Phecode[input$table_phe_rows_selected])]
  #     print(uqid)
  #     # href <- paste0("https://phenomics-dev.va.ornl.gov/cipher/phenotype-viewer?uqid=", uqid)
  #     href <- paste0(url_va, uqid)
  #     output$toVA <- renderUI({
  #       
  #       actionButton("tova",
  #                    class = "btn-primary active", width = "157px",
  #                    icon = icon("share"),
  #                    title = "Link back to CIPHER.",
  #                    style = "margin: 8px 10px 0px 0px;",
  #                    tags$a("View in CIPHER", 
  #                           href = href, 
  #                           target = "_blank"))
  #   })
  # })
  
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
