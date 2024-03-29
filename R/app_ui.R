#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      shinydashboardPlus::dashboardHeader(title = "Phecode to ICD map",
                      leftUi = tagList(
                        # includeCSS("www/style.css"),
                        actionButton("instruct", " About",
                                     # icon = icon("book"),
                                     class="btn btn-primary header-button",
                                     width = "100px",
                                     style = "padding: 6px 20px 6px 20px;"),
                        actionButton("help", " Tutorial",
                                     # icon = icon("question"),
                                     class="btn btn-primary header-button",
                                     width = "100px",
                                     style = "padding: 6px 20px 6px 20px;")
                      ),
                      titleWidth = "200pt"
      ),
      shinydashboardPlus::dashboardSidebar(width = "0px", minified = FALSE),
      shinydashboard::dashboardBody(
        rintrojs::introjsUI(),
        includeScript(app_sys("app/www/setHeight.js")),
        includeCSS(app_sys("app/www/style.css")),
        shinydashboardPlus::box(
          width = 8, id = "box_table",
          title = "Phecode Mapping with ICD-9 and ICD-10-cm Codes",
          status = "primary",
          align = "center", # collapsible = TRUE,
          dropdownMenu = shinydashboardPlus::boxDropdown(
            id = "box_table_sunb",
            div(p(HTML('<b>Note:</b>')),
                p(HTML("<b>Rollup:</b>  Whether or not ICDs mapped to this code also map to this code's parents.For example, if rollup is 1, an ICD that maps to the phecode 008.11 will also map to the phecodes 008.1 and
                       008.")),
                style = "padding: 0 10px;width: 320px;"),
            icon = icon("circle-question")
          ),
          uiOutput("ui_table")
        ),
        shinydashboardPlus::box(
          width = 4, id = "box_legend",
          # title = textOutput("box_title"),
          title = uiOutput("box_title"),
          status = "info",
          align = "center", # collapsible = TRUE,
          # uiOutput("toVA"),
          uiOutput("ui_legend")#,
          # uiOutput("legend_btn")
        ),
        
        #  sunburst   --------------------
        
        shinydashboardPlus::box(
          width = 6, id = "box_sunburst",
          title = "Sunburst", status = "warning",
          align = "center", collapsible = TRUE,
          dropdownMenu = shinydashboardPlus::boxDropdown(
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
        
        shinydashboardPlus::box(
          width = 6, id = "box_tree",
          align = "center",
          title = "Tree", status = "success",
          collapsible = TRUE, # closable = TRUE,
          dropdownMenu = shinydashboardPlus::boxDropdown(
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
        
        shinyBS::bsModal(
          id = "instruction", title = "Instruction", trigger = "instruct",
          size = "large",
          # h2("test")
          includeMarkdown(app_sys("app/doc/Documentation.md"))
        )
        
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Phecode to ICD map'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

