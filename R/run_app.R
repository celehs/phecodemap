#' Run the Shiny Application
#'
#' @param dict_uqid path to a csv files for dict_uqid.
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import shinyBS
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  dict_uqid = NULL,
  onStart = NULL,
  options = list(), 
  enableBookmarking = "server",
  uiPattern = "/",
  ...
) {
  options(stringsAsFactors = FALSE)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server(dict_uqid),
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}
