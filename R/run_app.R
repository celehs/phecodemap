#' Run the Shiny Application
#'
#' @param Uniq_id path to a csv files for dict_uqid.
#' @param url_va url to va for phecode.
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import shinyBS
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  Uniq_id = NULL,
  url_va = NULL,
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
      server = app_server(Uniq_id, url_va),
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}
