#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts
#'
#' @export
#' @importFrom shiny shinyApp onStop
#' @importFrom golem with_golem_options

run_app <- function(...) {

  #test  
  onStart <- function() {
    
    source("data-raw/tab_auxiliares.R")
    
    cat("Launching app...\n")
    
    onStop(function() {
      cat("Desconnecting from database...\n")
      RSQLite::dbDisconnect(con)
    })
  }
  
  if (is.null(options()$golem.app.prod)) {
    port <- 4242
  } else {
    port <- NULL
  }
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = list(launch.browser = FALSE, port = port), 
      enableBookmarking = NULL
    ), 
    golem_opts = list(...)
  )
  
}
