#' app
#'
#' Generate the Shiny app
#' @param ... additional arguments
#' @return a Shiny app object
#' @export
#' @include app_server.R
#' @include app_ui.R
app <- function(...) {
    shiny::shinyApp(app_ui(), app_server, ...)
}

#' run_app
#'
#' Run the Shiny app
#' @param ... additional arguments
#' @export
run_app <- function(...) {
    shiny::runApp(app(...))
}
