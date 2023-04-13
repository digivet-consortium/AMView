#' app
#'
#' Generate the Shiny app
#' @param ... additional arguments
#' @return a Shiny app object
#' @export
#' @include server.R
#' @include ui.R
app <- function(...) {
    shinyApp(app_ui(), app_server, ...)
}

#' run_app
#'
#' Run the Shiny app
#' @param ... additional arguments
#' @export
run_app <- function(...) {
    runApp(app(...))
}
