##' app
##' Generate the Shiny app
##' @importFrom shiny shinyApp
##' @export
##' @return a Shiny app object
app <- function(...) {
    shinyApp(app_ui(), app_server, ...)
}

##' run_app
##' Run the Shiny app
##' @importFrom shiny runApp
##' @export
run_app <- function(...) {
    runApp(app())
}
