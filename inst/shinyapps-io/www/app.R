#' app
#'
#' Generate the Shiny app
#' @param ... additional arguments
#' @return a Shiny app object
#' @export
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

#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
app_ui <- function() {
    options("spinner.type" = 5)
    options("spinner.color" = "#2C3E50")

    amu <- get_amu()
    start_date <- min(amu$DateTransaction, na.rm = TRUE)
    end_date <- max(amu$DateTransaction, na.rm = TRUE)
    animal_types <- sort(unique(amu$AnimalType))
    atc <- get_atc()
    groups <- sort(unique(atc$subgroup_1))

    # Dashboard UI setup
    shiny::navbarPage(
        map_ui("map", animal_types, groups, start_date, end_date),
        trends_ui("trends", animal_types, groups, start_date, end_date),
        about_ui("about"),

        theme = bslib::bs_theme(bootswatch = "flatly"),
        title = "AMView - Visualize AMU"
    )
}

#' app_server
#'
#' Manage the server (backend) side of the Shiny app
app_server <- function(input, output, session) {
    amu <- get_amu()
    countries <- get_spatial_data(unique(amu$Country))
    amu <- shiny::reactiveVal(amu)
    countries <- shiny::reactiveVal(countries)

    atc <- get_atc()
    val_sub <- shiny::reactiveVal(sort(unique(atc$subgroup_2)))
    atc <- shiny::reactiveVal(atc)

    map_server("map", amu, countries)
    trends_server("trends", amu, countries, atc, val_sub)
    about_server("about")
}
