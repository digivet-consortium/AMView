#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
app_ui <- function() {
    amu <- get_amu()
    start_date <- min(amu$DateTransaction, na.rm = TRUE)
    end_date <- max(amu$DateTransaction, na.rm = TRUE)
    species <- sort(unique(amu$AnimalType))
    atc <- get_atc()
    groups <- sort(unique(atc$subgroup_1))

    strftime_url <-
        "https://svastatichosting.z6.web.core.windows.net/js/strftime-min.js"

    # Dashboard UI setup
    shiny::fluidPage(
        shiny::tags$head(shiny::tags$script(
            src = strftime_url,
            type = "text/javascript"
        )),

        # Header
        shiny::headerPanel(title = "AMView"),

        # Body
        shiny::tabsetPanel(
            timeseries_panel(species, groups, start_date, end_date),
            map_panel(species, groups, start_date, end_date),
            about_panel(),
            id = "tabs"
        ),

        theme = bslib::bs_theme(bootswatch = "flatly")
    )
}

#' @noRd
timeseries_panel <- function(species, groups, start_date, end_date) {
    shiny::tabPanel(
        list(
            timeseries_output(start_date, end_date),
            timeseries_filters(species, groups)
        ),
        title = "Trends"
    )
}

#' @noRd
map_panel <- function(species, groups, start_date, end_date) {
    shiny::tabPanel(
        shiny::br(),
        shiny::sidebarLayout(
            sidebarPanel = shiny::sidebarPanel(
                shiny::h3("Filter"),
                shinyWidgets::pickerInput(
                    inputId = "map_filter_species",
                    label = "Animal types",
                    multiple = TRUE,
                    choices = species,
                    options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE
                    ),
                    inline = FALSE
                ),
                shinyWidgets::pickerInput(
                    inputId = "map_filter_gender",
                    label = "Genders",
                    multiple = TRUE,
                    choices = NULL,
                    options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE
                    ),
                    inline = FALSE
                ),
                shinyWidgets::pickerInput(
                    inputId = "map_filter_age",
                    label = "Age categories",
                    multiple = TRUE,
                    choices = NULL,
                    options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE
                    ),
                    inline = FALSE
                ),
                shinyWidgets::pickerInput(
                    inputId = "map_filter_medication",
                    label = "Medication groups",
                    multiple = TRUE,
                    choices = groups,
                    options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE
                    ),
                    inline = FALSE
                ),
                width = 2
            ),
            mainPanel = shiny::mainPanel(
                shiny::fluidRow(
                    shiny::column(
                        shiny::fluidRow(
                            leaflet::leafletOutput(
                                outputId = "map", height = "75vh", width = "95%"
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                shiny::sliderInput(
                                    inputId = "map_slider",
                                    label = "Date range",
                                    min = start_date,
                                    max = end_date,
                                    value = c(start_date, end_date),
                                    width = "100%"
                                ),
                                width = 10, offset = 1
                            ),
                        ),
                        width = 7
                    ),
                    shiny::column(
                        shiny::div(
                            shiny::uiOutput(outputId = "selected_region")
                        ),
                        shiny::fluidRow(shiny::plotOutput(
                            outputId = "pie_species", height = "25vh"
                        )),
                        shiny::fluidRow(shiny::plotOutput(
                            outputId = "pie_diagnosis", height = "25vh"
                        )),
                        shiny::fluidRow(shiny::plotOutput(
                            outputId = "pie_medication", height = "30vh"
                        )),
                        width = 5
                    )
                ),
                width = 10
            )
        ),
        title = "Map"
    )
}

#' @noRd
about_panel <- function() {
    shiny::tabPanel(
        title = "About"
    )
}
