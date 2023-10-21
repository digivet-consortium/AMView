#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
app_ui <- function() {
    options("spinner.type" = 5)
    options("spinner.color" = "#2C3E50")

    amu <- get_amu()
    start_date <- min(amu$DateTransaction, na.rm = TRUE)
    end_date <- max(amu$DateTransaction, na.rm = TRUE)
    species <- sort(unique(amu$AnimalType))
    atc <- get_atc()
    groups <- sort(unique(atc$subgroup_1))

    # Dashboard UI setup
    shiny::navbarPage(
        timeseries_panel(species, groups, start_date, end_date),
        map_panel(species, groups, start_date, end_date),
        about_panel(),

        theme = bslib::bs_theme(bootswatch = "flatly"),
        title = "AMView - Visualize AMU"
    )
}

#' @noRd
timeseries_panel <- function(species, groups, start_date, end_date) {
    shiny::tabPanel(
        shiny::tagList(
            timeseries_output(start_date, end_date),
            timeseries_filters(species, groups)
        ),
        title = "Trends"
    )
}

#' @noRd
map_panel <- function(species, groups, start_date, end_date) {
    shiny::tabPanel(
        shiny::fluidRow(
            shiny::column(
                shiny::wellPanel(
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
                    shiny::br(),
                    shinyscreenshot::screenshotButton(
                        filename = "AMView_map",
                        id = "map",
                        icon = shiny::icon("image"),
                        label = " Save map as PNG"
                    ),
                    shiny::br(), shiny::br(),
                    shiny::downloadButton(
                        outputId = "download_map",
                        label = " Download current map data",
                        icon = shiny::icon("file-csv")
                    ),
                    shiny::br(), shiny::br(),
                    shiny::actionButton(
                        inputId = "help_map",
                        label = "About this page",
                        icon = shiny::icon("circle-info")
                    )
                ),
                width = 3
            ),
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        shiny::uiOutput(outputId = "selected_region"),
                        width = 12
                    )
                ),
                shinycssloaders::withSpinner(
                    leaflet::leafletOutput(
                        outputId = "map", height = "76vh"
                    ), hide.ui = FALSE
                ),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        shiny::sliderInput(
                            inputId = "map_slider",
                            label = NULL,
                            min = start_date,
                            max = end_date,
                            value = c(start_date, end_date),
                            width = "100%"
                        ),
                        width = 10, offset = 1
                    ),
                ),
                width = 5
            ),
            shiny::column(
                shiny::fluidRow(
                    shiny::column(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput(
                                outputId = "pie_species",
                                height = "30vh", width = "95%"
                            )
                        ),
                        width = 12
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput(
                                outputId = "pie_diagnosis",
                                height = "30vh", width = "95%"
                            )
                        ),
                        width = 12
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput(
                                outputId = "pie_medication",
                                height = "30vh", width = "95%"
                            )
                        ),
                        width = 12
                    )
                ),
                width = 4
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
