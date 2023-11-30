#' @noRd
map_ui <- function(id, animal_types, groups, start_date, end_date) {
    ns <- shiny::NS(id)

    shiny::tabPanel(
        shiny::fluidRow(
            shiny::column(
                shiny::wellPanel(
                    shiny::h3("Filter"),
                    shinyWidgets::pickerInput(
                        inputId = ns("filter_animal_types"),
                        label = "Animal types",
                        multiple = TRUE,
                        choices = animal_types,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = ns("filter_gender"),
                        label = "Genders",
                        multiple = TRUE,
                        choices = NULL,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = ns("filter_age"),
                        label = "Age categories",
                        multiple = TRUE,
                        choices = NULL,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = ns("filter_medication"),
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
                        id = ns("map"),
                        icon = shiny::icon("image"),
                        label = " Save map as PNG"
                    ),
                    shiny::br(), shiny::br(),
                    shiny::downloadButton(
                        outputId = ns("download"),
                        label = " Download current map data",
                        icon = shiny::icon("file-csv")
                    ),
                    shiny::br(), shiny::br(),
                    shiny::actionButton(
                        inputId = ns("help"),
                        label = "About this page",
                        icon = shiny::icon("circle-info")
                    )
                ),
                width = 3
            ),
            shiny::column(
                shinycssloaders::withSpinner(
                    leaflet::leafletOutput(
                        outputId = ns("map"), height = "75vh"
                    ), hide.ui = FALSE
                ),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        shiny::sliderInput(
                            inputId = ns("slider"),
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
                        shiny::uiOutput(outputId = ns("selected_region")),
                        shiny::p(
                            "Hover the pie charts for detailed information.",
                            style = "font-size:11px"
                        ),
                        width = 12
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput(
                                outputId = ns("pie_animal_types"),
                                height = "27vh", width = "95%"
                            )
                        ),
                        width = 12
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput(
                                outputId = ns("pie_diagnosis"),
                                height = "27vh", width = "95%"
                            )
                        ),
                        width = 12
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput(
                                outputId = ns("pie_medication"),
                                height = "27vh", width = "95%"
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
