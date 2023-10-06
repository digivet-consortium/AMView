#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
app_ui <- function() {
    amu <- get_amu()
    start_date <- min(amu$DateTransaction, na.rm = TRUE)
    end_date <- max(amu$DateTransaction, na.rm = TRUE)
    species <- sort(unique(amu$AnimalType))

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
            timeseries_panel(species, start_date, end_date),
            map_panel(),
            about_panel(),
            id = "tabs"
        ),

        theme = bslib::bs_theme(bootswatch = "flatly")
    )
}

#' @noRd
timeseries_panel <- function(species, start_date, end_date) {
    shiny::tabPanel(
        shiny::fluidRow(
            timeseries_output(start_date, end_date)
        ),
        timeseries_filters(species),
        title = "Trends"
    )
}

#' @noRd
map_panel <- function() {
    shiny::tabPanel(
        title = "Map"
    )
}

#' @noRd
about_panel <- function() {
    shiny::tabPanel(
        title = "About"
    )
}

#' @noRd
timeseries_filters <- function(species) {
    atc <- get_atc()
    groups <- sort(unique(atc$subgroup_1))

    shiny::wellPanel(
        shiny::fluidRow(
            shiny::column(
                shiny::fluidRow(
                    shiny::h3("Group & aggregate"),
                    shiny::column(
                        shiny::h5("X axis unit"),
                        shiny::radioButtons(
                            inputId = "agg_x",
                            label = NULL,
                            choiceNames = c(
                                "Year", "Month", "Date", "Region (NUTS3)"
                            ),
                            choiceValues = c("year", "month", "date", "NUTS3"),
                            selected = "month",
                        ),
                        width = 6
                    ),
                    shiny::column(
                        shiny::h5("Group by"),
                        shiny::radioButtons(
                            inputId = "agg_group",
                            label = NULL,
                            choiceNames = c(
                                "Animal type/species",
                                "Region (NUTS3)",
                                "Diagnosis",
                                "Medication group"
                            ),
                            choiceValues = c(
                                "AnimalType", "NUTS3", "Diagnosis", "subgroup_1"
                            ),
                            selected = "NUTS3"
                        ),
                        width = 6
                    )
                ),
                width = 6
            ),
            shiny::column(
                shiny::p(
                    shiny::h3("Filter"),
                    shinyWidgets::pickerInput(
                        inputId = "filter_species",
                        label = "Animal type",
                        multiple = TRUE,
                        choices = species,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = TRUE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "filter_gender",
                        label = "Gender",
                        multiple = TRUE,
                        choices = NULL,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = TRUE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "filter_age",
                        label = "Age categories",
                        multiple = TRUE,
                        choices = NULL,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = TRUE
                    )
                ),
                shiny::p(
                    shiny::h5("Medication filters"),
                    shiny::p(
                        shinyWidgets::pickerInput(
                            inputId = "filter_medication_group",
                            label = "Groups",
                            multiple = TRUE,
                            choices = groups,
                            options = shinyWidgets::pickerOptions(
                                actionsBox = TRUE
                            ),
                            inline = TRUE
                        ),
                        shinyWidgets::pickerInput(
                            inputId = "filter_medication_subgroup",
                            label = "Subgroups",
                            multiple = TRUE,
                            choices = NULL,
                            options = shinyWidgets::pickerOptions(
                                actionsBox = TRUE
                            ),
                            inline = TRUE
                        )
                    )
                ),
                width = 6
            )
        )
    )
}

timeseries_output <- function(start_date, end_date) {
    list(
        shiny::fluidRow(
            plotly::plotlyOutput(outputId = "plot")
        ),
        shiny::fluidRow(
            shiny::column(
                shiny::sliderInput(
                    inputId = "timeslider",
                    label = "Date range",
                    min = start_date,
                    max = end_date,
                    value = c(start_date, end_date),
                    width = "100%"
                ),
                width = 9, offset = 1
            ),
            shiny::column(
                shiny::radioButtons(
                    inputId = "chart_type",
                    label = "Chart type",
                    choiceValues = c("bar", "lines"),
                    choiceNames = c("Bar chart", "Line chart"),
                    inline = TRUE,
                    width = "100%",
                    selected = "bar"
                ),
                width = 1, offset = 1
            )
        )
    )
}
