#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
app_ui <- function() {
    amu <- get_amu()
    start_date <- min(amu$DateTreatment, na.rm = TRUE)
    end_date <- max(amu$DateTreatment, na.rm = TRUE)
    species <- sort(unique(amu$AnimalType))

    strftime_url <-
        "https://svastatichosting.z6.web.core.windows.net/js/strftime-min.js"

    # Dashboard UI setup
    shiny::fluidPage(
        shiny::tags$script(src = strftime_url),

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
        shiny::fluidRow(
            timeseries_filters(species)
        ),
        title = "Timeseries"
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

    shiny::inputPanel(
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
        shiny::h3("Aggregate"),
        shiny::radioButtons(
            inputId = "agg_time",
            label = "Time unit",
            choiceNames = c("Year", "Month", "Date"),
            choiceValues = c("year", "month", "date"),
            selected = "month",
            inline = TRUE
        ),
        shiny::radioButtons(
            inputId = "agg_group",
            label = "Group by",
            choiceNames = c("Species", "Region (NUTS3)", "Diagnosis"),
            choiceValues = c("AnimalType", "NUTS3", "Diagnosis"),
            selected = "NUTS3",
            inline = TRUE
        )
    )
}

timeseries_output <- function(start_date, end_date) {
    shiny::column(
        shiny::fluidRow(plotly::plotlyOutput(outputId = "plot")),
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
                    choiceValues = c("bar", "line"),
                    choiceNames = c("Bar chart", "Line chart"),
                    inline = TRUE,
                    width = "100%",
                    selected = "bar"
                ),
                width = 1, offset = 1
            )
        ),
        width = 9
    )
}
