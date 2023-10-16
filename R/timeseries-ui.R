#' @noRd
timeseries_filters <- function(species, groups) {
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
                        label = "Animal types",
                        multiple = TRUE,
                        choices = species,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = TRUE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "filter_gender",
                        label = "Genders",
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
                    shinyWidgets::pickerInput(
                        inputId = "filter_medication_group",
                        label = "Medication groups",
                        multiple = TRUE,
                        choices = groups,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = FALSE
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "filter_medication_subgroup",
                        label = "Medication subgroups",
                        multiple = TRUE,
                        choices = NULL,
                        options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE
                        ),
                        inline = FALSE
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
                    inputId = "timeseries_slider",
                    label = "Date range",
                    min = start_date,
                    max = end_date,
                    value = c(start_date, end_date),
                    width = "100%"
                ),
                width = 9, offset = 1
            ),
            shiny::column(
                shiny::selectInput(
                    inputId = "chart_type",
                    label = "Chart type",
                    choices = list(
                        "Bar chart" = "bar",
                        "Line chart" = "lines",
                        "Scatter plot" = "scatter"
                    ),
                    width = "100%",
                    selected = "bar"
                ),
                width = 1, offset = 1
            )
        )
    )
}
