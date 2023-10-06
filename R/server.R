#' app_server
#'
#' Manage the server (backend) side of the shiny app.
#' @noRd
app_server <- function(input, output, session) {
    amu <- shiny::reactiveVal(get_amu())
    atc <- get_atc()
    val_sub <- shiny::reactiveVal(sort(unique(atc$subgroup_2)))
    atc <- shiny::reactiveVal(atc)


    shiny::observeEvent(
        input$filter_species,
        {
            amu_data <- amu()

            selected_species <- input$filter_species
            if (is.null(selected_species))
                selected_species <- sort(unique(amu_data$AnimalType))

            populate_selection(
                session = session,
                select_id = "filter_gender",
                choices = amu_data[
                    amu_data$AnimalType %in%
                        selected_species, ]$Gender
            )

            populate_selection(
                session = session,
                select_id = "filter_age",
                choices = amu_data[
                    amu_data$AnimalType %in%
                        selected_species, ]$AgeCategory
            )
        },
        ignoreNULL = FALSE
    )

    shiny::observeEvent(
        input$filter_medication_group,
        {
            atc_data <- atc()

            selected_groups <- input$filter_medication_group

            print(selected_groups)

            if (is.null(selected_groups))
                selected_groups <- sort(unique(atc_data$subgroup_1))

            valid_subgroups <- atc_data[atc_data$subgroup_1 %in%
                    selected_groups, ]$subgroup_2

            populate_selection(
                session = session,
                select_id = "filter_medication_subgroup",
                choices = valid_subgroups
            )

            val_sub(valid_subgroups)
        },
        ignoreNULL = FALSE
    )

    shiny::observeEvent(
        input$agg_x,
        {
            agg <- input$agg_x
            current_group <- input$agg_group

            choice_name <- ifelse(
                agg == "NUTS3", "Year", "Region (NUTS3)"
            )
            choice_value <- ifelse(
                agg == "NUTS3", "year", "NUTS3"
            )

            choice_names <- c("Animal type/species", choice_name, "Diagnosis")
            choice_values <- c("AnimalType", choice_value, "Diagnosis")

            shiny::updateRadioButtons(
                session = session,
                inputId = "agg_group",
                choiceNames = choice_names,
                choiceValues = choice_values,
                selected = ifelse(
                    current_group %in% c("AnimalType", "Diagnosis"),
                    current_group,
                    choice_value
                )
            )
        }
    )

    filtered_data <- shiny::reactive({
        amu_data <- amu()
        DateTreatment <- # nolint
            AnimalType <- # nolint
            AnimalSpecies <- # nolint
            Gender <- # nolint
            AgeCategory <- # nolint
            subgroup_2 <- # nolint
            NULL # nolint

        all_species <- amu_data$AnimalType
        all_genders <- amu_data$Gender
        all_ages <- amu_data$AgeCategory
        daterange <- input$timeslider

        amu_data <- amu_data[
            DateTreatment >= daterange[1] &
                DateTreatment <= daterange[2] &
                AnimalType %in% filter_data(
                    all_species, input$filter_species) &
                Gender %in% filter_data(
                    all_genders, input$filter_gender) &
                AgeCategory %in% filter_data(all_ages, input$filter_age) &
                subgroup_2 %in% filter_data(
                    val_sub(), input$filer_medication_subgroup
                )
        ]

        data.table::set(
            amu_data,
            j = c("month", "year"),
            value = list(
                factor(
                    format(amu_data$DateTreatment, "%B"),
                    levels = month.name
                ),
                as.factor(format(amu_data$DateTreatment, "%Y"))
            )
        )

        year <- month <- yearmonth <- ActiveSubstanceKg <- NULL # nolint

        years <- levels(amu_data$year)
        months <- levels(amu_data$month)

        data.table::set(
            amu_data,
            j = "yearmonth",
            value = factor(
                paste(amu_data$year, amu_data$month, sep = "-"),
                levels = paste(
                    rep(years, each = length(months)),
                    months,
                    sep = "-"
                )
            )
        )

        x_agg <- agg_x(input$agg_x)
        group_agg <- input$agg_group
        amu_data <- amu_data[
            , sum(ActiveSubstanceKg), by = c(x_agg, group_agg)
        ]
        print(str(amu_data))

        data.table::setnames(amu_data, "V1", "N")

        amu_data
    })

    output$plot <- plotly::renderPlotly({
        x <- agg_x(input$agg_x) # nolint

        chart_type <- input$chart_type

        if (chart_type == "bar") {
            type <- "bar"
            mode <- NULL
        } else if (chart_type == "lines") {
            type <- "scatter"
            mode <- "lines"
        } else {
            stop("This shouldn't happen")
        }

        plotly::plot_ly(
            data = filtered_data(),
            x = ~get(x),
            y = ~N,
            type = type,
            mode = mode,
            color = ~get(input$agg_group)
        ) |>
            plotly::layout(
                xaxis = list(title = x),
                yaxis = list(title = "Substance consumed (kg)"),
                legend = list(title = list(text = input$agg_group))
            )
    })
}

#' @noRd
populate_selection <- function(session, select_id, choices) {
    shinyWidgets::updatePickerInput(
        session = session,
        inputId = select_id,
        choices = sort(unique(choices))
    )
}

#' @noRd
filter_data <- function(data, selection) {
    if (is.null(selection)) data else data[data %in% selection]
}

#' @noRd
agg_x <- function(agg) {
    switch(
        agg,
        "date" = "DateTreatment",
        "month" = "yearmonth",
        "year" = "year",
        "NUTS3" = "NUTS3"
    )
}
