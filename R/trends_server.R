#' trends_server
#' Configure the backend of the Trends page
#' @noRd
trends_server <- function(id, amu, countries, atc, val_sub) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- shiny::ns(id)
        selected_area <- shiny::reactiveVal(NULL)

        shiny::observeEvent(
            input$filter_species,
            {
                amu_data <- amu()

                selected_species <- input$filter_species
                if (is.null(selected_species))
                    selected_species <- sort(unique(amu_data$AnimalType))

                populate_selection(
                    session = session,
                    select_id = ns("filter_gender"),
                    choices = amu_data[
                        amu_data$AnimalType %in%
                            selected_species, ]$Gender
                )

                populate_selection(
                    session = session,
                    select_id = ns("filter_age"),
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

                if (is.null(selected_groups))
                    selected_groups <- sort(unique(atc_data$subgroup_1))

                valid_subgroups <- atc_data[atc_data$subgroup_1 %in%
                        selected_groups, ]$subgroup_2

                populate_selection(
                    session = session,
                    select_id = ns("filter_medication_subgroup"),
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

                choice_names <- c(
                    "None", "Animal type/species", choice_name,
                    "Diagnosis", "Medication group"
                )
                choice_values <- c(
                    "", "AnimalType", choice_value, "Diagnosis", "subgroup_1"
                )

                shiny::updateRadioButtons(
                    session = session,
                    inputId = ns("agg_group"),
                    choiceNames = choice_names,
                    choiceValues = choice_values,
                    selected = ifelse(
                        current_group %in% c(
                            "AnimalType", "Diagnosis", "subgroup_1"
                        ),
                        current_group,
                        ""
                    )
                )
            }
        )

        trends_data <- shiny::reactive({
            amu_data <- amu()
            DateTransaction <- # nolint
                AnimalType <- # nolint
                AnimalSpecies <- # nolint
                Gender <- # nolint
                AgeCategory <- # nolint
                subgroup_2 <- # nolint
                NULL # nolint

            all_species <- amu_data$AnimalType
            all_genders <- amu_data$Gender
            all_ages <- amu_data$AgeCategory
            daterange <- as.Date(input$slider)

            amu_data <- amu_data[
                DateTransaction >= daterange[1] &
                    DateTransaction <= daterange[2] &
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
                        format(amu_data$DateTransaction, "%B"),
                        levels = month.name
                    ),
                    as.factor(format(amu_data$DateTransaction, "%Y"))
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


            agg <- agg_x(input$agg_x)
            group_agg <- input$agg_group


            if (nchar(group_agg) > 0)
                agg <- c(agg, group_agg)

            amu_data <- amu_data[
                , round(sum(ActiveSubstanceKg), 2), by = agg
            ]

            data.table::setnames(amu_data, "V1", "N")

            selected_area(NULL)

            amu_data[order(get(agg[1]))]
        })

        output$plot <- plotly::renderPlotly({
            x <- agg_x(input$agg_x) # nolint

            chart_type <- input$chart_type

            if (chart_type == "bar") {
                type <- "bar"
                mode <- NULL
            } else if (chart_type == "line") {
                type <- "scatter"
                mode <- "lines+markers"
            } else if (chart_type == "scatter") {
                type <- "scatter"
                mode <- "markers"
            } else {
                stop("This shouldn't happen")
            }

            if (nchar(input$agg_group) > 0) {
                p <- plotly::plot_ly(
                    data = trends_data(),
                    x = ~get(x),
                    y = ~N,
                    type = type,
                    mode = mode,
                    color = ~get(input$agg_group)
                )
            } else {
                p <- plotly::plot_ly(
                    data = trends_data(),
                    x = ~get(x),
                    y = ~N,
                    type = type,
                    mode = mode
                )
            }

            p |>
                plotly::layout(
                    xaxis = list(title = x),
                    yaxis = list(title = "Substance consumed (kg)"),
                    legend = list(title = list(text = input$agg_group))
                )
        })

        shiny::observeEvent(input$help, {
            help_popup(
                title = "Page guide",
                content = shiny::includeMarkdown(
                    path_to_markdown("popup_trends.md")
                )
            )
        })

        output$download <- shiny::downloadHandler(
            filename = "AMView_trend_data.csv",
            content = function(f) {
                write.csv(x = trends_data(), file = f, row.names = FALSE)
            }
        )
    })
}

#' @noRd
agg_x <- function(agg) {
    switch(
        agg,
        "date" = "DateTransaction",
        "month" = "yearmonth",
        "year" = "year",
        "NUTS3" = "NUTS3"
    )
}
