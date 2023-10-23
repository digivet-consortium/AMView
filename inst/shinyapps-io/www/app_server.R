#' app_server
#'
#' Manage the server (backend) side of the shiny app.
#' @noRd
app_server <- function(input, output, session) {
    amu <- get_amu()
    countries <- get_spatial_data(unique(amu$Country))
    amu <- shiny::reactiveVal(amu)
    countries <- shiny::reactiveVal(countries)

    atc <- get_atc()
    val_sub <- shiny::reactiveVal(sort(unique(atc$subgroup_2)))
    atc <- shiny::reactiveVal(atc)

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
        input$map_filter_species,
        {
            amu_data <- amu()

            selected_species <- input$map_filter_species
            if (is.null(selected_species))
                selected_species <- sort(unique(amu_data$AnimalType))

            populate_selection(
                session = session,
                select_id = "map_filter_gender",
                choices = amu_data[
                    amu_data$AnimalType %in%
                        selected_species, ]$Gender
            )

            populate_selection(
                session = session,
                select_id = "map_filter_age",
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

            choice_names <- c(
                "Animal type/species", choice_name,
                "Diagnosis", "Medication group"
            )
            choice_values <- c(
                "AnimalType", choice_value, "Diagnosis", "subgroup_1"
            )

            shiny::updateRadioButtons(
                session = session,
                inputId = "agg_group",
                choiceNames = choice_names,
                choiceValues = choice_values,
                selected = ifelse(
                    current_group %in% c(
                        "AnimalType", "Diagnosis", "subgroup_1"
                    ),
                    current_group,
                    choice_value
                )
            )
        }
    )

    timeseries_data <- shiny::reactive({
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
        daterange <- as.Date(input$timeseries_slider)

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

        x_agg <- agg_x(input$agg_x)
        group_agg <- input$agg_group
        amu_data <- amu_data[
            , round(sum(ActiveSubstanceKg), 2), by = c(x_agg, group_agg)
        ]

        data.table::setnames(amu_data, "V1", "N")

        selected_area(NULL)

        amu_data[order(get(x_agg), get(group_agg))]
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

        plotly::plot_ly(
            data = timeseries_data(),
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

    map_data <- shiny::reactive({
        selected_area(NULL)
        create_map_data(
            amu_data = amu(),
            regions = countries(),
            daterange = as.Date(input$map_slider),
            filter_species = input$map_filter_species,
            filter_gender = input$map_filter_gender,
            filter_age = input$map_filter_age,
            filter_medication = input$map_filter_medication,
            geo_group = TRUE
        )
    })

    pie_data <- shiny::reactive({
        selected_area(NULL)
        create_map_data(
            amu_data = amu(),
            regions = countries(),
            daterange = as.Date(input$map_slider),
            filter_species = input$map_filter_species,
            filter_gender = input$map_filter_gender,
            filter_age = input$map_filter_age,
            filter_medication = input$map_filter_medication,
            geo_group = FALSE
        )
    })

    shiny::observeEvent(selected_area(), {
        s_a <- selected_area()
        map_proxy <- leaflet::leafletProxy(mapId = "map")
        m_d <- map_data()

        if (is.null(s_a)) {
            map_proxy |> leaflet::removeShape("selected")
        } else {
            map_proxy |> leaflet::addPolylines(
                stroke = TRUE,
                weight = 3,
                color = "blue",
                data = m_d[m_d$NUTS_ID == s_a, ],
                layerId = "selected"
            )
        }
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$map_shape_click, {
        prev_selected <- selected_area()
        new_selected <- input$map_shape_click$id

        if (!is.null(prev_selected) && new_selected == prev_selected) {
            selected_area(NULL)
        } else {
            selected_area(new_selected)
        }
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$help_timeseries, {
        help_popup(
            title = "Page guide",
            content = shiny::p(
                "This is a test!"
            )
        )
    })

    shiny::observeEvent(input$help_map, {
        help_popup(
            title = "Page guide",
            content = shiny::p(
                "This is a test!"
            )
        )
    })

    output$download_timeseries <- shiny::downloadHandler(
        filename = "AMView_trend_data.csv",
        content = function(f) {
            write.csv(x = timeseries_data(), file = f, row.names = FALSE)
        }
    )

    output$download_map <- shiny::downloadHandler(
        filename = "AMView_map_data.csv",
        content = function(f) {
            write.csv(x = map_data(), file = f, row.names = FALSE)
        }
    )

    output$map <- leaflet::renderLeaflet({
        m_d <- map_data()
        palette <- leaflet::colorBin(palette = "YlOrRd", domain = m_d$N)
        labels <- htmltools::htmlEscape(paste0(
            m_d$NAME_LATN, ": ", m_d$N, " kg"
        ))

        leaflet::leaflet(data = m_d)  |>
            leaflet::addTiles() |>
            leaflet::addPolygons(
                fillOpacity = 1, fillColor = palette(m_d$N),
                color = "black", weight = 1, label = labels,
                labelOptions = leaflet::labelOptions(
                    textsize = "12px",
                    style = list(
                        "font-weight" = "bold",
                        padding = "5px"
                    ),
                ),
                layerId = ~NUTS_ID
            ) |>
            leaflet::addLegend(
                position = "bottomleft",
                pal = palette,
                values = m_d$N,
                title = "AMU per region"
            )
    })

    output$pie_species <- plotly::renderPlotly({
        dt <- data.table::setDT(pie_data())

        region <- selected_area()

        NUTS3 <- NULL #nolint

        if (!is.null(region))
            dt <- dt[NUTS3 == region]

        summary_pie(
            data = dt,
            count_var = "ActiveSubstanceKg",
            group_var = "AnimalType",
            title = "AMU per animal type/species"
        )
    })

    output$pie_diagnosis <- plotly::renderPlotly({
        dt <- data.table::setDT(pie_data())

        region <- selected_area()

        NUTS3 <- NULL #nolint

        if (!is.null(region))
            dt <- dt[NUTS3 == region]

        summary_pie(
            data = dt,
            count_var = "ActiveSubstanceKg",
            group_var = "Diagnosis",
            title = "AMU per diagnosis"
        )
    })

    output$pie_medication <- plotly::renderPlotly({
        dt <- data.table::setDT(pie_data())

        region <- selected_area()

        NUTS3 <- NULL #nolint

        if (!is.null(region))
            dt <- dt[NUTS3 == region]

        summary_pie(
            data = dt,
            count_var = "ActiveSubstanceKg",
            group_var = "subgroup_1",
            title = "AMU per medication group"
        )
    })

    output$selected_region <- shiny::renderUI({
        region <- selected_area()

        if (is.null(region)) {
            region <- "All regions"
        } else {
            m_d <- map_data()
            region <- paste0(
                m_d[m_d$NUTS_ID == region, ]$NAME_LATN, " (", region, ")"
            )
        }

        shiny::h4(paste0("Selected: ", region))
    })
}

#' @noRd
help_popup <- function(title, content) {
    shiny::showModal(
        shiny::modalDialog(
            content,
            title = title,
            footer = shiny::modalButton("Close"),
            easyClose = TRUE,
            size = "l"
        )
    )
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
        "date" = "DateTransaction",
        "month" = "yearmonth",
        "year" = "year",
        "NUTS3" = "NUTS3"
    )
}

#' @noRd
create_map_data <- function(
    amu_data,
    regions,
    daterange,
    filter_species,
    filter_gender,
    filter_age,
    filter_medication,
    geo_group = TRUE
) {
    all_species <- amu_data$AnimalType
    all_genders <- amu_data$Gender
    all_ages <- amu_data$AgeCategory
    all_groups <- amu_data$subgroup_1

    DateTransaction <- AnimalType <- Gender <- #nolint
    AgeCategory <- subgroup_1 <- ActiveSubstanceKg <- NULL #nolint

    dt <- amu_data[
        DateTransaction >= daterange[1] &
            DateTransaction <= daterange[2] &
            AnimalType %in% filter_data(
                all_species, filter_species
            ) &
            Gender %in% filter_data(
                all_genders, filter_gender
            ) &
            AgeCategory %in% filter_data(
                all_ages, filter_age
            ) &
            subgroup_1 %in% filter_data(
                all_groups, filter_medication
            )
    ]

    if (isFALSE(geo_group))
        return(dt)

    sums <- dt[, round(sum(ActiveSubstanceKg), 2), by = "NUTS3"]

    stopifnot(all(sums$NUTS3 %in% regions$NUTS_ID))

    regions[match(sums$NUTS3, regions$NUTS_ID), "N"] <- sums$V1

    return(regions)
}

#' @noRd
summary_pie <- function(data, count_var, group_var, title) {
    stopifnot(
        data.table::is.data.table(data),
        count_var %in% colnames(data),
        group_var %in% colnames(data),
        is.character(title)
    )

    data <- data[, sum(get(count_var)), by = group_var]

    plotly::plot_ly(
        data = data,
        labels = ~get(group_var),
        values = ~V1,
        type = "pie",
        textinfo = "label+percent",
        textposition = "outside"
    ) |> plotly::layout(title = title, showlegend = FALSE)
}
