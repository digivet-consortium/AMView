#' Configure the backend of the map page
#'
#' @noRd
map_server <- function(id, amu, countries) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- shiny::NS(id)

        selected_area <- shiny::reactiveVal(NULL)

        shiny::observeEvent(
            input$filter_species,
            {
                amu_data <- amu()

                selected_species <- input$filter_species
                if (is.null(selected_species))
                    selected_species <- sort(unique(amu_data$AnimalSpecies))

                populate_selection(
                    id = id,
                    session = session,
                    select_id = "filter_animal_types",
                    choices = amu_data[
                        amu_data$AnimalSpecies %in%
                            selected_species, ]$AnimalType
                )

                populate_selection(
                    id = id,
                    session = session,
                    select_id = "filter_gender",
                    choices = amu_data[
                        amu_data$AnimalSpecies %in%
                            selected_species, ]$Gender
                )

                populate_selection(
                    id = id,
                    session = session,
                    select_id = "filter_age",
                    choices = amu_data[
                        amu_data$AnimalSpecies %in%
                            selected_species, ]$AgeCategory
                )
            },
            ignoreNULL = FALSE
        )

        map_data <- shiny::reactive({
            selected_area(NULL)
            create_map_data(
                amu_data = amu(),
                regions = countries(),
                daterange = as.Date(input$slider),
                filter_species = input$fitler_species,
                filter_animal_types = input$filter_animal_types,
                filter_gender = input$filter_gender,
                filter_age = input$filter_age,
                filter_medication = input$filter_medication,
                geo_group = TRUE
            )
        })

        pie_data <- shiny::reactive({
            selected_area(NULL)
            create_map_data(
                amu_data = amu(),
                regions = countries(),
                daterange = as.Date(input$slider),
                filter_species = input$fitler_species,
                filter_animal_types = input$filter_animal_types,
                filter_gender = input$filter_gender,
                filter_age = input$filter_age,
                filter_medication = input$filter_medication,
                geo_group = FALSE
            )
        })

        shiny::observeEvent(selected_area(), {
            s_a <- selected_area()
            map_proxy <- leaflet::leafletProxy(mapId = ns("map"))
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

        shiny::observeEvent(input$help, {
            help_popup(
                title = "Page guide",
                content = shiny::includeMarkdown(
                    path_to_markdown("popup_map.md")
                )
            )
        })

        output$map <- leaflet::renderLeaflet({
            m_d <- map_data()
            palette <- leaflet::colorBin(palette = "YlOrRd", domain = m_d$N)
            labels <- htmltools::htmlEscape(paste0(
                m_d$NAME_LATN, ": ", m_d$N, " kg"
            ))

            leaflet::leaflet(
                data = m_d,
                options = leaflet::leafletOptions(scrollWheelZoom = FALSE)
            )  |>
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
                    position = "bottomright",
                    pal = palette,
                    values = m_d$N,
                    title = "AMU per region"
                )
        })

        output$pie_animal_types <- plotly::renderPlotly({
            dt <- data.table::setDT(pie_data())

            region <- selected_area()

            NUTS3 <- NULL #nolint

            if (!is.null(region))
                dt <- dt[NUTS3 == region]

            summary_pie(
                data = dt,
                count_var = "ActiveSubstanceKg",
                group_var = "AnimalType",
                title = "AMU per animal animal type"
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
                region <- "all regions"
            } else {
                m_d <- map_data()
                region <- paste0(
                    m_d[m_d$NUTS_ID == region, ]$NAME_LATN, " (", region, ")"
                )
            }

            shiny::h5(paste0("AMU breakdown: ", region))
        })

        output$download <- shiny::downloadHandler(
            filename = "AMView_map_data.csv",
            content = function(f) {
                write.csv(x = map_data(), file = f, row.names = FALSE)
            }
        )
    })
}

#' @noRd
create_map_data <- function(
    amu_data,
    regions,
    daterange,
    filter_species,
    filter_animal_types,
    filter_gender,
    filter_age,
    filter_medication,
    geo_group = TRUE
) {
    all_species <- amu_data$AnimalSpecies
    all_animal_types <- amu_data$AnimalType
    all_genders <- amu_data$Gender
    all_ages <- amu_data$AgeCategory
    all_groups <- amu_data$subgroup_1

    DateTransaction <- AnimalSpecies <- AnimalType <- Gender <- #nolint
    AgeCategory <- subgroup_1 <- ActiveSubstanceKg <- NULL #nolint

    dt <- amu_data[
        DateTransaction >= daterange[1] &
            DateTransaction <= daterange[2] &
            AnimalSpecies %in% filter_data(all_species, filter_species) &
            AnimalType %in% filter_data(
                all_animal_types, filter_animal_types
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

    regions
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
        textinfo = "none"
    ) |> plotly::layout(
        title = list(text = title, font = list(size = 11)), showlegend = FALSE
    )
}
