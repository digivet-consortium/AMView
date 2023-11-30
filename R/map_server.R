#' Configure the backend of the map page
#'
#' @noRd
map_server <- function(id, amu, countries, atc, val_sub) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- shiny::NS(id)

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
                    select_id = ns("map_filter_age"),
                    choices = amu_data[
                        amu_data$AnimalType %in%
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

        shiny::observeEvent(input$help_map, {
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
