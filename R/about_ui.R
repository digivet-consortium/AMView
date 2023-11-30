#' @noRd
aobut_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tabPanel(
        shiny::fluidRow(
            shiny::column(
                shiny::includeMarkdown(
                    path_to_markdown("about.md")
                ),
                width = 8, offset = 2
            )
        ),
        shiny::fluidRow(
            shiny::column(
                shiny::downloadButton(
                    outputId = ns("download_data_structure"),
                    label = " Common data structure: download template"
                ),
                width = 3, offset = 2
            )
        ),
        title = "About"
    )
}
