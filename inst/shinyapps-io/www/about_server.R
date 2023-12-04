#' about_server
#'
#' Configure the backend of the "About" page
#'
#' @noRd
about_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        output$download_data_structure <- shiny::downloadHandler(
            filename = "AMU_data_structure.xlsx",
            content = function(f) {
                file.copy(
                    from = path_to_data_structure(),
                    to = f
                )
            }
        )
    })
}
