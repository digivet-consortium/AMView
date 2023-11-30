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
