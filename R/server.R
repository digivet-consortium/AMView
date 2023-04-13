#' app_server
#'
#' Manage the server (backend) side of the shiny app.
#' @noRd
app_server <- function(input, output, session) {
    output$distPlot <- renderPlot({

        x    <- datasets::faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        graphics::hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "Waiting time to next eruption (in mins)",
             main = "Histogram of waiting times")

    })
}
