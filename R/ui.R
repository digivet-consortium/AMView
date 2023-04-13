#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
#'
app_ui <- function() {
    navbarPage("Shiny Example",


               tabPanel("Shiny app readme",
                        includeMarkdown(
                            system.file("README.md",
                                        package = "shinyTemplate"))),

               tabPanel("Example app",
                        h1("Hello Shiny!"),
                        sidebarLayout(

                            # Sidebar panel for inputs ----
                            sidebarPanel(

                                # Input: Slider for the number of bins ----
                                sliderInput(inputId = "bins",
                                            label = "Number of bins:",
                                            min = 1,
                                            max = 50,
                                            value = 30)

                            ),

                            # Main panel for displaying outputs ----
                            mainPanel(

                                # Output: Histogram ----
                                plotOutput(outputId = "distPlot")

                            )
                        )))

}
