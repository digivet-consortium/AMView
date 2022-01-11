##' app_ui
##'
##' Configure the UI (frontend) side of the Shiny app
##' @noRd
##' @import markdown
##' @import shiny 
##' @import data.table
##' @import bslib

app_ui <- function() {
  test <- data.table(herdsize)
    navbarPage("Herdsize dashboard",
               theme = bs_theme(bootswatch = "flatly"),


               tabPanel("Shiny app readme",
                        includeMarkdown(
                            system.file("README.md",
                                        package = "shinyHerdsizeDash"))),

               tabPanel("Dashboard",
                        h1("Herdsize"),
                        sidebarLayout(

                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              selectInput(
                                "region",
                                label = "Region",
                                choices = unique(herdsize[,region]),
                                selected = unique(herdsize[,region])[1]
                              ),
                              sliderInput(
                                "time", 
                                label="Select time span:",
                                min = min(herdsize[,time]),
                                max = max(herdsize[,time]),
                                value = c(min(herdsize[,time]),max(herdsize[,time]))
                              )
                            ),

                            # Main panel for displaying outputs ----
                            mainPanel(

                                # Output: 
                              tabsetPanel(
                                tabPanel("Plot line", plotOutput("plot1")), 
                                tabPanel("Table", tableOutput("table"))
                              )

                            )
                        )))

}
