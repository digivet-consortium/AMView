#' app_ui
#'
#' Configure the UI (frontend) side of the Shiny app
#' @noRd
#'
app_ui <- function() {
    strftime_url <-
        "https://svastatichosting.z6.web.core.windows.net/js/strftime-min.js"

    ## Dashboard UI setup
    shiny::fluidPage(
        shiny::tags$script(src = strftime_url),

        shiny::includeCSS(system.file("styles.css",
                                      package = "AMView")),

        shinydashboard::dashboardPage(
            ## Header
            shinydashboard::dashboardHeader(title = "AMView",
                shiny::tags$li(
                class = "dropdown",
                shiny::tags$a(
                    shiny::img(
                    src = "https://www.sva.se/media/j5th5vl0/sva_logo.svg",
                    width = "150",
                    height = "50"),
                    href = "https://www.sva.se"
                )
            )
        ),

        ## Sidebar menu
        shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
                shinydashboard::menuItem("Start", tabName = "start"))),

        ## Content
        shinydashboard::dashboardBody(
            shinydashboard::tabItems(
                tab_start(),
            )
        )
    ))
}

#' @noRd
tab_start <- function() {
    shinydashboard::tabItem("start")
}
