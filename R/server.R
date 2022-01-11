##' app_server
##'
##' Manage the server (backend) side of the shiny app. This example app just
##' generates some random content using the shinipsum package.
##' @noRd
##' @import shiny
##' @import ggplot2
##' @import gghighlight
##' 
app_server <- function(input, output, session) {
    output$plot1 <- renderPlot({
        
        ggplot(data=herdsize[region==input$region, 
                             .(meanSize=mean(size)), by = time], 
               aes(x=time, y=meanSize)) +
            geom_line(color="#13598d", size=1) +
            gghighlight(time>=input$time[1] & time <= input$time[2]) +
            ggtitle(input$region) +
            labs(x="Time", y="Mean herdsize")
    })
    
    output$table <- renderTable(herdsize[region==input$region & 
                                             time >= input$time[1] & 
                                             time <= input$time[2], 
                                         .(meanSize=mean(size)), by = time])
}
