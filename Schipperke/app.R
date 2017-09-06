#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("~/../Desktop/port_hackathon/")
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)

ui <- dashboardPage(skin="black",
  dashboardHeader(title = "Schipperke dashboard",   dropdownMenu(type = "notifications",
                                                                 notificationItem(
                                                                   text = "5 incidents in next hour",
                                                                   icon("users")
                                                                 ),
                                                                 notificationItem(
                                                                   text = "12 incidents in past day",
                                                                   icon("truck"),
                                                                   status = "success"
                                                                 ),
                                                                 notificationItem(
                                                                   text = "Prediction accuracy 86%",
                                                                   icon = icon("exclamation-triangle"),
                                                                   status = "warning"
                                                                 )
  )),
  dashboardSidebar( selectInput("State of time", "State:",
                                c("Past" = "past",
                                  "Live" = "live",
                                  "Prediction" = "pr")),
                    dateInput("time_input", "Enter time", value = strptime("12:34:56", "%T")),
                    checkboxInput("Incidents", "Incidents", TRUE), 
                    checkboxInput("Congestion", "Congestion", TRUE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(leafletOutput("mymap")),
      
      box(
        title = "Significance",
        sliderInput("slider", "Sign.:", 1, 100, 50)),
      box(tableOutput("table")
      )
    )
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    x <- fread("incident_with_xy.csv")
    x[, .(date, report_time,  gemelde.locatie)]})
  
  points <- eventReactive(input$recalc, {
    x <- fread("incident_with_xy.csv")
    cbind(x[date>"2017-06-03",as.numeric(y)], x[date>"2017-06-03", as.numeric(x)])
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points()) %>% addRectangles(
        lng1=4.5634322008582, lat1=51.892391734851,
        lng2=4.5119609663735, lat2=51.925336234654,
        fillColor = "red", fillOpacity = 0.5)
    })
  
}

shinyApp(ui, server)

