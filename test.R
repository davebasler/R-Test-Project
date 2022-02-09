# Load libraries
library(leaflet)
library(data.table)
library(shiny)
library(DT)

# Load data
d <- fread("demographics_plus_zip_info.csv")
d[, JoinDate:=dmy(JoinDate)]
#zips <- as.matrix(d[1:10000, list(zip_longitude, zip_latitude)])

# ui part -----------------------------------------------------------------

# Define a map object as output
ui <-  fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("Customer map"),
  sidebarLayout(
    sidebarPanel(checkboxGroupInput("gender", label = h3("Gender"), 
                                    choices = list("Male" = "m", "Female" = "f", "Alien" = "alien"),
                                    selected = c("f","m","alien")),
                 sliderInput("joined", label = h3("Join Date"), 
                             min = as.Date("1965-01-01","%Y-%m-%d"), 
                             max = as.Date("2011-12-31","%Y-%m-%d"),
                             value = c(as.Date("1990-01-01"), as.Date("2008-12-01")))),
    mainPanel(leafletOutput(outputId = "mymap", height = "800px"))
  ),
  DTOutput('tbl'),
  fluidRow(column(3, verbatimTextOutput("test")),
           column(3, verbatimTextOutput("joined")))
)


# server part -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Define map and fill map with data points  
  output$mymap <- renderLeaflet({
    map <- leaflet();
    map <- addTiles(map);
    zips <- as.matrix(d[Gender %in% input$gender & JoinDate >= input$joined[1]
                        & JoinDate <= input$joined[2],
                        list(zip_longitude, zip_latitude)])
    map <- addMarkers(map, data = zips, clusterOptions = markerClusterOptions());
    map <- setView(map, lat= 43, lng= -79, zoom = 3); # North America
    map
  })
  output$test <- renderPrint({ input$gender })
  output$joined <- renderPrint({ input$joined })
  output$tbl = renderDT(
    d[Gender %in% input$gender & JoinDate >= input$joined[1]
      & JoinDate <= input$joined[2],
      list(Customer, Gender)], options = list(lengthChange = FALSE)
  )
}  

# End server

shinyApp(ui = ui, server = server)