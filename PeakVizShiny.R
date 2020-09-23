  
library(jsonlite)
library(leaflet)
library(dplyr)
library(shiny)
  

### LOAD DATA ####

FILE <- 'deine-berge.json'
peakFile <- fromJSON(FILE)

# for filter input
countries <- unique(peakFile$country)


### UI ###

ui <- fluidPage(
        
        headerPanel('My Peak Navigator'),
        
        
        sidebarPanel(
          selectInput(inputId = 'country', 
                      'Select a county', 
                      choices = countries, 
                      selected ='Deutschland'),
          
          uiOutput('peakgroup'),
          
          sliderInput(inputId = 'elevationRange', 
                      'Select a peak elevation range', 
                      min = 50, 
                      max = 5000, 
                      value = c(1500, 3000), 
                      step = 50),
          
          h3(textOutput(outputId = 'sum')),
        ),
        
        
        mainPanel(
          leafletOutput(outputId = 'peakMap', height = '400px'),
          plotOutput(outputId = 'histogram')
        )
      )


### SHINY SERVER ###

  server <- function(input, output){
          
          # REACTIVE INPUT VALUE FOR PEAK GROUP
          output$peakgroup <- renderUI({
            filteredFile <- peakFile %>% filter(peakFile$country == input$country)
            peakgroups <-unique(filteredFile$peak_group)
            selectInput('peakgroup', 
                        'Select one or more peak groups', 
                        peakgroups, 
                        selected = NULL, 
                        multiple = TRUE)
            })
      
          # FILTER PEAK DATASET (COUNTRY & ELEVATION)
          filterPeaks <- reactive({
              filtered <- peakFile %>% filter(
                country %in% input$country,
                between(elevantion_meters,
                        min(input$elevationRange), 
                        max(input$elevationRange))
                )
             })
          
          # FILTER PEAK DATASET (PEAKGROUP)
          filterPeakGroup <- reactive({
              filtered <- filterPeaks() %>% filter(
                peak_group %in% input$peakgroup)
            })
  
          # DRAW A MAP WITH MARKERS
          output$peakMap <- renderLeaflet({
            # apply filter setting
            if(!is.null(input$peakgroup)){
              data <- filterPeakGroup()
            }
            else {
              data <- filterPeaks()
            }
            
            # map configuration
            leaflet(data) %>% addTiles() %>% addMarkers(
              ~longitude, ~latitude, 
              label=paste(data$name, ', ',data$elevantion_meters, ' Metres', ', ', data$country)
            )
          })
          
          # DRAW A HISTOGRAM OF PEAK ELEVATION
          output$histogram <- renderPlot({
            # apply filter setting
            if(!is.null(input$peakgroup)){
              data <- filterPeakGroup()
            } 
            else {
              data <- filterPeaks()
            } 
            
          # draw histogram
          data$elevation_meters <- as.numeric(data$elevantion_meters)
          hist(data$elevation_meters, 
               main = 'Distribution of peak elevation', 
               breaks = 100)
          })
          
          # COMPUTE NUMBER OF PEAKS IN SELECTION
          output$sum <- renderText({
            # apply filter setting
            if(!is.null(input$peakgroup)){
              nrow(filterPeakGroup())
            }
            else {
              nrow(filterPeaks())}
          })
    }

shinyApp(ui, server)
