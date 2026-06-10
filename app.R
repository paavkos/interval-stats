# GPX file -based Strava-style interval analyzer for runners
# Paavo Kosunen
# 9.7.2026

#-------------------------------------------------------------------------------

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(leaflet)
library(osmdata)
library(sf)
library(tidygeocoder)
library(osrm)
library(geosphere)
library(zoo)
library(changepoint)
library(data.table)

#-------------------------------------------------------------------------------

source("~/Desktop/GPX-Analyzer/functions_interval.R") # tähän oikea polku

# käyttöliittymä
ui <- fluidPage(
  fileInput("gpx_file", "GPX file"),
  tabsetPanel(id = "main_tabs",
              
              tabPanel(
                "Map",
                leafletOutput("map", height = 700)
              ),
              
              tabPanel(
                "Intervals",
                
                selectInput(
                  "segment_type",
                  "Segment type",
                  choices = c("All", "Sprint", "Slow"),
                  selected = "All"
                ),
                
                numericInput(
                  "min_distance",
                  "Min distance (m)",
                  value = 0,
                  min = 0
                ),
                
                actionButton(
                  "apply_filter",
                  "Apply filter"
                ),
                
                tableOutput("interval_table"),
                tableOutput("stats_summary")
              ),
              
              tabPanel(
                "Analytics",
                verbatimTextOutput("stats_summary"),
                plotOutput("hist_lengths"),
                plotOutput("hist_duration"),
                plotOutput("scatter")
                # DT::DTOutput("sprint_table"),
                # plotOutput("test")
              )
  )
)


# RAKENNE_ESIMERKKI
server <- function(input, output, session){
  
  # Data inputti sisään, lue tiedosto
  gpx_raw <- reactive({
    req(input$gpx_file)
    parse_gpx(input$gpx_file$datapath)
  })
  
  # 2. Datan puhdistus
  track <- reactive({
    req(gpx_raw())
    clean_data(gpx_raw())
  })
  
  # 3. DERIVATIIVIT (speed etc.)
  intervallit <- reactive({
    req(track())
    intervals(track())
  })
  
  # 3.5. Geomit kuntoon 
  full <- reactive({
    req(gpx_raw(), intervallit())
    to_linestrings(gpx_raw(), intervallit())
  })
  
  # 4. Segmentit
  segmentit <- reactive({
    req(intervallit())
    segments(intervallit())
  })
  
  # 4.1. Segmenttien suodatus
  filtered <- reactive({
    req(segmentit())
    df <- segmentit()
    # suodata
    if (input$segment_type == "Sprint") {
      df <- df %>% dplyr::filter(class == "sprint")
    }
    df
    # distance filter
    df %>%
      dplyr::filter(length >= input$min_distance)
  })
  
  # 4.5. Linestringeihin liitetään segmentti-tiedot
  data <- reactive({
    req(full(),segmentit())
    left_join(full(), segmentit(), by = 'segment')
  })
  
  # 5. Kartta visualisointiin
  output$map <- renderLeaflet({
    req(data())
    
    leaflet(data()) %>%
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolylines(
        color = ~ifelse(class == "sprint", "#FC4C02", "#ffffff"),
        opacity = 1,
        popup = ~paste(
          class, " ", segment,
          "<br>Distance:", round(length), "m",
          "<br>Duration: ", round(seconds/60,2), "min",
          "<br>Pace:", round(interval_pace,2), "min/km"
        )
      )
  })
  
  # 6. Taulukko 
  output$interval_table <- renderTable(filtered())
  
  # 7. Tilastoja lenkistä
  output$stats_summary <- renderTable({
    # filtteröi
    df <- segmentit() %>%
      dplyr::filter(class == "sprint", length >= 10)
    #
    df %>%
      dplyr::summarise(
        longest = max(length, na.rm = TRUE),
        shortest = min(length, na.rm = TRUE),
        fastest = min(interval_pace, na.rm = TRUE),
        slowest = max(interval_pace, na.rm = TRUE)
      )
  })
  
  # 8. Kiinnostavia kuvioita
  output$hist_lengths <- renderPlot({
    df <- segmentit() %>% 
      filter(class == "sprint", length >= 10)
    hist(df$length)
  })
  
  # histogrammi kestoista
  output$hist_duration <- renderPlot({
    df <- segmentit() %>% 
      filter(class == "sprint", length >= 10)
    hist(df$seconds)
  })
  
  # yli 10m sprinteissä hajontakuvio: pituuden ja nopeuden korrelaatio?
  output$scatter <- renderPlot({
    
    df <- segmentit() %>%
      dplyr::filter(class == "sprint", length >= 10)
    
    ggplot(df, aes(x = length, y = interval_pace)) +
      geom_point()
  })
}



shinyApp(ui, server)

