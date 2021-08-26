# tutorial https://rstudio.github.io/leaflet/choropleths.html
library(tidyverse)
library(lubridate)
library(leaflet)
library(jsonlite)
library(geojsonio)
library(htmltools)
library(shiny)
library(rsconnect)

# transform json to spatial object
deutschland_kreise <- geojsonio::geojson_read("data/kreise.topo.cached.json", what = "sp")

countryPolygons.df <- as.data.frame(deutschland_kreise)

# read in covid data as csv
df_json <-  fromJSON("data/kreise_coronadaten_seit_coronabeginn.json") %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble() %>% 
  unnest()

df <- df_json %>%
    unnest(casesHistorical, names_repair = "unique")  %>% 
    mutate(date=as_date(date)) %>% 
    select("id", "name", "date", "incidence") %>% 
    arrange(date)  


ui <- fluidPage(
  sliderInput("slider",
              "Dates:",
              min = min(df$date),
              max = max(df$date),
              value=as.Date("2020-10-12"),
              animate=TRUE),
  leafletOutput("mymap")
  )

m <- leaflet(deutschland_kreise) %>%
  setView(10.4515, 51.1657, 5) %>%
  addTiles()

labels <- sprintf(
  "<strong>%s</strong><br/>",
  deutschland_kreise$ags
) %>% lapply(htmltools::HTML)
  
server <- function(input, output, session) { 
  
  output$mymap  <- renderLeaflet({m}) %>% bindCache(m)

  observe({
    req(input$slider)
    pal <-  reactive({colorBin("YlOrRd", domain = df$incidence, bins = c(0, 35, 50, 100, 200, Inf))})
    pal <- pal()
    leafletProxy("mymap", session) %>%
      addPolygons(
        data = deutschland_kreise,
        fillColor = ~pal(df$incidence[df$date==input$slider]),
        weight = 1,
        opacity = 1,
        color = "grey",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
        label = labels)
  })
}    

shinyApp(ui, server)