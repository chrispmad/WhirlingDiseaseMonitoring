library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(leafpop)
library(lubridate)

# Load and clean data
aqua_data <- read.csv("data/WaterTemperature.csv")
names(aqua_data) <- gsub("\\.", "_", names(aqua_data))

aqua_data_limits <- aqua_data |> 
  filter(!is.na(Value)) |>
  filter(Value < 50)  #


aqua_limits_sf <- st_as_sf(aqua_data_limits, coords = c("Longitude", "Latitude"), crs = 4326)
aqua_limits_sf$Date_Time_UTC_ <- as.POSIXct(aqua_limits_sf$Date_Time_UTC_)

aqua_limits_sf <- aqua_limits_sf |> 
  mutate(
    year = year(Date_Time_UTC_),
    month = format(Date_Time_UTC_, "%B"),
    month = factor(month, levels = month.name)
  )


summary_data <- aqua_limits_sf %>%
  group_by(Location_ID, Location_Name, year, month, geometry) %>%
  summarise(
    mean_temp = mean(Value, na.rm = TRUE),
    min_temp = min(Value, na.rm = TRUE),
    max_temp = max(Value, na.rm = TRUE),
    sd_temp   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  st_as_sf()


year_choices <- sort(unique(summary_data$year))
month_choices <- levels(summary_data$month)

ui <- fluidPage(
  titlePanel("Monthly Water Temperature Summary by Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = year_choices, selected = max(year_choices)),
      selectInput("month", "Select Month:", choices = month_choices, selected = "January")
    ),
    mainPanel(
      leafletOutput("temp_map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    summary_data |> 
      filter(year == input$year, month == input$month)
  })
  
  output$temp_map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles("CartoDB.Positron") |> 
      setView(lng = -122, lat = 53, zoom = 6)
  })
  
  observe({
    leafletProxy("temp_map", data = filtered_data()) |> 
      clearMarkers() |> 
      addCircleMarkers(
        radius = 5,
        color = "darkblue",
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = popupTable(
          filtered_data(),
          zcol = c("Location_Name", "year", "month", "mean_temp", "min_temp", "max_temp", "sd_temp"),
          feature.id = FALSE,
          row.numbers = FALSE
        )
      )
  })
}

shinyApp(ui = ui, server = server)
