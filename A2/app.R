library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(stringr)
library(here)
library(treemapify)


# Load the data in computer
# data <- read.csv("C:/Users/1234d/Documents/es/assignment2/cleaned_Canine_Waste_Dispensers.csv")
data <- read.csv("cleaned_Canine_Waste_Dispensers.csv")




# Extract Longitude and Latitude from the 'point' column
data <- data %>%
  mutate(
    Longitude = as.numeric(str_extract(point, "(?<=\\().+?(?=\\s)")),
    Latitude = as.numeric(str_extract(point, "(?<=\\s)-?\\d+\\.\\d+(?=\\))")),
    InstallationDate = as.Date(InstallationDate, format = "%Y-%m-%d")
  )

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Canine Waste Dispensers Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Distribution", tabName = "distribution", icon = icon("chart-bar")),
      
      # Controls
      dateRangeInput("dateRange", "Select Installation Date Range", 
                     start = min(data$InstallationDate, na.rm = TRUE), 
                     end = max(data$InstallationDate, na.rm = TRUE)),
      
      selectInput("borough", "Select Borough", 
                  choices = c("All", unique(data$Borough)), 
                  selected = "All"),
      
      selectInput("mountingSurface", "Select Mounting Surface", 
                  choices = c("All", unique(data$MountingSurface)), 
                  selected = "All"),
      
      sliderInput("zipRange", "Select ZIP Code Range", 
                  min = min(data$ZIPCode, na.rm = TRUE), 
                  max = max(data$ZIPCode, na.rm = TRUE), 
                  value = c(min(data$ZIPCode, na.rm = TRUE), max(data$ZIPCode, na.rm = TRUE))),
      
      sliderInput("radius", "Map Point Radius", 
                  min = 1, max = 20, value = 5)
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "map",
              box(title = "Map of Dispensers", width = 12,
                  leafletOutput("map", height = 800))
      ),
      
      tabItem(tabName = "trends",
              box(title = "Installation Trends Over Time", width = 12,
                  plotOutput("timePlot"))
      ),
      
      tabItem(tabName = "distribution",
              fluidRow(
                box(title = "Borough Distribution", width = 6,
                    plotOutput("boroughPlot")),
                box(title = "ZIP Code Distribution", width = 6,
                    plotOutput("zipPlot"))
              ),
              fluidRow(
                box(title = "Mounting Surface Distribution", width = 6,
                    plotOutput("mountingPlot")),
                box(title = "Location Type Distribution", width = 6,
                    plotOutput("locationPlot"))
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive data based on user inputs
  filteredData <- reactive({
    data %>%
      filter(
        (is.na(InstallationDate) | 
           (InstallationDate >= input$dateRange[1] & InstallationDate <= input$dateRange[2])),
        
        (input$borough == "All" | Borough == input$borough),
        
        (input$mountingSurface == "All" | MountingSurface == input$mountingSurface),
        
        ZIPCode >= input$zipRange[1] & ZIPCode <= input$zipRange[2]
      )
  })
  
  # Map Visualization
  output$map <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, 
        radius = input$radius,
        color = ~Borough, 
        popup = ~paste("Location:", Location, "<br>",
                       "Dispenser Type:", DispenserUnitLocation)
      )
  })
  
  # Installation Trends
  output$timePlot <- renderPlot({
    filteredData() %>%
      filter(!is.na(InstallationDate)) %>%
      count(InstallationDate) %>%
      ggplot(aes(x = InstallationDate, y = n)) +
      geom_area(fill = "skyblue", alpha = 0.6) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Installation Trends Over Time", x = "Date", y = "Number of Installations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Borough Distribution
  output$boroughPlot <- renderPlot({
    filteredData() %>%
      filter(!is.na(Borough)) %>%
      count(Borough) %>%
      ggplot(aes(x = Borough, y = n)) +
      geom_bar(stat = "identity") +
      labs(title = "Dispenser Distribution by Borough", x = "Borough", y = "Count")
  })
  
  # ZIP Code Distribution 
  output$zipPlot <- renderPlot({
    filteredData() %>%
      count(ZIPCode) %>%
      ggplot(aes(x = factor(ZIPCode), y = n, group = 1)) +
      geom_line(aes(color = n), size = 1.2) +  
      scale_color_gradient(low = "lightblue", high = "darkblue") + 
      labs(title = "ZIP Code Distribution", x = "ZIP Code", y = "Count") +
      theme_minimal() +
      theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Mounting Surface Distribution
  output$mountingPlot <- renderPlot({
    filteredData() %>%
      filter(!is.na(MountingSurface)) %>%
      count(MountingSurface) %>%
      ggplot(aes(x = "", y = n, fill = MountingSurface)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Distribution of Mounting Surfaces", fill = "Mounting Surface")
  })
  
  # Location Type Distribution
  output$locationPlot <- renderPlot({
    filteredData() %>%
      count(DispenserUnitLocation) %>%
      ggplot(aes(area = n, fill = DispenserUnitLocation, label = DispenserUnitLocation)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "center", grow = TRUE) +
      labs(title = "Location Type Distribution") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
