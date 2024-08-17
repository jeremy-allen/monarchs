# app.R

# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)
library(RColorBrewer)
library(bslib)
library(thematic)

# Function to scrape and process data
get_monarch_data <- function() {
  url <- "https://journeynorth.org/sightings/querylist.html?season=spring&map=monarch-adult-spring&year=2024&submit=View+Data"
  page <- read_html(url)
  
  data <- page %>%
    html_table() %>%
    .[[1]] |> 
    select(-1, -Image) %>%
    rename(
      City = Town,
      State = `State/Province`,
      Count = Number
    ) %>%
    mutate(
      Date = mdy(Date),
      Latitude = as.numeric(str_extract(Latitude, "\\d+\\.\\d+")),
      Longitude = as.numeric(str_extract(Longitude, "-?\\d+\\.\\d+")),
      Count = as.numeric(str_extract(Count, "\\d+")),
      Month = factor(
        month(Date, label = TRUE, abbr = TRUE),
        levels = month.abb,
        labels = month.abb,
        ordered = TRUE
      )
    ) %>%
    arrange(Date)|> 
    filter(!is.na(Latitude) & !is.na(Longitude) & !is.na(Count))
  
  return(data)
}

dat <- get_monarch_data()
available_months <- unique(dat$Month)
print(available_months)

# Enable thematic
thematic::thematic_shiny(font = "auto")

# UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "morph",
    primary = "#ee561f",
    secondary = "#000000",
    base_font = font_google("Inter")
  ) |> 
    bs_add_rules(sass::sass_file("www/cards.scss")),
  layout_sidebar(
    sidebar = sidebar(
      h5("2024 Adult Monarch Butterfly Sightings"),
      checkboxGroupInput(
        "months",
        NULL,
        choices = month.abb,
        selected = if (length(available_months) >= 3) {
          available_months[1:3]
        } else {
          available_months[1]
        }
      ),
      actionButton(
        "go",
        "Refresh Map"
    ),
      br(),
      tags$p("Data source: ", 
           tags$a("Journey North", href = "https://journeynorth.org/", target = "_blank")),
      tags$p(tags$a("app code by Jeremy Allen", href = ""))     
    ),
    card(
      full_screen = TRUE,
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive expression for filtered data
  filtered_data <- reactive({

    req(length(input$months) > 0)
    req(input$months %in% available_months)

    dat %>%
      filter(
        Month %in% input$months
      )
  }) |> 
    bindEvent(input$go, ignoreNULL = FALSE)
  
  # Render the map
  output$map <- renderLeaflet({

    data <- filtered_data()
    data <- data |> 
      mutate(
        Month = factor(Month, levels = Month, labels = Month, ordered = TRUE)
      )
      

    # Create a color palette for months using Set3
    month_colors <- brewer.pal(12, "Paired")
    month_palette <- colorFactor(
      palette = month_colors,
      levels = levels(data$Month),
      ordered = TRUE
    )
    
    ord <- factor(levels(data$Month), levels = levels(data$Month), ordered = TRUE)

    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%  # Use dark map tiles
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = ~sqrt(Count) * 2,  # Adjust the multiplier for better visibility
        popup = ~paste("Date:", Date, "<br>",
                        "Month:", Month, "<br>",
                       "Location:", City, ", ", State, "<br>",
                       "Count:", Count),
        label = ~as.character(Count),
        color = ~month_palette(Month),
        fillOpacity = 0.5
      ) %>%
        setView(lng = -94.4, lat = 32.7, zoom = 4) |> 
        addLegend(
          position = "bottomright",
          pal = month_palette,
          values = ord,
          title = "Month",
          opacity = .6
        )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
