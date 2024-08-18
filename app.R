# app.R

# Load required libraries
library(shiny)
library(reactable)
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


# Enable thematic
thematic::thematic_shiny(font = "auto")

# UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "zephyr",
    primary = "#ee561f",
    secondary = "#82cae8",
    base_font = font_google("Lato"),
    input_btn_padding_x = ".25rem",
    input_btn_padding_y =  ".25rem",
    card_cap_padding_x = "2",
    card_cap_padding_y = "2",
    nav_tabs_link_active_color = "#ee561f"
  ),
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
      textOutput("above_33"),
      br(),
      textOutput("east_94"),
      br(),
      tags$p("Data source: ", tags$a("Journey North", href = "https://journeynorth.org/", target = "_blank")),
      tags$p(tags$a("App by Jeremy Allen", href = "https://github.com/jeremy-allen/monarchs.git")),
      tags$p(tags$a("Powered by Shiny for R", href = "https://shiny.posit.co/"))
         
    ),
    navset_card_tab(
      nav_panel("Map",
        card_body(
          class = "p-0",
          leafletOutput("map")
        )
      ),
      nav_panel("Data",
        card_body(
          reactableOutput("table")
        )
      ),
      full_screen = TRUE
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

  above_33 <- reactive({
    count <- filtered_data() %>%
      filter(Latitude > 33) |> 
      nrow()

    prop <- (count/nrow(filtered_data()))*100

    return(prop)
  })

  east_94 <- reactive({
    count <- filtered_data() %>%
      filter(Longitude > -94.0) |> 
      nrow()

    prop <- (count/nrow(filtered_data()))*100

    return(prop)
  })

  output$above_33 <- renderText({
    paste0(round(above_33(), 2), "% of these sightings are north of 33°N")
  })
  
  output$east_94 <- renderText({
    paste0(round(east_94(), 2), "% of these sightings are east of -94°E")
  })

  # Render the data table
  output$table <- renderReactable({
    reactable(filtered_data(),
    filterable = TRUE,
    defaultPageSize = 12,
    minRows = 12,)
  })
  
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
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolylines(
        lng = c(-94.0, -94.0),
        lat = c(-90, 90),
        stroke = TRUE,
        color = "#82cae8",
        weight = 2,
        opacity = 0.3) |> 
      addPolylines(
        lng = c(0, -360),
        lat = c(33, 33),
        stroke = TRUE,
        color = "#82cae8",
        weight = 2,
        opacity = 0.3) |> 
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = ~sqrt(Count) * 2,  # Adjust the multiplier for better visibility
        popup = ~paste("Date:", Date, "<br>",
                        "Month:", Month, "<br>",
                       "Location:", City, ", ", State, "<br>",
                       "Latitude:", Latitude, "<br>",
                       "Longitude:", Longitude, "<br>",
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
