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
      Count = as.numeric(str_extract(Count, "\\d+"))
    ) %>%
    arrange(Date) |> 
    mutate(
      Month = factor(month(Date, label = TRUE, abbr = TRUE), 
                     levels = month.abb,
                     ordered = TRUE)
    ) |> 
    filter(!is.na(Latitude) & !is.na(Longitude) & !is.na(Count))

levels(data$Month)

