#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyverse)
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(nflfastR)

totalinj <- read.csv("injuries_by_stadium_sum.csv")
raw_data <- read.csv("injuries_by_stadium_edit.csv")
injury_long <- raw_data %>%
  pivot_longer(
    cols = -stadium,           
    names_to = "injury_type", 
    values_to = "count"        
  )
write.csv(injury_long, "injuries_long_format.csv", row.names = FALSE)
seasons <- 2010:2020
pbp <- load_pbp(seasons)
stadium_mapping <- tribble(
  ~stadium, ~season, ~unified_venue, ~surface,
  "Edward Jones Dome", 1995, "Edward Jones", "Turf",
  "Acrisure Stadium", 2001, "Acrisure", "Grass",
  "MetLife Stadium", 2010, "MetLife", "Turf",
  "Gillette Stadium", 2002, "Gillette", "Turf",
  "Raymond James Stadium", 1998, "Raymond James", "Grass",
  "Commanders Field", 1997, "FedEx", "Grass",
  "EverBank Stadium", 1995, "EverBank", "Grass",
  "Soldier Field", 1924, "Soldier", "Grass",
  "Lincoln Financial Field", 2003, "Lincoln Financial", "Grass",
  "NRG Stadium", 2002, "NRG", "Turf",
  "Highmark Stadium", 1973, "Highmark", "Grass",
  "Caesars Superdome", 1975, "Caesars", "Turf",
  "Nissan Stadium", 1999, "Nissan", "Grass",
  "GEHA Field at Arrowhead Stadium", 1972, "GEHA", "Grass",
  "Lumen Field", 2002, "Lumen", "Turf",
  "Georgia Dome", 1992, "Georgia", "Turf",
  "Paycor Stadium", 2000, "Paycor", "Turf",
  "Lambeau Field", 1957, "Lambeau", "Grass",
  "AT&T Stadium", 2009, "AT&T", "Turf",
  "Qualcomm Stadium", 1967, "Qualcomm", "Grass",
  "Cleveland Browns Stadium", 1999, "Cleveland Browns", "Grass",
  "Mall of America Field", 1982, "Mall of America", "Turf",
  "Candlestick Park", 1960, "Candlestick", "Grass",
  "Lucas Oil Stadium", 2008, "Lucas Oil", "Turf",
  "Ford Field", 2002, "Ford", "Turf",
  "Empower Field at Mile High", 2001, "Mile High", "Grass",
  "Oakland-Alameda County Stadium", 1966, "Oakland-Alameda County", "Grass",
  "Bank of America Stadium", 1996, "Bank of America", "Grass",
  "M&T Bank Stadium", 1998, "M&T Bank", "Grass",
  "Hard Rock Stadium", 1987, "Hard Rock", "Grass",
  "State Farm Stadium", 2006, "State Farm", "Grass",
  "Wembley Stadium", 2007, "Wembley", "Grass",
  "Rogers Centre", 1989, "Rogers", "Turf",
  "TCF Bank Stadium", 2009, "TCF Bank", "Turf",
  "Levi's Stadium", 2014, "Levi's", "Grass",
  "U.S. Bank Stadium", 2016, "U.S. Bank", "Turf",
  "Los Angeles Memorial Coliseum", 1923, "Memorial Coliseum", "Grass",
  "Twickenham Stadium", 1909, "Twickenham", "Grass",
  "Estadio Azteca", 1966, "Azteca", "Grass",
  "Mercedes-Benz Stadium", 2017, "Mercedes-Benz", "Turf",
  "ROKiT Field - Dignity Health Sports Park", 2003, "ROKiT Field", "Grass",
  "Tottenham Hotspur Stadium", 2019, "Tottenham Hotspur", "Grass",
  "SoFi Stadium", 2020, "SoFi", "Turf",
  "Allegiant Stadium", 2020, "Allegiant", "Grass"
)

stadium_stats <- pbp %>%
  left_join(
    stadium_mapping %>% dplyr::select(stadium, unified_venue),
    by = "stadium"
  ) %>%
  dplyr::mutate(
    unified_venue = tidyr::replace_na(unified_venue, "Unknown"),
    surface = dplyr::case_when(
      surface == "grass"                                         ~ "Grass",
      surface %in% c("astroplay", "fieldturf", "sportturf", 
                     "matrixturf", "a_turf", "astroturf")       ~ "Turf",
      TRUE                                                       ~ "Unknown"
    )
  ) %>%
  dplyr::filter(!is.na(yards_gained)) %>%
  dplyr::group_by(unified_venue, surface, game_id) %>%
  dplyr::summarise(total_yards = sum(yards_gained, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(unified_venue, surface) %>%
  dplyr::summarise(
    avg_yards = mean(total_yards),
    n_games   = n(),
    .groups   = "drop"
  ) %>%
  dplyr::filter(n_games > 5)
surface_summary <- stadium_stats %>%
  
  dplyr::filter(surface %in% c("Grass", "Turf")) %>%
  
  dplyr::group_by(surface) %>%
  
  dplyr::summarise(
    
    avg_yards  = mean(avg_yards),
    
    total_games = sum(n_games),
    
    .groups = "drop"
    
  ) 



#UI
ui <- fluidPage(
  titlePanel("NFL Stadium Visualization Project"),
  p(
    "data sourced from: ", 
    tags$a(href = "https://nflverse.nflverse.com/", "nflverse"), 
    " & ", 
    tags$a(href = "https://nflfastr.com/", "nflfastr")
  ),
  p("For some more info on the topic visit:",
    tags$a(href ="https://pmc.ncbi.nlm.nih.gov/articles/PMC11567718/","here")),
  
  tabsetPanel(
    tabPanel("Stadium Map", 
             
             p("We start our journey looking at the current active stadiums of the NFL"),
             p("Click on marker to see if grass or turf"),
             fluidRow(
               column(12, leafletOutput("map1", height = "600px"))
             )
    ),
    tabPanel("Injury Heatmap", h3("Injury Frequency"),
             p("We then see the total amount of injuries from 2010-2020, notable outliers are metlife and Lucas oil, both are turf fields"),
             sidebarPanel(
               width = 3,
               textInput("stadium_search_inj", "Search Stadium:", placeholder = "e.g. MetLife, Lambeau..."),
               helpText("Leave blank to show all stadiums."),
               helpText("Also type grass or turf to show all staduims with that field type"),
               hr(),
               helpText("Filter by searching for specific venues.")
             ),
             fluidRow(
               column(12, plotlyOutput("map2", height = "600px"))
             )
    ),
    tabPanel("Injury Scatterplot", h3("Injury Distribution"),
             p("The scatterplot shows that both knee and ankle injuries are the most common irregardless of field type"),
             sidebarPanel(
               width = 3,
               textInput("stadium_search_inj", "Search Stadium:", placeholder = "e.g. MetLife, Lambeau..."),
               helpText("Leave blank to show all stadiums."),
               helpText("Also type grass or turf to show all staduims with that field type"),
               hr(),
               helpText("Filter by searching for specific venues.")
             ),
             fluidRow(
               column(12, plotlyOutput("scatterplot_out", height = "500px"))
             )
    ),
    tabPanel("Stadium injuries", p("Total amounts of injuries show that metlife is definitely cursed"),
             sidebarPanel(
               width = 3,
               textInput("stadium_search_inj2", "Search Stadium:", placeholder = "e.g. MetLife, Lambeau..."),
               helpText("Leave blank to show all stadiums."),
               helpText("Also type grass or turf to show all staduims with that field type"),
               hr(),
               helpText("Filter by searching for specific venues.")
             ),
             fluidRow(
               column(12, plotlyOutput("map3", height = "600px"))
             )
    ),
    
    tabPanel("Analytics Dashboard",
             p("Finally I wanted to show that there is no real difference in play of turf vs grass"),
             p("Many players have said publically that they prefer grass to turf however the stats show that there is no significant difference in play between the two"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 textInput("stadium_search", "Search Stadium:", placeholder = "e.g. MetLife, Lambeau..."),
                 helpText("Leave blank to show all stadiums."),
                 hr(),
                 helpText("Filter by searching for specific venues.")
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(12, plotlyOutput("yard_plot", height = "600px"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("Turf vs Grass: Overall Average Yards"),
                          plotlyOutput("surface_compare", height = "250px"))
                 )
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # Map of staduims
  output$map1 <- renderLeaflet({
    mymap <- leaflet() %>%
      addTiles() %>%
      
      addMarkers( lat=39.0489, lng=-94.4840, label="Arrowhead Stadium (Chiefs)", popup="Natural Grass")%>%
      
      addMarkers( lat=39.7439, lng=-105.0201, label="Empower Field at Mile High (Broncos)", popup="Natural Grass")%>%
      
      addMarkers( lat=30.3239, lng=-81.6374, label="EverBank Stadium (Jaguars)", popup="Natural Grass")%>%
      
      addMarkers( lat=42.0909, lng=-71.2643, label="Gillette Stadium (Patriots)", popup="Artificial Turf")%>%
      
      addMarkers( lat=42.7737, lng=-78.7870, label="Highmark Stadium (Bills)", popup="Natural Grass")%>%
      
      addMarkers( lat=41.5060, lng=-81.6996, label="Huntington Bank Field (Browns)", popup="Natural Grass")%>%
      
      addMarkers( lat=39.7601, lng=-86.1638, label="Lucas Oil Stadium (Colts)", popup="Artificial Turf")%>%
      
      addMarkers( lat=39.2780, lng=-76.6228, label="M&T Bank Stadium (Ravens)", popup="Natural Grass")%>%
      
      addMarkers( lat=36.1665, lng=-86.7712, label="Nissan Stadium (Titans)", popup="Artificial Turf")%>%
      
      addMarkers( lat=29.6847, lng=-95.4110, label="NRG Stadium (Texans)", popup="Artificial Turf")%>%
      
      addMarkers( lat=39.0954, lng=-84.5160, label="Paycor Stadium (Bengals)", popup="Artificial Turf")%>%
      
      addMarkers( lat=32.7478, lng=-97.0928, label="AT&T Stadium (Cowboys)", popup="Artificial Turf")%>%
      
      addMarkers( lat=35.2258, lng=-80.8528, label="Bank of America Stadium (Panthers)", popup="Artificial Turf")%>%
      
      addMarkers( lat=29.9509, lng=-90.0813, label="Caesars Superdome (Saints)", popup="Artificial Turf")%>%
      
      addMarkers( lat=42.3401, lng=-83.0458, label="Ford Field (Lions)", popup="Artificial Turf")%>%
      
      addMarkers( lat=25.9580, lng=-80.2389, label="Hard Rock Stadium (Dolphins)", popup="Natural Grass")%>%
      
      addMarkers( lat=44.5013, lng=-88.0621, label="Lambeau Field (Packers)", popup="Hybrid Grass (reinforced with synthetic fibers) ")%>%
      
      addMarkers( lat=37.4030, lng=-121.9700, label="Levi's Stadium (49ers)", popup="Natural Grass")%>%
      
      addMarkers( lat=39.9008, lng=-75.1675, label="Lincoln Financial Field (Eagles)", popup="Natural Grass")%>%
      
      addMarkers( lat=47.5952, lng=-122.3316, label="Lumen Field (Seahawks)", popup="Artificial Turf")%>%
      
      addMarkers( lat=33.7555, lng=-84.4009, label="Mercedes-Benz Stadium (Falcons)", popup="Artificial Turf")%>%
      
      addMarkers( lat=40.8122, lng=-74.0769, label="MetLife Stadium (Giants/Jets)", popup="Artificial Turf")%>%
      
      addMarkers( lat=38.9077, lng=-76.8645, label="Northwest Stadium (Commanders)", popup="Natural Grass")%>%
      
      addMarkers( lat=27.9759, lng=-82.5033, label="Raymond James Stadium (Buccaneers)", popup="Natural Grass")%>%
      
      addMarkers( lat=33.9535, lng=-118.3395, label="SoFi Stadium (Rams/Chargers)", popup="Artificial Turf")%>%
      
      addMarkers( lat=41.8623, lng=-87.6167, label="Soldier Field (Bears)", popup="Natural Grass")%>%
      
      addMarkers( lat=33.5277, lng=-112.2626, label="State Farm Stadium (Cardinals)", popup="Natural Grass")%>%
      
      addMarkers( lat=44.9738, lng=-93.2581, label="U.S. Bank Stadium (Vikings)", popup="Artificial Turf")
  })
  #HEATMAP
  filtered_data_inj <- reactive({
    search_term <- input$stadium_search_inj

    df <- injury_long

    if (nchar(search_term) > 0) {
      df <- df %>% filter(grepl(search_term, stadium, ignore.case = TRUE))
    }
    
    return(df)
  })
  output$map2 <- renderPlotly({
    data_to_plot <- filtered_data_inj()
    print("Columns available to plot_ly:")
    print(colnames(data_to_plot))
    plot_ly(
      data = filtered_data_inj(), 
      x = ~injury_type,
      y = ~stadium,
      z = ~count, 
      type = "heatmap",
      colors = "Reds"
    ) %>%
      layout(
        title = paste("Injury Frequency for:", input$stadium_search_inj),
        xaxis = list(title = "Injury Type", tickangle = -45),
        yaxis = list(title = "Stadium"),
        margin = list(l = 150, b = 100) 
      )
  })
  #SCATTER
  output$scatterplot_out <- renderPlotly({
    req(filtered_data_inj()) 
    
    plot_ly(
      data = filtered_data_inj(),
      x = ~injury_type,
      y = ~count,
      color = ~stadium,      
      type = "scatter",
      mode = "markers",      
      marker = list(size = 10, opacity = 0.7)
    ) %>%
      layout(
        title = "Injury Count by Type (Colored by Stadium)",
        xaxis = list(title = "Injury Type", tickangle = -45),
        yaxis = list(title = "Number of Injuries")
      )
  })
  #bar
  filtered_data_inj2 <- reactive({
    search_term <- input$stadium_search_inj2
    
    df <- totalinj
    
    if (nchar(search_term) > 0) {
      df <- df %>% filter(grepl(search_term, stadium, ignore.case = TRUE))
    }
    
    return(df)
  })
  output$map3 <- renderPlotly({
    plot_ly(
      data = filtered_data_inj2(),
      x = ~Sum,
      y = ~reorder(stadium, Sum), 
      type = "bar",
      orientation = 'h',          
      marker = list(color = '#73061a') 
    ) %>%
      layout(
        title = "Total Injuries by Stadium",
        xaxis = list(title = "Total Injury Count"),
        yaxis = list(title = ""), 
        margin = list(l = 150)    
      )
  })
  
  #yards by staduim
  filtered_data <- reactive({
    df <- stadium_stats %>%
      dplyr::filter(unified_venue != "Unknown", surface %in% c("Grass", "Turf"))
    
    if (nchar(trimws(input$stadium_search)) > 0) {
      df <- df %>% dplyr::filter(grepl(input$stadium_search, unified_venue, ignore.case = TRUE))
    }
    
    df %>%
      dplyr::arrange(avg_yards) %>%
      dplyr::mutate(unified_venue = factor(unified_venue, levels = unique(unified_venue)))
  })
  output$yard_plot <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~avg_yards,
      y = ~unified_venue,
      color = ~surface,
      colors = c("Grass" = "darkgreen", "Turf" = "#DB934A"),
      type = "bar",
      orientation = "h",
      text = ~paste0("Avg Yards: ", round(avg_yards, 1)),
      hoverinfo = "text"
    ) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 150), # Add margin for long stadium names
        xaxis = list(title = "Average Yards"),
        yaxis = list(title = ""),
        barmode = "group"
      )
  })
  
  #turf vs grass
  output$surface_compare <- renderPlotly({
    plot_ly(
      data = surface_summary,
      x = ~surface,
      y = ~avg_yards,
      color = ~surface,
      colors = c("Grass" = "darkgreen", "Turf" = "#DB934A"),
      type = "bar"
    ) %>%
      layout(
        autosize = TRUE,
        xaxis = list(title = ""),
        yaxis = list(title = "Avg Yards"),
        showlegend = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)
