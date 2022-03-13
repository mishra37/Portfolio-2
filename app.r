library(lubridate)
library(feasts)
library(stringr)
library(tidyverse)
library(tsibble)
library(shiny)
library(plotly)


shooting_2018 <- read_csv("https://raw.githubusercontent.com/mishra37/Portfolio-2/main/shootings_2018.csv")
shooting_2019 <- read_csv("https://raw.githubusercontent.com/mishra37/Portfolio-2/main/shootings_2019.csv")
shooting_2020 <- read_csv("https://raw.githubusercontent.com/mishra37/Portfolio-2/main/shootings_2020.csv")
shooting_2021 <- read_csv("https://raw.githubusercontent.com/mishra37/Portfolio-2/main/shootings_2021.csv")

combined_data <- rbind(shooting_2018, shooting_2019, shooting_2020, shooting_2021) 
combined_data <- combined_data %>%
  mutate(Date = dmy(Date)) %>%
  mutate(year = year(Date)) %>%
  filter(!(State %in% c("District of Columbia", "Washington D.C.", "Washington, D.C.", "Puerto Rico", "United States Virgin Islands")))  %>%
  mutate(case_type = case_when(
    str_detect(Description, "party|club") ~ "Party/Club",
    str_detect(Description, "home|residence|apartment|neighborhood") ~ "Residential Areas",
    str_detect(Description, "restaurant|shopping|gas|park|drive-by|parking|store") ~ "Public Areas",
    T ~ "Others"
  )
  )


pca <- function(final_pca_data){
  
  p <- ggplot(final_pca_data , aes(x = .fittedPC1, y = .fittedPC2)) +
    geom_point(aes(text = paste("State: ", State, "\nTotal Dead: ", `Total Dead`, "\nTotal Injured: ", `Total Injured`), col = `Total Casualties`)) +
    scale_alpha(c(1.5, 0.1)) +
    scale_color_viridis_c(option = "turbo", oob=squish) +
    labs(title = "PCA to depict similar shooting trends among the states", x = "PC1", y = "PC2") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 10))
  
  ggplotly(p, tooltip = c("text","Total Casualties"), source = "Let") %>%
    style(hoveron = "fill")%>%
    layout(dragmode = "select")
}


area <- function(data, selected){
  area_data <- data %>%
    fill_gaps()%>%
    mutate(selected_ = State %in% selected) %>%
    mutate(selected_ = as.numeric(selected_)) %>%
    mutate(selected_ = case_when(
      selected_ == 0 ~ 0.3, 
      T ~ 1
    )) %>%
    mutate(total_cas = replace_na(total_cas, 0)) %>%
    mutate(total_dead = replace_na(total_dead, 0)) %>%
    mutate(total_injured = replace_na(total_injured, 0)) 
  area_data %>%
    ggplot() + 
    geom_area(aes(month, total_cas, group = State, fill = "Total Casualties", alpha = selected_)) + 
    geom_area(aes(month, total_injured, group = State, fill = "Total Injured", alpha = selected_))+
    geom_area(aes(month, total_dead, group = State, fill = "Total Dead", alpha = selected_)) +
    scale_alpha(range = c(min(area_data$selected_, na.rm = T), max(area_data$selected_, na.rm = T)), guide = 'none')+
    labs(title = paste0("Breakdown of Total shooting cases into deaths and injuries per month\n"), x = "Month",y = "Total Cases") +
    scale_fill_manual(name = "Cases", values = c("Total Casualties" = "yellow", "Total Injured" = "blue", "Total Dead" = "red"), 
                      breaks = c("Total Casualties", "Total Injured", "Total Dead")) +
    scale_x_discrete(expand = expansion(0)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

state_map <- map_data("state")

map_data <- state_map %>%
  mutate(State = str_to_title(region)) %>%
  filter(State != "District Of Columbia") 


map <- function(data, selected){
  map_data <- data %>%
    mutate(selected_ = State %in% selected) %>%
    mutate(selected_ = as.numeric(selected_)) %>%
    mutate(selected_ = case_when(
      selected_ == 0 ~ 0.3, 
      T ~ 1
    )) %>%
    group_by(State, selected_) %>%
    summarise(total_cas = sum(Total)) %>%
    right_join(map_data)
  
  map_data %>%
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = total_cas, alpha = selected_), color = "black") +
    scale_fill_viridis_c("Total Shooting cases", option = "turbo", guide = guide_colorbar(title.position = "top"))+
    scale_alpha(range = c(min(map_data$selected_, na.rm = T), max(map_data$selected_, na.rm = T)), guide = 'none')+
    coord_map("albers",  lat0 = 45.5, lat1 = 29.5) +
    labs(x = NULL, y = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      legend.key.height = unit(0.5, 'cm'), 
      legend.key.width = unit(1.5, 'cm'),
      legend.title.align = 0.5,
      legend.position = "top",
      legend.title = element_text(size = 12, vjust = 1, face = "bold"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank()) 
  
}

ui <- fluidPage(
  titlePanel(h1("Mass Shooting in the US (2018-2022)", align = "center")),
  sidebarPanel(
    titlePanel(h4("Instructions:", align = "center")),
    titlePanel(h5("Drag the slider to observe changes in shooting cases per year", align = "center")),
    titlePanel(h5("Hover on points in the scatter plot to see total, deaths and injured cases per state", align = "center")),
    titlePanel(h5("Click on points to see breakdown of Total cases into deaths and injured cases per state in area plot", align = "center")), width = 1.5
  ),
  sliderInput("year", "Year", min = 2018,max = 2021, value=2019, sep = ""),
  plotOutput("map_plot"),
  fluidRow(
    column(6,plotOutput("area_plot"), height = 500),
    column(6, plotlyOutput("scatter_plot"), height = 500)
  ),
  dataTableOutput("table")
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    combined_data %>%
      filter(year == as.character(input$year))
  })
  
  
  tsibble_data <- reactive({
    as_tsibble(filtered_data() %>%
                 mutate(month = month(Date, label = T, abbr = T)) %>% 
                 group_by(month, State) %>%
                 summarise(total_cas = sum(Total),
                           total_dead = sum(Dead),
                           total_injured = sum(Injured)), index = month, key = State)
  })
  
  pca_data <- reactive({
    time_series_data <- tsibble_data() %>%
      features(total_cas, features = feature_set(tags = "trend"))
    time_series_data [is.na(time_series_data )] <- 0
    pca_data <- time_series_data[, -c(1, 2)]  %>%
      select(where(~ any(. != 0))) %>%
      prcomp(scale = TRUE) %>%
      augment(time_series_data)
    aggregated_full_data <- filtered_data() %>%
      group_by(State) %>%
      summarise(`Total Dead` = sum(Dead),
                `Total Injured` = sum(Injured), 
                `Total Casualties` = sum(Total))
    final_pca_data <- inner_join(aggregated_full_data, pca_data, by = "State")
    final_pca_data
  })
  
  selected <- reactive({
    
    d <- event_data("plotly_click", source = "Let")
    if (is.null(d)) {
      combined_data %>% pull(State)
    } else {
      x <- round(d$x,4)
      y <- round(d$y, 4)
      
      states <- pca_data()%>%
        mutate(.fittedPC1 = round(.fittedPC1, 4))%>%
        mutate(.fittedPC2 = round(.fittedPC2, 4)) %>%
        filter((x == .fittedPC1 ) & (y ==.fittedPC2 )) %>%
        pull(State)
      
      states
    }
  })
  
  output$map_plot <- renderPlot({
    map(filtered_data(), selected())
  })
  
  output$area_plot <- renderPlot({
    area(tsibble_data(), selected())
  })
  
  output$scatter_plot <- renderPlotly({
    pca(pca_data())
  })
  
  
  output$table <- renderDataTable({
    filtered_data() %>%
      filter(State %in% selected()) %>%
      group_by(State, case_type) %>%
      summarize(count = sum(Total)) %>%
      mutate(Total = sum(count)) %>% 
      arrange(-Total) %>%
      pivot_wider(names_from = "case_type", values_from = "count", values_fill = 0) %>%
      select(-Others)
  })
  
  
  
}

shinyApp(ui, server)
