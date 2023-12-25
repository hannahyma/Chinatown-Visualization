# Load packages
library(shiny)
library(sf)
library(tidyverse)
library(skimr)
library(patchwork)
library(tigris)
options(tigris_use_cache = TRUE)

places_2021 <- read_csv("PLACES_2022.csv") %>% 
  janitor::clean_names()

# combine survey data with zctas map of IL from tigris
il_map <- zctas(state = "IL", year = 2010) %>% 
  janitor::clean_names() %>%
  left_join(places_2021, by = c("zcta5ce10" = "location_name")) %>% 
  filter(category == "Health Outcomes" & zcta5ce10 %in% c(60600:60700)) %>% 
  select(zcta5ce10, year, category, measure, data_value, 
         data_value_type, low_confidence_limit, high_confidence_limit, 
         total_population, geolocation, location_id, short_question_text, 
         geometry) %>% 
  arrange(short_question_text)
########################################################
# rsconnect::setAccountInfo(name='9iuyod-hannah-ma',
#                           token='6DCB8D5A13B6ACED5F148E4FF95E41E0',
#                           secret='FVKzByrNL8DneexD9acH/9ocYT2lsII/xVLoaf74'
#                           )
# 
# library(rsconnect)
# rsconnect::deployApp("C:/Users/hanyu/Documents/302 STAT/Final Presentation/Map Dashboard")
########################################################
ui <- fluidPage(
  titlePanel("Chicago Health Outcomes by Zipcode"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create heat maps of disease prevalence within Chicagoland with 
                    information from PLACES Survey Data, 2022 release"),
      selectInput("measure",
                  "Choose the Disease of Interest:",
                  choices = c(il_map$short_question_text),
                  selected = "temp"
      )
    ),
    mainPanel(
      plotOutput("il_plot")
    )
  )
)
########################################################
server <- function(input, output) {
  
  output$il_plot <- renderPlot({
    
    # create dataset that only includes selected disease statistics for filling map
    il_map_fill <- il_map %>%
      filter(short_question_text == input$measure)
    
    # plot code
    ggplot(il_map_fill, aes(fill = data_value)) +
      geom_sf(color = "black") +
      scale_fill_gradient2(
        name = "% Crude\nPrevalence",
        low = "white", 
        high = "#BB1818",
        breaks = c(0, 25, 50),
        limits = c(0, 50),
        labels = c("0", "25",  "50")
      ) +
      theme_void()
    
  },
  width = 500,
  height = 700
  )
}
########################################################
shinyApp(ui = ui, server = server)