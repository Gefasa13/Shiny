library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(tidyr)
library(DT)
# 从指定路径加载数据
sri_lanka_data <- read_csv("data/Srilanka.csv")

sri_lanka_data <- sri_lanka_data %>%
  mutate(Year = as.numeric(substr(Year, 1, 4)))
# 预处理数据
sri_lanka_data <- sri_lanka_data %>%
  select(-c(`Country Name`, `Country Code`, `Series Code`))

comparison_data <- read_csv("comp2.csv")

comparison_data <- read_csv("comp2.csv") %>%
  filter(!is.na(`Series.Name`) & `Series.Name` != "") %>%
  mutate(year = as.numeric(year), 
         value = as.numeric(value)) %>%
  drop_na()

# UI part
ui <- navbarPage("Sri Lanka Economic and Demographic Data Analysis",
                 tabPanel("General Overview",
                          fluidPage(
                            h1("General Description of Sri Lanka"),
                            p("Sri Lanka, officially the Democratic Socialist Republic of Sri Lanka, is an island country in South Asia, located in the Indian Ocean to the southwest of the Bay of Bengal and to the southeast of the Arabian Sea."),
                            p("It is separated from the Indian subcontinent by the Gulf of Mannar and the Palk Strait."),
                            p("The country's recent history has been marred by a 30-year civil war which decisively ended when the Sri Lankan military defeated the Liberation Tigers of Tamil Eelam in 2009."),
                            h3("More about Sri Lanka"),
                            p("Sri Lanka's economy is one of the fastest growing in the world, known for the production of tea, coffee, gemstones, and coconuts. The country has a rich cultural heritage and a history that dates back thousands of years."),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Key Facts", 
                                                 dataTableOutput("dataTable"))
                            )
                          )),
                 tabPanel("Map of Sri Lanka",
                          leafletOutput("map", height = "600px")),
                 tabPanel("Economic Data",
                          fluidPage(
                            h2("Economic Data Analysis"),
                            selectInput("selectedIndicator", "Choose an Indicator:",
                                        choices = unique(sri_lanka_data$`Series Name`)),
                            plotOutput("indicatorPlot")
                          )),
                 tabPanel("Comparison Analysis",
                          fluidPage(
                            h2("Regional Comparison Data"),
                            DTOutput("comparisonTable")  # 显示数据表格
                          )),
                 tabPanel("SWOT Analysis",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Strengths", 
                                               h3("Strengths"),
                                               p("Strong tourism sector with diverse attractions: beaches, historical sites, and wildlife."),
                                               p("Robust tea export industry, contributing significantly to the economy.")),
                                      tabPanel("Weaknesses",
                                               h3("Weaknesses"),
                                               p("High levels of government debt affecting fiscal sustainability."),
                                               p("Dependency on imported oil leads to vulnerability in energy sector.")),
                                      tabPanel("Opportunities",
                                               h3("Opportunities"),
                                               p("Growing digital economy and increasing investment in technology."),
                                               p("Potential for development of renewable energy sources due to geographic location.")),
                                      tabPanel("Threats",
                                               h3("Threats"),
                                               p("Geopolitical tensions in the region can affect stability and economic growth."),
                                               p("Natural disasters such as floods and droughts periodically impact agriculture and infrastructure."))
                          ))
                 )

# Server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%  # Default OpenStreetMap tiles
      setView(lng = 80.7718, lat = 7.8731, zoom = 7)  # Center view on Sri Lanka
  })
  
  output$dataTable <- renderDT({
    datatable(sri_lanka_data)
  })
  
  output$popPlot <- renderPlot({
    data <- sri_lanka_data %>% filter(`Series Name` == "Population, total")
    ggplot(data, aes(x = Year, y = Value)) + geom_line() + geom_point()
  })
  
  output$indicatorPlot <- renderPlot({
    data_to_plot <- sri_lanka_data %>%
      filter(`Series Name` == input$selectedIndicator)
    ggplot(data_to_plot, aes(x = Year, y = Value)) +
      geom_line() + geom_point() +
      labs(title = paste(input$selectedIndicator, "in Sri Lanka"), x = "Year", y = "Value")
  })
  output$comparisonTable <- renderDT({
    datatable(comparison_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
