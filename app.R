library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(Cairo)
library(shinythemes)
options(shiny.usecairo=T)


#data_import <- read_csv("https://data.nashville.gov/resource/emcy-s884.csv")
data_import <- read_csv("metro_arts_grants.csv")
#data_import <- read_csv("https://raw.githubusercontent.com/hymelam/interactive-metro_art_grant/master/metro_arts_grants.csv")

dat <- data_import %>% 
  select(-totals) %>% 
  gather(source, amount, -organization) %>% 
  separate(source, 
           into = c("source", "year", "year_end"), 
           sep="_(?=[:digit:])", 
           extra="merge") %>% 
  mutate(
    year = as.integer(year),
    year_end = as.integer(year_end),
    year_end = as.integer(ifelse(year_end >=89, year_end+1900, year_end+2000)),
    source = ifelse(source == "metro_arts_abc", "metro", source),
    organization = str_split(organization, " \\(", simplify = TRUE)[,1]
  )

# Define UI -----
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Nashville Metro Arts Grant History (1988-2017)"),
  
  hr(),
  
  # Row 1: 2 columns
  fluidRow(
    column(4,
           
           wellPanel(sliderInput("year_range",
                                 label = "Year filter:",
                                 min = 1988, max = 2017, step = 1, value = c(0,2017),
                                 sep=""))  
    ),
    column(8, 
           mainPanel(
             "This visualization summarizes historical data of organizations and projects funded
by Nashville Metro Arts through grants and micro awards. Use the slider to the left to filter the data displayed in the graph.", br(),br(),"The dataset was downloaded from the Nashville Open Data Portal (", tags$a(href="https://data.nashville.gov/Art/Metro-Arts-Grant-History/emcy-s884", "link"), ")."
             )  
    )
  ),
  
  # horizontal line
  hr(),
  
  # Row 2: 1 full screen column
  mainPanel(plotOutput("main_plot"))
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  title_text <- reactive({
    paste("Amount awarded to the top 10 grant recipients:",
          
          ifelse(input$year_range[1] == input$year_range[2], 
                 paste(input$year_range[1]), 
                 paste(input$year_range[1], "to", input$year_range[2]))
          )
  })
  
  output$main_plot <- renderPlot({

    dat %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>% 
      group_by(organization) %>% 
      summarize(amount = sum(amount)) %>% 
      ungroup() %>% 
      top_n(10, amount) %>% 
      mutate(organization = fct_reorder(organization, amount)) %>% 
      ggplot(aes(organization, amount)) +
      geom_col(color="gray35", fill="#4281CA") + # Higher gray##s are lighter
      scale_y_continuous(labels = dollar,
                         expand = c(0,0),
                         breaks = pretty_breaks(n=10)) +
      coord_flip() +       
      theme_light(base_size = 14) +
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
      labs(
        title = title_text(),
        x = element_blank(),
        y = element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

