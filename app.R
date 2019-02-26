library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(Cairo)

options(shiny.usecairo=T)

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Nashville Metro Arts Grant History"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('app_org_1', 'Organization', dat_orglist),
      selectInput('app_org_2', 'Organization', dat_orglist),
      selectInput('app_org_3', 'Organization', dat_orglist)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  app_orgs <- reactive({
    unique(c(input$app_org_1, input$app_org_2, input$app_org_3))
  })
  
   output$main_plot <- renderPlot({
     dat %>% 
       filter(organization %in% app_orgs()) %>% 
       group_by(year, organization) %>% 
       summarize(amount = sum(amount)) %>% 
       ungroup() %>% 
       ggplot(aes(year, amount, color=organization)) +
       geom_line() +
       scale_y_continuous(labels = dollar) +
       scale_x_continuous(expand = c(0, 0.3), 
                          breaks = c(seq(1988, 2017, 3), 2017), # From 1988 to 2017
                          limits = c(1988, 2017)) + # 2018 to add padding
       theme_light(base_size = 14)+
       theme(legend.position="bottom") 
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

