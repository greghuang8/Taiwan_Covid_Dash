library(shiny)
library(tidyverse)

# Load in tables
full_stats <- read.csv("Full_statistics.csv", header = TRUE,
                       stringsAsFactors = FALSE)

daily_change <- read.csv("daily_changes/Daily_change_June_14.csv", 
                         header = TRUE, stringsAsFactors = FALSE) %>%
  na_if(0) %>%
  select(-X) 
colnames(daily_change) = c("Date", "Deaths Previously Reported",
                           "Deaths Reported Today", "Updated Total Deaths")

today_only <- daily_change %>%
  filter(Deaths_reported_today != 0)

# Basic plots
daily_change %>%
  gather(Category, Deaths, c("Deaths Previously Reported",
                             "Deaths Reported Today"))%>%
  ggplot(aes(x = Date, y = Deaths , fill = Category, label = Deaths)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE))+
  geom_text(size = 3, position = position_stack(vjust=0.6, reverse = TRUE))+
  labs(title = "Taiwan Covid Deaths",
       subtitle = "Updated June 14th, 2021")
           

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID Deaths in Taiwan"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)