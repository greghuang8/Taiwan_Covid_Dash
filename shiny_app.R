library(shiny)
library(tidyverse)

# Load in tables
full_stats <- read.csv("Full_statistics.csv", header = TRUE,
                       stringsAsFactors = FALSE)

daily_change <- read.csv("daily_changes/Daily_change_June_13.csv", 
                         header = TRUE, stringsAsFactors = FALSE)

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