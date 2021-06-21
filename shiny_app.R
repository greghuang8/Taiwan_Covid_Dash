library(shiny)
library(tidyverse)

# Load in tables
full_stats <- read.csv("Full_statistics.csv", header = TRUE,
                       stringsAsFactors = FALSE) %>%
  select(-X)

daily_change <- read.csv("daily_changes/Daily_change_June_21.csv", 
                         header = TRUE, stringsAsFactors = FALSE) %>%
  na_if(0) %>%
  select(-X)
colnames(daily_change) = c("Date", "Deaths Previously Reported",
                           "Deaths Reported Today", "Updated Total Deaths")

daily_change_tw <- daily_change
colnames(daily_change_tw) = c("Date", "死亡案例", "今日回報新增死亡案例",
                              "Updated Total Deaths")

# Death count plot w/ today's additions (Chinese version)
daily_change_tw %>%
  gather(Category, Deaths, c("死亡案例",
                             "今日回報新增死亡案例"))%>%
  ggplot(aes(x = Date, y = Deaths , fill = Category, label = Deaths)) +
  geom_bar(stat = "identity")+
  geom_text(size = 3, position = position_stack(vjust=0.6))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  labs(title = "臺灣新冠肺炎死亡案例統計",
       subtitle = "2021年6月21日更新 / 5月21日開始追蹤",
       caption = "資料來源: 臺灣衛生福利部疾病管制署")


# English version 
daily_change %>%
  gather(Category, Deaths, c("Deaths Previously Reported",
                             "Deaths Reported Today"))%>%
  ggplot(aes(x = Date, y = Deaths , fill = Category, label = Deaths)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE))+
  geom_text(size = 3, position = position_stack(vjust=0.6, reverse = TRUE))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  labs(title = "Taiwan COVID Deaths",
       subtitle = "Updated June 21st, 2021 / Tracking began on May 21st, 2021",
       caption = "Source: Taiwan CDC")


# Start using the full stats for different categories 


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
                  value = 30),
      checkboxInput("")
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