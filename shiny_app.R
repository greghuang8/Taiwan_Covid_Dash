library(shiny)
library(tidyverse)
library(RColorBrewer)

# Load in tables
full_stats <- read.csv("Full_statistics.csv", header = TRUE,
                       stringsAsFactors = FALSE) %>%
  select(-X) %>%
  na_if(0)

daily_change <- read.csv("daily_changes/Daily_change_June_28.csv", 
                         header = TRUE, stringsAsFactors = FALSE) %>%
  na_if(0) %>%
  select(-X)
colnames(daily_change) = c("Date", "Deaths Previously Reported",
                           "Deaths Reported Today", "Updated Total Deaths")

daily_change_tw <- daily_change
colnames(daily_change_tw) = c("Date", "死亡案例", "今日回報新增死亡案例",
                              "Updated Total Deaths")

##### Part 1: Daily death counts plotted #####
# Death count plots w/ today's additions (Chinese version)
daily_change_tw %>%
  gather(Category, Deaths, c("死亡案例",
                             "今日回報新增死亡案例"))%>%
  ggplot(aes(x = Date, y = Deaths , fill = Category, label = Deaths)) +
  geom_bar(stat = "identity")+
  geom_text(size = 3, position = position_stack(vjust=0.6))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  labs(title = "臺灣新冠肺炎死亡案例統計",
       subtitle = "2021年6月28日更新 / 5月21日開始追蹤",
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
       subtitle = "Updated June 28th, 2021 / Tracking began on May 21st, 2021",
       caption = "Source: Taiwan CDC")



##### Part 2: General Statistics #####
# Various percentages fro Shiny display purposes
total_deaths <- sum(full_stats$Deaths)
female_pct <- round(sum(full_stats$Female, na.rm = TRUE)/total_deaths * 100) 
male_pct <- 100-female_pct
chronic_pct <- round(sum(full_stats$Has_Chronic_Condition, 
                         na.rm = TRUE)/total_deaths * 100)
und_chronic_pct <- round(
  sum(full_stats$Chronic_Condition_Uncertain, na.rm = TRUE)/total_deaths * 100)
non_chronic_pct <- 100 - chronic_pct - und_chronic_pct

# Quick Pie Charts
# gender split
slices <- c(female_pct, male_pct)
lbls <- c("Female", "Male") %>%
  paste("",slices) %>%
  paste0("","%")
pie(slices, lbls, main = "Gender Percentage Split", 
    col = brewer.pal(2, "Pastel2")) 

# chronic split
slices <- c(und_chronic_pct,chronic_pct, non_chronic_pct) 
lbls <- c("Chronic Conditions Uncertain", "Has Chronic Conditions", 
          "No Chronic Conditions") %>%
  paste(slices) %>%
  paste0("%")
pie(slices, lbls, main = "Chronic Conditions Percentage Split", 
    col = brewer.pal(3, "Pastel2"))

# Age split (pies are bad for this)

  

# Stacked bars for each category
# Gender
full_stats %>%
  gather(Gender, Deaths, c("Female", "Male")) %>%
  ggplot(aes(x = Date, y = Deaths, fill = Gender, label = Deaths)) + 
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.6)) + 
  theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  scale_fill_manual(values = brewer.pal(2, "Pastel2"))+
  labs(title = "Deaths By Gender", 
       caption = "Source: Taiwan CDC")

# Chronic
full_stats %>%
  gather(Chronic_Condition, Deaths, c("Has_Chronic_Condition", "No_Chronic_Condition", 
                           "Chronic_Condition_Uncertain")) %>%
  ggplot(aes(x = Date, y = Deaths, fill = Chronic_Condition, label = Deaths)) + 
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.6)) + 
  theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  scale_fill_manual(values = brewer.pal(3, "Pastel2"))+
  labs(title = "Deaths By Chronic Condition", 
       caption = "Source: Taiwan CDC")

# Age
full_stats %>%
  select(starts_with(c("Date","Deaths", "Age"))) %>%
  gather(Age_Group, Deaths, c(starts_with("Age"))) %>%
  ggplot(aes(x = Date, y = Deaths, fill = Age_Group, label = Deaths)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(size = 3, position = position_stack(vjust = 0.6, reverse = TRUE)) + 
  theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  scale_fill_manual(values = brewer.pal(8, "Set3"))+
  labs(title = "Deaths By Age Group", 
       caption = "Source: Taiwan CDC") 

# Grouped statistics


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