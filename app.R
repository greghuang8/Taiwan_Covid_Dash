library(shiny)
library(tidyverse)
library(RColorBrewer)

# Load in tables
full_stats <- read.csv("Full_statistics.csv", header = TRUE,
                       stringsAsFactors = FALSE) %>%
  select(-X) %>%
  na_if(0)

duration_stats <- read.csv("Durations.csv", header = TRUE, 
                           stringsAsFactors = FALSE)

daily_change <- read.csv("daily_changes/Daily_change_July_4.csv", 
                         header = TRUE, stringsAsFactors = FALSE) %>%
  na_if(0) %>%
  select(-X)

today_date <- "2021/07/04"

colnames(daily_change) = c("Date", "Deaths Previously Reported",
                           "Deaths Reported Today", "Updated Total Deaths")

daily_change_tw <- daily_change
colnames(daily_change_tw) = c("Date", "死亡案例", "今日回報新增死亡案例",
                              "Updated Total Deaths")


# Various numbers Shiny display purposes
total_deaths <- sum(full_stats$Deaths)
new_deaths <- sum(daily_change$`Deaths Reported Today`)

# Prep some tables for plotting
# Age - lock in factors
ages <- full_stats %>%
  select(starts_with("Age"))
num_colors <- ncol(ages) 

age_stacked_stats <- full_stats %>%
  select(starts_with(c("Date","Deaths", "Age"))) %>%
  gather(Age_Group, Deaths, c(starts_with("Age"))) %>%
  mutate(Age_Group = str_extract(Age_Group, "[[:digit:]]+"))%>%
  mutate(Age_Group = str_c(Age_Group, "+"))

age_stacked_stats$Age_Group <- factor(age_stacked_stats$Age_Group,
                                      levels = unique(age_stacked_stats$Age_Group))

# Prep table for group plotting
full_stats_group <- full_stats
full_stats_group[is.na(full_stats_group)] <- 0
# make new data frame from sums of each column 
Categories <- c(colnames(full_stats_group)[-c(1,2)])
Deaths <- numeric(0)
for (item in Categories){
  Deaths <- append(Deaths, sum(full_stats_group[[item]]))
}
Parameters <- c(rep("Gender",2), rep("Chronic Condition",3), 
              rep("Age",length(Categories)-5)) 
grouped_stats <- data.frame(Categories,Deaths,Parameters)
grouped_stats <- grouped_stats %>%
  mutate(Deaths_pct = as.character(round(Deaths/total_deaths*100,1)))%>%
  mutate(Deaths_pct = str_c(Deaths_pct,"%"))

grouped_stats$Categories <- factor(grouped_stats$Categories, 
                                   levels = grouped_stats$Categories)

#Age - lock in factors
age_grouped_stats <- grouped_stats %>%
  filter(Parameters == "Age") %>%
  mutate(Age = str_extract(Categories, "[[:digit:]]+"))%>%
  mutate(Age = str_c(Age, "+"))
age_grouped_stats$Age <- factor(age_grouped_stats$Age, 
                                levels = age_grouped_stats$Age)


ui <- fluidPage(
  
  # Application title
  titlePanel("COVID Deaths in Taiwan"),
  
  tabsetPanel(
    tabPanel("Welcome",includeMarkdown("README.md")),
    
    tabPanel("Today's Release",
             br(),
             sidebarLayout(
               sidebarPanel(h4("Today's Release"),
                            p(str_c("This panel to the right reports on the ",
                                    "latest status of COVID deaths in Taiwan, ",
                                    "followed by graphs illustrating daily counts."))),
               mainPanel(
                 h5(tags$b("Total Deaths Reported: "),
                    textOutput("total_deaths", inline = TRUE)),
                 h5(tags$b("New Deaths Reported Today: "),
                    textOutput("new_deaths", inline = TRUE)),
                 h5(tags$b("Last Updated: "),
                    textOutput("update_date", inline = TRUE)),
                 br(),
                 p(str_c("Note: This module contains 59 fewer deaths compared ",
                         "to the actual total deaths, as data prior to 5/21 ",
                         "is not avaiable for analysis. Please see the ",
                         "Welcome page for more information. "))
                 )
               ),
             hr(),
             h4("English Version"),
             plotOutput("dcPlot"),
             hr(),
             h4("繁體中文"),
             plotOutput("dc_tw_plot"),
             ),
    
    tabPanel("Age Statistics",
             br(),
             p(str_c("This tab shows the age distribution of deaths. ", 
                     "The first graph organizes the age groups by Date of ",
                     "Death, whereas the second graph shows counts by age group.")),
             hr(),
             plotOutput("age_stack_plot"),
             hr(),
             plotOutput("age_group_plot")
             ),
    
    tabPanel("Chronic Condition Statistics",
             br(),
             p(str_c("This tab shows the chronic condition distribution of deaths. ", 
                     "The first graph organizes the groups by Date of ",
                     "Death, whereas the second graph shows counts by group.")),
             hr(),
             plotOutput("chronic_stack_plot"),
             hr(),
             plotOutput("chronic_group_plot")
             ),
    
    tabPanel("Gender Statistics",
             br(),
             p(str_c("This tab shows the gender distribution of deaths. ", 
                     "The first graph organizes the groups by Date of ",
                     "Death, whereas the second graph shows counts by group.")),
             hr(),
             plotOutput("gender_stack_plot"),
             hr(),
             plotOutput("gender_group_plot")
             ),
    
    tabPanel("Duration Statistics",
             br(),
             
             #Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 p(str_c("This histogram shows the distribution of days ",
                          "between testing positive and death; a negative ",
                          "value indicates that the person tested positive ",
                          "after death. ")),
                 hr(),
                 sliderInput("bins",
                             "Set number of bins for histogram:",
                             min = 1,
                             max = 50,
                             value = 30),
                 hr(),
                 h5(tags$b("Average Days: "),
                    textOutput("duration_mean", inline = TRUE)),
                 h5(tags$b("Median Days: "),
                    textOutput("duration_median", inline = TRUE)),
                 h5(tags$b("Number of Cases Tested Positive After Death: "),
                    textOutput("duration_negative", inline = TRUE)),
                 h5("Note: Mean and median consider duration >= 0 days only.")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("durationPlot")
                 )
               ),
             br())
    # tabPanel("Historical Plots",
    #          br(),
    #          h4("Under maintenance! Please check back later."))
  )
)
server <- function(input, output) {
  
  output$total_deaths <- renderText({sum(full_stats$Deaths)})
  output$new_deaths <- renderText({sum(daily_change$`Deaths Reported Today`,
                                       na.rm = TRUE)})
  output$update_date <- renderText({today_date})
   
  output$duration_mean <- renderText({round(
    mean(duration_stats$DTD[duration_stats$DTD >= 0]),1)})
   
  output$duration_median <- renderText({median(
    duration_stats$DTD[duration_stats$DTD >=0])})
  
  output$duration_negative <- renderText({sum(duration_stats$DTD < 0)})
  
  output$dcPlot <- renderPlot({
    daily_change %>%
      gather(Category, Deaths, c("Deaths Previously Reported",
                                 "Deaths Reported Today"))%>%
      ggplot(aes(x = Date, y = Deaths , fill = Category, label = Deaths)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE))+
      geom_text(size = 3, position = position_stack(vjust=0.6, reverse = TRUE))+
      theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
      labs(title = "Taiwan COVID Deaths",
           subtitle = "Tracking began on 2021/05/21",
           caption = str_c("Source: Taiwan CDC; Updated on ", today_date))
    
  })
  
  output$dc_tw_plot <- renderPlot({
    daily_change_tw %>%
      gather(Category, Deaths, c("死亡案例",
                                 "今日回報新增死亡案例"))%>%
      ggplot(aes(x = Date, y = Deaths , fill = Category, label = Deaths)) +
      geom_bar(stat = "identity")+
      geom_text(size = 3, position = position_stack(vjust=0.6))+
      theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
      labs(title = "臺灣新冠肺炎死亡案例統計",
           subtitle = "5月21日開始追蹤",
           caption = str_c("資料來源: 臺灣衛生福利部疾病管制署/ 更新日: ",
                           today_date),
           x = "日期",
           y = "死亡案例數",
           fill = "")
  })
  
  output$age_stack_plot <- renderPlot({
    age_stacked_stats %>%
      ggplot(aes(x = Date, y = Deaths, fill = Age_Group,
                 label = Deaths)) + 
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      geom_text(size = 3, position = position_stack(vjust = 0.6, reverse = TRUE)) + 
      theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
      scale_fill_manual(values = brewer.pal(num_colors, "Set3"))+
      labs(title = "Distribution of Deaths by Age Group", 
           caption = str_c("Source: Taiwan CDC; Updated ",today_date),
           subtitle = "Sorted by Date of Death",
           fill = "Age Group")     
  })
  
  output$age_group_plot <- renderPlot({
    age_grouped_stats %>%
      ggplot(aes(x=Age, y=Deaths, fill = Age)) + 
      geom_bar(stat = "identity", width = 0.8)+
      geom_text(aes(label = Deaths_pct), vjust = -0.5)+
      scale_fill_manual(values = brewer.pal(num_colors, "Set3"))+
      labs(title = "Deaths By Age Group",
           fill = "Age Group",
           caption = str_c("Source: Taiwan CDC; Updated ",today_date))     
  })
  
  output$chronic_stack_plot <- renderPlot({
    full_stats %>%
      gather(Chronic_Condition, Deaths, c("Has_Chronic_Condition", "No_Chronic_Condition", 
                                          "Chronic_Condition_Uncertain")) %>%
      mutate(Chronic_Condition = str_replace_all(Chronic_Condition, "_"," ")) %>%
      ggplot(aes(x = Date, y = Deaths, fill = Chronic_Condition, label = Deaths)) + 
      geom_bar(stat = "identity") +
      geom_text(size = 3, position = position_stack(vjust = 0.6)) + 
      theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
      scale_fill_manual(values = brewer.pal(3, "Pastel2"))+
      labs(title = "Distribution of Deaths by Chronic Condition", 
           caption = str_c("Source: Taiwan CDC; Updated ",today_date),
           subtitle = "Sorted by Date of Death",
           fill = "Chronic Condition")     
  })
  
  output$chronic_group_plot <- renderPlot({
    grouped_stats %>%
      filter(Parameters == "Chronic Condition") %>%
      mutate(Chronic_Condition = Categories) %>%
      mutate(Chronic_Condition = str_replace_all(Chronic_Condition, "_", " "))%>%
      ggplot(aes(x = Chronic_Condition, y = Deaths, fill = Chronic_Condition))+
      geom_text(aes(label = Deaths_pct), vjust = -0.5, hjust = 0.5)+
      geom_bar(stat = 'identity', width = 0.5)+
      scale_fill_manual(values = brewer.pal(3, "Pastel2"))+
      labs(title = "Deaths by Chronic Conditions",
           fill = "Chronic Condition",
           x = "Status",
           caption = str_c("Source: Taiwan CDC; Updated ",today_date))    
  })
  
  output$gender_stack_plot <- renderPlot({
    full_stats %>%
      gather(Gender, Deaths, c("Female", "Male")) %>%
      ggplot(aes(x = Date, y = Deaths, fill = Gender, label = Deaths)) + 
      geom_bar(stat = "identity") +
      geom_text(size = 3, position = position_stack(vjust = 0.6)) + 
      theme(axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
      scale_fill_manual(values = brewer.pal(2, "Pastel2"))+
      labs(title = "Distribution of Deaths by Gender", 
           caption = str_c("Source: Taiwan CDC; Updated ",today_date),
           subtitle = "Sorted by Date of Death")     
  })
  
  output$gender_group_plot <- renderPlot({
    grouped_stats %>%
      filter(Parameters == "Gender") %>%
      mutate(Gender = Categories) %>%
      ggplot(aes(x = Gender, y = Deaths, fill = Gender))+
      geom_bar(stat = 'identity', width = 0.4)+
      geom_text(aes(label = Deaths_pct), vjust = -0.5, hjust = 0.5)+
      scale_fill_manual(values = brewer.pal(2, "Pastel2"))+
      labs(title = "Deaths by Gender Split", 
           caption = str_c("Source: Taiwan CDC; Updated ",today_date))    
  })
  
  output$durationPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- duration_stats$DTD
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', 
         main = "Days Elapsed Between Testing Positive and Dying",
         xlab = "Elapsed Days",
         labels = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)