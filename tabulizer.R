install.packages("tidyverse")
install.packages("tabulizer")
library(tabulizer)
library(tidyverse)

#### Setup Master #####
master <- extract_tables("june5.pdf")
page1 <- as.data.frame(master[[1]], stringAsFactors = FALSE)
page2 <- as.data.frame(master[[2]], stringAsFactors = FALSE)
master <- bind_rows(page1, page2)
master <- master[-1,1:11]
colnames(master) <- c("CaseNum","Gender","Age","Chronic","History",
                           "SymptomDate", "Symptoms","TestDate",
                           "QuarantineDate","ConfirmDate","DOD")
deaths <- as.data.frame(table(master$DOD))
colnames(deaths) <- c("Date", "Deaths")

##### Daily Updates #####
today <- extract_tables("june6.pdf")

page1_today <- as.data.frame(today[[1]], stringAsFactors = FALSE)

if (length(today) > 1){
  for (x in 2:length(today)){
    nextPage <- as.data.frame(today[[x]], stringAsFactors = FALSE)
    nextPage <- nextPage[-1,]
    page1_today <- bind_rows(page1_today,nextPage)
  }
}
master_today <- page1_today[-1,2:12]
colnames(master_today) <- c("CaseNum","Gender","Age","Chronic","History",
                         "SymptomDate", "Symptoms","TestDate",
                         "QuarantineDate","ConfirmDate","DOD")

##### Bind today's results to Master #####
master <- bind_rows(master, master_today)

##### Analytics #####
deaths_update <- as.data.frame(table(master$DOD))
colnames(deaths_update) <- c("Date", "Deaths")


compare <- full_join(deaths, deaths_update, by = "Date", 
                     suffix = c("_reported_yesterday", "_reported_today")) %>%
  replace_na(list("Deaths_reported_yesterday" = 0, 
                  "Deaths_reported_today" = 0)) %>%
  mutate(New_deaths_reported = 
           Deaths_reported_today - Deaths_reported_yesterday) %>%
  mutate(Date = as.character(Date)) %>%
  arrange(Date)


chronic_tally <- master %>%
  select(DOD, Chronic) %>%
  mutate(DOD = as.character(DOD)) %>%
  count(Chronic, DOD) %>%
  spread(Chronic, n) %>%
  arrange(DOD) %>%
  select(-DOD)

gender_tally <- master %>%
  select(DOD, Gender) %>%
  mutate(DOD = as.character(DOD))%>%
  count(Gender,DOD) %>%
  spread(Gender, n) %>%
  arrange(DOD) %>%
  bind_cols(chronic_tally)

release <- bind_cols(compare, gender_tally) %>%
  select(-DOD)
  
colnames(release) <- c("Date_of_Death", "Deaths_Reported_Yesterday",
                      "Deaths_Reported_Today", "New_Deaths_Reported", 
                      "Female", "Male", "Has_Chronic_Illness", 
                      "No_Chronic_Illness", "Chronic_Illness_Uncertain")
release[is.na(release)]<- 0


##### Output #####
write.csv(release, "Daily_release.csv")
  
  

