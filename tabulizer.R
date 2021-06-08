install.packages("tidyverse")
install.packages("tabulizer")
library(tabulizer)
library(tidyverse)

##### Retrospective Data Additions #####
# May 28 to June 4th all have the same format
# Loop through 8 documents in retro_pdf

index <- 0
for (item in list.files("retro_pdf")){
  path <- paste("retro_pdf/",item, sep = "")
  pdf_table <- extract_tables(path) 
  if (index == 0){
    master <- as.data.frame(pdf_table[[1]], stringAsFactors = FALSE)
    master <- master[-1,]
    index <- index + 1
  }
  else{
    pdf_table <- as.data.frame(pdf_table[[1]], stringAsFactors = FALSE)
    pdf_table <- pdf_table[-1,1:11]
    master <- bind_rows(master, pdf_table)
  }
}


##### Setup June 5th (unique case w/ locations) #####
june5 <- extract_tables("june5.pdf")
page1 <- as.data.frame(june5[[1]], stringAsFactors = FALSE)
page2 <- as.data.frame(june5[[2]], stringAsFactors = FALSE)
june5 <- bind_rows(page1, page2)
june5 <- june5[-1,1:11]
master <- bind_rows(master,june5)
colnames(master) <- c("CaseNum","Gender","Age","Chronic","History",
                      "SymptomDate", "Symptoms","TestDate",
                      "QuarantineDate","ConfirmDate","DOD")

##### June 6 onward #####
#Starting from June 6th, the pdf format changes again. 

for(item in list.files("pdf")){
  path <- paste("pdf/",item, sep = "")
  pdf_table <- extract_tables(path)   
  page1 <- as.data.frame(pdf_table[[1]], stringAsFactors = FALSE)
  page1 <- page1[-1,2:12] 
  
  if(length(pdf_table) > 1) {
    for (x in 1:length(pdf_table)){
      nextPage <- as.data.frame(pdf_table[[x]], stringAsFactors = FALSE)
      nextPage <- nextPage[-1,2:12]
      final <- bind_rows(page1,nextPage)
      }
  }
  else{
    final <- page1
  }
  colnames(final) <- c("CaseNum","Gender","Age","Chronic","History",
                        "SymptomDate", "Symptoms","TestDate",
                        "QuarantineDate","ConfirmDate","DOD")
  master <- bind_rows(master,final)
  
}

##### Analytics for MASTER data #####
analytics <- master %>%
  mutate(Age = paste("Age_", Age, sep=""))%>%
  mutate(Age = str_sub(Age,1,6)) %>%
  mutate(Age = paste(Age,"+",sep="")) %>%
  mutate(Chronic = str_replace(Chronic, "無", "No_Chronic_Condition")) %>%
  mutate(Chronic = str_replace(Chronic, "調查中",
                               "Chronic_Condition_Uncertain")) %>%
  mutate(Chronic = iconv(Chronic,from = "latin1", to = "ASCII")) %>%
  mutate(Chronic = replace_na(Chronic, "Has_Chronic_Condition")) %>%
  mutate(DOD = as.character(DOD))


deaths <- as.data.frame(table(analytics$DOD))
colnames(deaths) <- c("Date", "Deaths")
deaths <- deaths %>%
  mutate(Date = as.character(Date)) %>%
  arrange(Date)
  
gender_tally <- analytics %>%
  select(DOD, Gender) %>%
  count(Gender,DOD) %>%
  spread(Gender, n) %>%
  arrange(DOD) %>%
  select(-DOD)

chronic_tally <- analytics %>%
  count(Chronic, DOD) %>%
  spread(Chronic, n) %>%
  arrange(DOD) %>%
  select(-DOD)

age_tally <- analytics %>%
  count(Age, DOD) %>%
  spread(Age, n) %>%
  arrange(DOD) %>%
  select(-DOD)

analytics_release <- bind_cols(list(deaths, gender_tally,
                                    chronic_tally, age_tally))
colnames(analytics_release)[colnames(analytics_release) == "女"] <- "Female"
colnames(analytics_release)[colnames(analytics_release) == "男"] <- "Male"
analytics_release[is.na(analytics_release)] <- 0

write.csv(analytics_release, "Full_statistics.csv")

# Code above allows for reproduction of the entire database up til 
# the most recent press release of death counts. 

##### DAILY CHANGE TRACKING #####
today <- extract_tables("june8.pdf")

master_today <- page1_today[-1,2:12]
colnames(master_today) <- c("CaseNum","Gender","Age","Chronic","History",
                         "SymptomDate", "Symptoms","TestDate",
                         "QuarantineDate","ConfirmDate","DOD")

##### Bind today's results to Master #####
deaths <- as.data.frame(table(master$DOD))
colnames(deaths) <- c("Date", "Deaths")
master <- bind_rows(master, master_today)


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


release[is.na(release)]<- 0


##### Output #####
write.csv(release, "Daily_release.csv")
  
  

