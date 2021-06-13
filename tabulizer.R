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
  pages <- as.data.frame(pdf_table[[1]], stringAsFactors = FALSE)
  pages <- pages[-1,2:12] 
  
  if(length(pdf_table) > 1) {
    for (x in 2:length(pdf_table)){
      nextPage <- as.data.frame(pdf_table[[x]], stringAsFactors = FALSE)
      nextPage <- nextPage[-1,2:12]
      pages <- bind_rows(pages,nextPage)
      }
  }

  colnames(pages) <- c("CaseNum","Gender","Age","Chronic","History",
                        "SymptomDate", "Symptoms","TestDate",
                        "QuarantineDate","ConfirmDate","DOD")
  master <- bind_rows(master,pages)
  
}

##### Analytics for MASTER data #####
analytics <- master %>%
  mutate(Age = str_extract_all(Age,"[0-9]+"))%>%
  mutate(Age = paste("Age_", Age, sep=""))%>%
  mutate(Age = paste(Age,"_plus",sep="")) %>%
  mutate(Chronic = str_replace(Chronic, "無", "No_Chronic_Condition")) %>%
  mutate(Chronic = str_replace(Chronic, "調查中",
                               "Chronic_Condition_Uncertain")) %>%
  mutate(Chronic = iconv(Chronic,from = "latin1", to = "ASCII")) %>%
  mutate(Chronic = replace_na(Chronic, "Has_Chronic_Condition")) %>%
  mutate(DOD = as.character(DOD)) %>%
  select(DOD, Gender, Chronic, Age)

deaths <- as.data.frame(table(analytics$DOD))
colnames(deaths) <- c("Date", "Deaths")
deaths <- deaths %>%
  mutate(Date = as.character(Date)) %>%
  arrange(Date)

gender_tally <- analytics %>%
  count(Gender, DOD) %>%
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0)%>%
  arrange(DOD)%>%
  select(-DOD)

chronic_tally <- analytics %>%
  count(Chronic, DOD) %>%
  pivot_wider(names_from = Chronic, values_from = n, values_fill = 0)%>%
  arrange(DOD)%>%
  select(-DOD)

age_tally <- analytics %>%
  count(Age, DOD) %>%
  pivot_wider(names_from = Age, values_from = n, values_fill = 0)%>%
  arrange(DOD)%>%
  select(-DOD)

analytics_release <- bind_cols(list(deaths, gender_tally,
                                    chronic_tally, age_tally)) %>%
  mutate(Date = as.Date(Date, "%m/%d")) %>%
  arrange(Date)

colnames(analytics_release)[colnames(analytics_release) == "女"] <- "Female"
colnames(analytics_release)[colnames(analytics_release) == "男"] <- "Male"

write.csv(analytics_release, "Full_statistics.csv")

# Code above allows for reproduction of the entire database up til 
# the most recent press release of death counts. 

##### DAILY CHANGE TRACKING #####
today <- extract_tables("june13.pdf")
pages_today <- as.data.frame(today[[1]])
pages_today <- pages_today[-1,2:12]

if(length(today) > 1) {
  for (x in 2:length(today)){
    nextPage <- as.data.frame(today[[x]], stringAsFactors = FALSE)
    nextPage <- nextPage[-1,2:12]
    pages_today <- bind_rows(pages_today,nextPage)
  }
}

colnames(pages_today) <- c("CaseNum","Gender","Age","Chronic","History",
                         "SymptomDate", "Symptoms","TestDate",
                         "QuarantineDate","ConfirmDate","DOD")

##### Compare today's deaths to master #####
deaths_old <- deaths
deaths_today <- as.data.frame(table(pages_today$DOD))
colnames(deaths_today) <- c("Date", "Deaths")

compare <- full_join(deaths_old, deaths_today, by = "Date", 
                     suffix = c("_previously_reported", "_reported_today")) %>%
  replace_na(list("Deaths_previously_reported" = 0, 
                  "Deaths_reported_today" = 0)) %>%
  mutate(New_total = Deaths_previously_reported + Deaths_reported_today)%>%
  mutate(Date = as.Date(Date,"%m/%d")) %>%
  arrange(Date)

##### Output #####
write.csv(compare, "Daily_changes/Daily_change_June_13.csv")


  

