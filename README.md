### Overview

To access the Shiny dashboard, please visit [**this link**](https://greghuang8.shinyapps.io/Taiwan_Covid_Dash), if you're not on it already. 

This is a repository for tracking COVID deaths in the recent outbreak in Taiwan.
The data is pulled from a daily release issued by the [Taiwan Centers for Disease Control](https://www.cdc.gov.tw/) (Taiwan-CDC), which typically goes live at 2PM 
Taiwan Time (GMT +8). 

All of the deaths reported in the daily release are records from previous days; 
the lag in reporting can run up to a month, although that seems to be improving.
This can cause some confusion as the daily release may have a high death count, 
but in reality it is just catching up on reporting previous days' deaths.
The purpose of this project is to tally up the deaths by its reporting date instead
of the date of the news release. Additionally, other statistics, such as the age,
gender, and chronic condition splits of the deaths are also summarized and presented. 

Note that since I did not start collecting data from Taiwan CDC until mid June, 
and there were no similar news releases that contained data prior to May 21st,
this project under reports 59 deaths that occurred before this date. 

Special thanks to [@xyk2](https://github.com/xyk2) for the idea to snap true 
dates of death from the daily news releases. 

### Data and Files 

Historical plots for newly reported deaths and other statistics can be found in the [**plots**](https://github.com/greghuang8/Taiwan_Covid_Dash/tree/main/plots) folder. 

A summarized csv file that groups each day's death statistics can be found at 
[**Full_statistics.csv**](https://github.com/greghuang8/Taiwan_Covid_Dash/blob/main/Full_statistics.csv).

Each day's new added deaths are stored in the [**daily_changes**](https://github.com/greghuang8/Taiwan_Covid_Dash/tree/main/daily_changes) folder. 

A csv file indicating each case's duration between testing positive and death can be found in the [**Durations.csv**](https://github.com/greghuang8/Taiwan_Covid_Dash/blob/main/Durations.csv) file. 

### About
Author: [Greg Huang](https://github.com/greghuang8)

Please forward your questions and comments to <gregory.huang@ryerson.ca>!





