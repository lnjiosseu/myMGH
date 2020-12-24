#clear the environment 
rm(list = ls())
ls()

#read the data
library(readr)
exam <- read_csv("da_exam_file.csv")

library(dplyr)
library(tidyverse)
library(lubridate)

#creating a copy of the dataset
abc <- exam

#checking the structure of the dataset
str(abc) #all date values are character types and death, recurrence and secondary_tumor are numeric types

#converting date values from character to date type
abc$followup_date = as.Date(abc$followup_date, "%m/%d/%Y")
abc$consent = as.Date(abc$consent, "%m/%d/%Y")
abc$radiation_start_date = as.Date(abc$radiation_start_date, "%m/%d/%Y")
abc$radiation_end_date = as.Date(abc$radiation_end_date, "%m/%d/%Y")
abc$death_date = as.Date(abc$death_date, "%m/%d/%Y")
abc$recurrence_date = as.Date(abc$recurrence_date, "%m/%d/%Y")
abc$secondary_tumor_date = as.Date(abc$secondary_tumor_date, "%m/%d/%Y")

#converting the following values from numeric to character type
abc$death = as.character(abc$death)
abc$recurrence = as.character(abc$recurrence)
abc$secondary_tumor = as.character(abc$secondary_tumor)

str(abc)
#Question 1 --------------------------------------------------------------------

#a. converting the dataset into a simpler format (long to wide) with desired order
wide <- abc %>%
  pivot_wider(names_from = event, values_from = c(followup_date, death, recurrence, secondary_tumor)) %>%   #spreads event values across the dataset
  group_by(ID) %>%
  mutate(last_followup_date  = max(followup_date_followup_1, followup_date_followup_2, followup_date_followup_3, followup_date_followup_4, 
                                   followup_date_followup_5, na.rm=TRUE),
         death = max(death_followup_1, death_followup_2, death_followup_3, death_followup_4, death_followup_5, na.rm=TRUE),
         recurrence = max(recurrence_followup_1, recurrence_followup_2, recurrence_followup_3, recurrence_followup_4, recurrence_followup_5, na.rm=TRUE),
         secondary_tumor = max(secondary_tumor_followup_1, secondary_tumor_followup_2, secondary_tumor_followup_3, secondary_tumor_followup_4, 
                               secondary_tumor_followup_5, na.rm=TRUE)) %>% #combine columns from same category into a single one
  select('ID', 'consent', 'last_followup_date', 'radiation_start_date', 'radiation_end_date', 'death_date', 'recurrence_date', 
         'secondary_tumor_date', 'death', 'recurrence', 'secondary_tumor') %>% 
  fill(consent, last_followup_date, radiation_start_date, radiation_end_date, death_date, recurrence_date, secondary_tumor_date) %>% 
  slice(n())

#b. renaming the 'followup_date' variable
colnames(wide)[3] <- "latest_fup_date"

#saving the new simpler format
write.csv(wide, file = "wide.csv", na = "",
          sep = "\t", row.names = F)


#Question 2 ----------------------------------------------------------------

#finding follow-up duration
end<-ymd(wide$latest_fup_date)
begin<-ymd(wide$radiation_start_date)
elapsed.time <- begin %--% end

duration <- as.duration(elapsed.time) / ddays(1)
duration

#finding the median follow-up in years
min(duration, na.rm=TRUE)
max(duration, na.rm=TRUE)
median(duration, na.rm=TRUE) / 365.25   #4.47years

#Question 3 --------------------------------------------------------------

#a. number of patients deceased
table(wide$death, exclude=NULL) #3

#b. Number of patients with recurrences
table(wide$recurrence, exclude=NULL) #5

#c. Number of patients who experienced a secondary tumor
table(wide$secondary_tumor, exclude=NULL) #2

#d. Number of patients that do not have at least 1 follow-up event recorded
table(wide$latest_fup_date, exclude=NULL) #5
