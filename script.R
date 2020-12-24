#clear the environment 
rm(list = ls())
ls()

#read the data
library(readr)
exam <- read_csv("da_exam_file.csv")

library(dplyr)
library(tidyverse)

#arranging 'event' values for patient ID = 10 in order
exam <- exam[c(1:26, 28:27, 29:35),]

#Question 1 --------------------------------------------------------------------

#a. converting the dataset into a simpler format (long to wide) with desired order
wide <- exam %>% 
  select('ID', 'consent', 'followup_date', 'radiation_start_date', 'radiation_end_date', 'death_date', 'recurrence_date', 
         'secondary_tumor_date', 'death', 'recurrence', 'secondary_tumor') %>% 
  group_by(ID) %>% 
  fill(consent, radiation_start_date, radiation_end_date, death_date, recurrence_date, secondary_tumor_date, death, recurrence, secondary_tumor,
       .direction = c('down')) %>% 
  slice(n())
  #slice(tail(row_number(), 1)) same as above i.e last value for each ID
  #slice(1, n()) first and last values for each ID

wide$recurrence[wide$recurrence==0]<-1 #changed the recurrence value for patien 7 from 0 to 1 even though there's no recurrence date here.

 
#patient 10 has followup events (rows 27 and 28) switched; I fixed it but that's bc it's a small df.
#recurrence column shows no '1' even though there are recurrence dates; that's bc the recurrence  does not happen at the last followup.

#b. renaming the 'followup_date' variable
colnames(wide)[3] <- "latest_fup_date"

#saving the new simpler format
write.table(wide, file = "wide.csv",
            sep = "\t", row.names = F)


#Question 2 ----------------------------------------------------------------

#finding follow-up duration
library(lubridate)
end<-mdy(wide$latest_fup_date)
begin<-mdy(wide$radiation_start_date)
elapsed.time <- begin %--% end

duration <- as.duration(elapsed.time) / ddays(1)
duration

#finding the median follow-up in years
min(duration, na.rm=TRUE)
max(duration, na.rm=TRUE)
median(duration, na.rm=TRUE) / 365.25   #4.47years

#Question 3 --------------------------------------------------------------

#a. number of patients deceased
summary(wide$death==1) #3

#b. Number of patients with recurrences
summary(wide$recurrence ==1) #6 (should be 5); see line 33 for issue.

#c. Number of patients who experienced a secondary tumor
summary(wide$secondary_tumor ==1) #2

#d. Number of patients that do not have at least 1 follow-up event recorded
sum(is.na(wide$latest_fup_date)) #5
