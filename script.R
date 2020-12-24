#clear the environment 
rm(list = ls())
ls()

#read the data
library(readr)
exam <- read_csv("da_exam_file.csv")

library(dplyr)
library(tidyverse)

exam <- exam[c(1:26, 28:27, 29:35),]
#Question 1 --------------------------------------------------------------------

# a. desired order

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

# b. renaming
colnames(wide)[3] <- "latest_fup_date"


#Question 2 ----------------------------------------------------------------

Duration = function(radiation_start_date, followup_date) {
  output = followup_date + radiation_start_date
  return(output)
}
Duration(4, 12)


#Question 3 --------------------------------------------------------------


 
