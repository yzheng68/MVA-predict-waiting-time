
## remove objects
rm(list = ls())

## load r packages
library(readr)
library(dplyr)

## save working directory
wd = getwd()
data.wd = paste0(wd,"/data")

filenames = list.files(data.wd)

###########################
## read .txt raw data
###########################

read_raw_data = mvadata = NULL
setwd(data.wd)

for(i in seq_along(filenames)){
  
  ## read each raw data
  read_raw_data <- read_delim(filenames[i],
                              delim = " ", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE,
                              col_types = cols(X4 = col_character(), X5 = col_character()))
  
  if(ncol(read_raw_data) == 5){
    names(read_raw_data) = c("service","num_people","wait_time","date","time")
    read_raw_data = as.data.frame(read_raw_data)
    
    ## add columns and clean strings
    read_raw_data$filename = filenames[i] %>%
      gsub(".txt", "", .) %>%
      gsub("ExpressOffice","", .) %>%
      gsub("LimitedServiceOffice","", .)
    read_raw_data$office = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", read_raw_data$filename)
    
    ## date to day
    read_raw_data$day = weekdays(as.Date(read_raw_data$date))
    
    ## reorder columns
    read_raw_data = read_raw_data %>%
      select(office, service, num_people, wait_time, date, day, time)
    
    ## rbind data
    mvadata = rbind(mvadata, read_raw_data)
  }
  
}

###########################
## add time index
###########################

library(kimisc)
mvadata$index = ceiling((hms.to.seconds(mvadata$time)+1-hms.to.seconds("08:30:00"))/300)

###########################
## delete invalid data 
###########################

library(dplyr)
library(Hmisc)
mvadata = mvadata %>%
  # delete holidays: Veteran's day and Thanksgiving Day
  filter(!date %in% c("2017-11-10", "2017-11-11", "2017-11-23"),
         # delete Saturday afternoon
         !(day == "Saturday" & index %in% c(43:96))) %>%
  # capitalize the first letter of office name
  mutate(office = capitalize(office))

###########################
## export to .csv file
###########################

setwd(wd)
write.csv(mvadata, "mvadata.csv", row.names = FALSE)


###########################
## change time zone
###########################



