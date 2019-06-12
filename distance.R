library(dplyr)
library(gmapsdistance)
library(ggmap)
library(lubridate)
library(udunits2)
library(knitr)
library(printr)

###Load mvadata


###Processed Data

geo.office = read.csv("mva data.csv",strip.white=TRUE)
geo.office$Office<- paste(geo.office$Name,geo.office$County,sep = ', ')

# Input:address 
#Calculate time and distance
#Input : departure time, departure data, commute, address

est.time.distance <-function(address,commute,dep.date,dep.time){
  
  Lat.MVA<-geo.office$lat
  Lon.MVA<-geo.office$lon
  register_google(key="AIzaSyBucAhtOn805M1SKeSgN-81dnwKqFN5eSE")
  address<-geocode(address)
  
  ori<-paste(address$lat, address$lon,sep = "+")
  des<-paste(Lat.MVA , Lon.MVA,sep = "+")
  
  disinfor<-gmapsdistance(origin = ori,destination = des %>% as.vector(),
                          mode = commute, 
                          dep_date = as.character((as.Date(dep.date)+7)), 
                          dep_time = dep.time, 
                          shape = "long",key="AIzaSyBucAhtOn805M1SKeSgN-81dnwKqFN5eSE")
  
  
  output.data <- geo.office$Name %>% as.data.frame()
  colnames(output.data)[1]<-"office"
  output.data$est.time<-disinfor$Time[[3]]/60 
  output.data$dis <- udunits2::ud.convert(disinfor$Distance[[3]], "m", "km")
  arr.time<-seconds_to_period(disinfor$Time[[3]]+period_to_seconds(hms(dep.time)))
  output.data$arr.time<-sprintf("%02i:%02i:%02i", hour(arr.time), minute(arr.time),second(arr.time))
  output.data$day<-format(as.Date(dep.date), "%A")
  return(output.data)
}

#address="929 North Wolfe Street, MD 21209"
#commute="driving"
#dep.date = "2017-12-13"
#dep.time = "15:40:00"

#output.data<-est.time.distance(address="929 North Wolfe Street, MD 21209",commute="driving",dep.date="2017-12-12",dep.time="08:40:00")
#coordinate of the address :Latitude(39.301224),Longitude(-76.590923)