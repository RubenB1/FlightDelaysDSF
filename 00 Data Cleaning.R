#Data Cleaning

  # The goal of this code is to bundle and merge the data about flight delays, 
  #weather and holidays in the USA for the year 2015. These data are from different 
  #sources and are neither clean or in the same format. That is why we will proceed 
  #with this code in order to retrieve from it a clean data set with which we can work.


#install.packages("tidyverse")
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(base)
library(tidyverse)

#Data Loading initial
graphics.off() 
rm(list=ls())


dataflight2015= read.csv("flights2015.csv")
#dataflight2016= read.csv("flight2016.csv")


# airlines= read.csv("airlines.csv")
# aeroports= read.csv("airports.csv")

#selecting right columns
data=dataflight2015
  #data=dataflight2016

data=select(data,"YEAR", "MONTH", "DAY", "DAY_OF_WEEK", 
  "AIRLINE", "TAIL_NUMBER", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", 
  "DEPARTURE_TIME", "DEPARTURE_DELAY", "TAXI_OUT", "SCHEDULED_TIME", "ELAPSED_TIME", 
  "AIR_TIME", "DISTANCE", "TAXI_IN", "SCHEDULED_ARRIVAL", "ARRIVAL_TIME", "ARRIVAL_DELAY", 
  "CANCELLED", "AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", 
  "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")

#Selecting and filtering for 10 busiest airports in the USA
  #Regarding Origin
SelectAirports= c("ATL","CLT","ORD","DFW","DEN","LAS","LAX","JFK","SFO","SEA")

data=filter(data, ORIGIN_AIRPORT %in% SelectAirports)

  #Regarding destination (only flights between, from and to, the selected airports)

data=filter(data, DESTINATION_AIRPORT %in% SelectAirports)

#Cleaning rows for NA values in the columns 21:25

data[, 21:25][is.na(data[, 21:25])] <- 0 #converting NA values in 0 for the delay as in the dataset the NA values mean no delay (therefore 0)
data= na.omit(data) #omitting the NA values in the dataset that mean abscence of data (to obtain a clean dataset)

#Setting the correct Hour and Minute format in the data file (first having 4 characters in a column and then splitting the data in the corresponding Year, Month,...etc.)
  #Departure
data$DEP_TIME2 = as.character(data$DEPARTURE_TIME)
data$DEP_TIME3 = ifelse(nchar(data$DEP_TIME2)==3, sprintf("0%s",data$DEP_TIME2) , data$DEP_TIME2) 
data$DEP_TIME4 = ifelse(nchar(data$DEP_TIME3)==2, sprintf("00%s",data$DEP_TIME3) , data$DEP_TIME3) 
data$DEP_TIME5 = ifelse(nchar(data$DEP_TIME4)==1, sprintf("000%s",data$DEP_TIME4) , data$DEP_TIME4)

data$DEP_HOUR = substring(data$DEP_TIME5,1,2)
data$DEP_MIN = substring(data$DEP_TIME5,3,4)
data = select(data, -DEP_TIME2,-DEP_TIME3,-DEP_TIME4,-DEP_TIME5,-DEPARTURE_TIME)

  #Arrival
data$ARR_TIME2 = as.character(data$ARRIVAL_TIME)
data$ARR_TIME3 = ifelse(nchar(data$ARR_TIME2)==3, sprintf("0%s",data$ARR_TIME2) , data$ARR_TIME2) 
data$ARR_TIME4 = ifelse(nchar(data$ARR_TIME3)==2, sprintf("00%s",data$ARR_TIME3) , data$ARR_TIME3) 
data$ARR_TIME5 = ifelse(nchar(data$ARR_TIME4)==1, sprintf("000%s",data$ARR_TIME4) , data$ARR_TIME4)

data$ARR_HOUR = substring(data$ARR_TIME5,1,2)
data$ARR_MIN = substring(data$ARR_TIME5,3,4)
data = select(data, -ARR_TIME2,-ARR_TIME3,-ARR_TIME4,-ARR_TIME5,-ARRIVAL_TIME)


#Woking the pre-processed weather file to extract the wind data

load("Weather_US.RData")

Weather_US = mutate(Weather_US,YEAR = format(as.POSIXct(Weather_US$datetime, format="%Y-%m-%d %H:%M:%S"),"%Y"))
Weather_US = mutate(Weather_US,MONTH = format(as.POSIXct(Weather_US$datetime, format="%Y-%m-%d %H:%M:%S"),"%m"))
Weather_US = mutate(Weather_US,DAY = format(as.POSIXct(Weather_US$datetime, format="%Y-%m-%d %H:%M:%S"),"%d"))
Weather_US = mutate(Weather_US,HOUR = format(as.POSIXct(Weather_US$datetime, format="%Y-%m-%d %H:%M:%S"),"%H"))
Weather_US = select(Weather_US, -datetime)

#creating a vector with the names of the cities selected
tobereplaced= unique(Weather_US$City)

#Replace the name of he city by the name of the airport in the city (3 letters format)
for (i in tobereplaced) {
  Weather_US$City[which(Weather_US$City==i)]= SelectAirports[which(tobereplaced==i)]
}

Wind = select(Weather_US, -Temperature,-Pressure)
names(Wind)[1:7]<-c("ORIGIN_AIRPORT","DEP_Humidity","DEP_wind_speed","YEAR","MONTH","DEP_DAY","DEP_HOUR")

#Merging the wind data with the dataflights2015 data

  #Departure
data=merge(data,Wind, by.x=c("ORIGIN_AIRPORT","YEAR","MONTH","DAY","DEP_HOUR"), by.y=c("ORIGIN_AIRPORT","YEAR","MONTH","DEP_DAY","DEP_HOUR"))

  #Arrival
names(Wind)[1:7]<-c("DESTINATION_AIRPORT","ARR_Humidity","ARR_wind_speed","YEAR","MONTH","ARR_DAY","ARR_HOUR")

data=merge(data,Wind, by.x=c("DESTINATION_AIRPORT","YEAR","MONTH","DAY","ARR_HOUR"), by.y=c("DESTINATION_AIRPORT","YEAR","MONTH","ARR_DAY","ARR_HOUR"))

#Working the holiday data

load("Holidays.RData")

Holidays = mutate(Holidays,YEAR = format(as.POSIXct(Date, format="%d.%m.%Y"),"%Y"))
Holidays = mutate(Holidays,MONTH = format(as.POSIXct(Date, format="%d.%m.%Y"),"%m"))
Holidays = mutate(Holidays,DAY = format(as.POSIXct(Date, format="%d.%m.%Y"),"%d"))
Holidays = select(Holidays, -Date)

#Merging the Holidays data with the dataflights2015 data
data=merge(data,Holidays, by.x=c("YEAR","MONTH","DAY"), by.y=c("YEAR","MONTH","DAY"))


#save(data,file="data15_weather_holidays.RData")









