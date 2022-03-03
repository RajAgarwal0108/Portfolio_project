# loading libraries needed
library(tidyverse)
library(janitor)
library(lubridate)
library(readr)


# creating data frames of 12 months trips

df_01 = read_csv("files/2021_01-divvy-tripdata.csv")
df_02 = read_csv("files/2021_02-divvy-tripdata.csv")
df_03 = read_csv("files/2021_03-divvy-tripdata.csv")
df_04 = read_csv("files/2021_04-divvy-tripdata.csv")
df_05 = read_csv("files/2021_05-divvy-tripdata.csv")
df_06 = read_csv("files/2021_06-divvy-tripdata.csv")
df_07 = read_csv("files/2021_07-divvy-tripdata.csv")
df_08 = read_csv("files/2021_08-divvy-tripdata.csv")
df_09 = read_csv("files/2021_09-divvy-tripdata.csv")
df_10 = read_csv("files/2021_10-divvy-tripdata.csv")
df_11 = read_csv("files/2021_11-divvy-tripdata.csv")
df_12 = read_csv("files/2021_12-divvy-tripdata.csv")


# Merging all data frames into one single frame naming:"bike_rides"

bike_rides = rbind(df_01,df_02,df_03,df_04,df_05,df_06,df_07,df_08,df_09,df_10,df_11,df_12)


# Removing non-required fields from the data frame (bike_rides) which includes -> [start_lat,start_lng,end_lat,end_lng] 


bike_rides = bike_rides %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))

bike_rides = bike_rides %>% 
  select(-c(start_station_name,start_station_id,end_station_name,end_station_id))



# Adding columns into the data frame for analysis

#calculating time taken in each ride
bike_rides$ride_length = difftime(bike_rides$ended_at,bike_rides$started_at) 
#Adding date,month,day,year,day_of_week and season so we could aggregate ride data

bike_rides$date = as.Date(bike_rides$started_at) # in the format yyyy-mm-dd
bike_rides$month = format(as.Date(bike_rides$date),"%m")
bike_rides$year = format(as.Date(bike_rides$date),"%y")
bike_rides$day = format(as.Date(bike_rides$date),"%d")
bike_rides$day_of_week = format(as.Date(bike_rides$date),"%A")

# creating column for seasons

bike_rides = bike_rides %>%
  mutate(season = case_when(month == "03"~"spring",
                            month == "04"~"spring",
                            month == "05"~"spring",
                            month == "06"~"summer",
                            month == "07"~"summer",
                            month == "08"~"summer",
                            month == "09"~"autumn",
                            month == "10"~"autumn",
                            month == "11"~"autumn",
                            month == "12"~"winter",
                            month == "01"~"winter",
                            month == "02"~"winter")
  )




