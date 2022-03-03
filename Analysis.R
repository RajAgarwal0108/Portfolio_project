# Conductive descriptive analysis
# distinguishing between casual and annual members on the basis of ride length

# Basic stats on ride_length

mean_ride = mean(bike_rides_v2$ride_length) #average(total ride_length / rides)
median_ride = median(bike_rides_v2$ride_length)#midpoint number in the ascending array of ride lengths
max_ride = max(bike_rides_v2$ride_length)# longest ride
min_ride = min(bike_rides_v2$ride_length)# smallest ride

# ordering the days_of_week
bike_rides_v2$day_of_week = ordered(bike_rides_v2$day_of_week,
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Average ride time by each day for casual and members

aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual,bike_rides_v2$day_of_week),
           FUN = mean)

#------------Total rides-------------------
#total number of rides by members and casual
bike_rides_v2 %>%
  group_by(member_casual) %>%
  count(member_casual)

# number of rides taken by type of bike 

bike_rides_v2 %>% 
  group_by(member_casual,rideable_type) %>%
  count(rideable_type)

# number of rides on week day

bike_rides_v2 %>%
  group_by(member_casual,day_of_week) %>%
  count(day_of_week)

# number of rides on months

bike_rides_v2 %>%
  group_by(member_casual,month) %>%
  count(month) 

# number of rides by season
bike_rides_v2 %>%
  group_by(member_casual,season) %>%
  count(season) 

# number of rides by each day

bike_rides_v2 %>%
  group_by(member_casual,day) %>%
  count(day) 

#--------------------------------------------------------------
#----------------------------Average ride length------------------

#total average ride length  by members and casual
aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual),
          FUN = mean)

#total average ride length  by members and casual by ride_able_type

aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual,bike_rides_v2$rideable_type),
          FUN = mean)

# Average ride time by each day of week for casual and members

aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual,bike_rides_v2$day_of_week),
          FUN = mean)

# Average ride time by each month for casual and members

aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual,bike_rides_v2$month),
          FUN = mean)
# Average ride time by each day of month for casual and members

aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual,bike_rides_v2$day),
          FUN = mean)

# Average ride time by season for casual and members

aggregate(bike_rides_v2$ride_length, 
          by=list(bike_rides_v2$member_casual,bike_rides_v2$season),
          FUN = mean)

#visualization of average_ride_length for each day of month categorized into weeks
bike_rides_v2 %>% 
  group_by(day_of_week,day) %>%
  summarise(number_of_rides = n() , avg_dur = mean(ride_length))%>%
  ggplot(aes(x = day,y= avg_dur,fill = day_of_week)) + geom_col() + facet_wrap(~day_of_week)

#--------------------------------WEEK DATA-----------------------------------------------------------
#analyse ridership data by type and day_of_week

bike_rides_v2 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides = n() , average_duration = mean(ride_length)) %>%
  arrange(member_casual,day_of_week)

#----------------------------------------------------------------------------------------------------
 #-------------------------------------VISULIZATION-----------------------------------------

# 1. Total rides by user type
# casual : 2048302
# member : 2539802

bike_rides_v2 %>% 
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = member_casual , y = number_of_rides,fill = member_casual)) + geom_col() 

# 2. number of rides taken by type of bike 

bike_rides_v2 %>% 
  group_by(member_casual,rideable_type) %>%
  summarise(number_of_rides = n())%>%
  ggplot(aes(x = rideable_type , y = number_of_rides,fill = member_casual)) + geom_col()  + facet_wrap(~ member_casual)

#3. number of rides per month

bike_rides_v2 %>%
  group_by(member_casual,month) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = month, y = number_of_rides,fill = member_casual)) + geom_col() + facet_wrap(~ member_casual)


# 4.Visualizing number_of_rides taken on week day by rider type

bike_rides_v2 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides = n() , average_duration = mean(ride_length)) %>%
  arrange(member_casual,day_of_week)%>%
  ggplot(aes(x = day_of_week,y=number_of_rides,fill = member_casual))+
  geom_col()  + facet_wrap(~ member_casual)

#5. number of rides by season

bike_rides_v2 %>%
  group_by(member_casual,season) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = season, y = number_of_rides,fill = member_casual)) + geom_col() + facet_wrap(~ member_casual)

# 6. number of rides per day 

bike_rides_v2 %>%
  group_by(member_casual,day) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = day, y = number_of_rides,fill = member_casual)) + geom_col() + facet_wrap(~member_casual)

#---------------------------------------------------------------------------------------------------------------------
#---------------------Visualization based upon average_ride_length

# 1. average_ride_length by user type
# casual : 1951 sec
# member : 791 sec

bike_rides_v2 %>% 
  group_by(member_casual) %>%
  summarise(avg_ride = mean(ride_length)) %>%
  ggplot(aes(x = member_casual , y = avg_ride,fill = member_casual)) + geom_col() 

# 2. average_ride_length taken by type of bike 

bike_rides_v2 %>% 
  group_by(member_casual,rideable_type) %>%
  summarise(avg_ride = mean(ride_length))%>%
  ggplot(aes(x = rideable_type , y = avg_ride,fill = member_casual)) + geom_col()  + facet_wrap(~ member_casual)

#3. average_ride_length per month

bike_rides_v2 %>%
  group_by(member_casual,month) %>%
  summarise(avg_ride = mean(ride_length)) %>%
  ggplot(aes(x = month, y = avg_ride,fill = member_casual)) + geom_col() + facet_wrap(~ member_casual)


# 4.Visualizing number_of_rides taken on week day by rider type

bike_rides_v2 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides = n() , average_duration = mean(ride_length)) %>%
  arrange(member_casual,day_of_week)%>%
  ggplot(aes(x = day_of_week,y=average_duration,fill = member_casual))+
  geom_col()  + facet_wrap(~member_casual)


#5.  average_ride_length by season

bike_rides_v2 %>%
  group_by(member_casual,season) %>%
  summarise(avg_ride = mean(ride_length))%>%
  ggplot(aes(x = season, y = avg_ride ,fill = member_casual)) + geom_col() + facet_wrap(~ member_casual)

#6. average_ride_length per day 

bike_rides_v2 %>%
  group_by(member_casual,day) %>%
  summarise(avg_ride = mean(ride_length)) %>%
  ggplot(aes(x = day, y = avg_ride ,fill = member_casual)) + geom_col() + facet_wrap(~member_casual)




