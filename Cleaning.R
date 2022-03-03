# Cleaning and processing data for analysis

#First lets inspect the new table 

colnames(bike_rides) #listing all the column names 
nrow(bike_rides) # Total number of rows in the data frame
dim(bike_rides) # Dimension of the data frame
head(bike_rides) # Displaying the first 6 rows of data frame
str(bike_rides) # List of columns and data types
summary(bike_rides) # Statistical summary of data


# converting "ride_length" from factor
#to numeric so we can run calculations on the data
is.factor(bike_rides$ride_length)

bike_rides$ride_length = as.numeric(as.character(bike_rides$ride_length))


#data cleaning

#removing all rows with NA values
bike_rides = na.omit(bike_rides)
#removing duplicates rows
bike_rides = distinct(bike_rides)
#removing ride_length where it is zero or negative
bike_rides_v2 = filter(bike_rides,bike_rides$ride_length>0)




