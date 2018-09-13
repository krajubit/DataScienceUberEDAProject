# Install the libraries if not available
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("scales")
# install.packages("gridExtra")

# Load the libraries 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(scales)
library(gridExtra)

#Set working directory where data file is available for reading
setwd("C:/Training/DataScience-Upgrad/Course2-Stastics and Exploratory Data Analytics/Assignment-UberSupplyDemand Gap-Module4/data")

#set seed for reproducibility rsults
set.seed(1234)

# Read the data set to be analyzed
uberDf <- read.csv("Uber Request Data.csv",stringsAsFactors = F ,header = T)

#Check structure of dataset and data type
View(uberDf)
head(uberDf)
str(uberDf)

####################################Data Cleaning#########################################
#Following Issues found with data after viewing the sample rows and checking the summary
#Pickup.point : It looks like factor data as it has only two values "Airport" and "City". Will convert it into factor for better analysis
#Status : It looks like factor data as it has only three values "Trip Completed", "Cancelled" and "No Cars Available". Will convert it into factor for better analysis
#Driver.id : It has 2650 NA (missing) values. Missing value exists when trip status is "No Cars Available". Since NA values exist because of business reasons , so we will keep them for our analysis.
#Request.timestamp : Field values here are like date time variable. So, we will convert to Timestamp. Also it has 2 different format values like 13-07-2016 08:33:16 and 11/7/2016  1:10. 
#                    We need to convert it to a common format for our analysis.
#Drop.timestamp    : Field values here are like date time variable. So, we will convert to Timestamp. Also it has 2 different format values like 13-07-2016 08:33:16 and 11/7/2016  1:10.
#					 We need to convert it to a common format for our analysis. 
#					 It has 3914 NA (missing) values. NA values exists when Status is Cancelled or No Cars Available.Since NA values exist because of business reasons , so we will keep them for our analysis


#Check for duplicate records in dataset
uberDfLen <- nrow(uberDf)
uberUniqueDfLen <- nrow(unique(uberDf))
print(uberDfLen)
print(uberUniqueDfLen)
diffRec <- uberDfLen - uberUniqueDfLen
if(diffRec == 0){
    print("All are unique records")
}else{
    print(paste(diffRec, " are duplicate records"))
}
#Here All records are found to be unique.

#Check if Request.id field is unique key to identify each record 
uberDfReqIdLen <- length(uberDf$Request.id)
uberDfReqIdUniqueLen <- length(unique(uberDf$Request.id))
print(uberDfReqIdLen)
print(uberDfReqIdUniqueLen)
diffCount <- uberDfReqIdLen - uberDfReqIdUniqueLen
if(diffCount == 0){
    print("All are unique records")
}else{
    print(paste(diffCount, " are duplicate values"))
}
#Here Request.id can be used as unique key to identify each record

# We need to convert Pickup.point and Status variables to factor and Request.timestamp and Drop.timestamp  to TimeStamp
#Fix any case or white space issue before converting to factor
uberDf$Pickup.point <- str_trim(toupper(uberDf$Pickup.point))
uberDf$Status <- str_trim(toupper(uberDf$Status))

#Convert Pickup.point and status to factor
uberDf$Pickup.point <- as.factor(uberDf$Pickup.point)
uberDf$Status <- as.factor(uberDf$Status)

#Pickup.point Levels: Airport , City
#Pickup.point Levels: Trip Completed, Cancelled, No Cars Available


# Convert date columns Request.timestamp  and Drop.timestamp to Timestamp variable

#Since Drop.timestamp has NA values, so we will assign some default variable to NA to avoid NA mismatch of date column length issues. Later we will revert back to NA again.
uberDf[which(is.na(uberDf$Drop.timestamp)),"Drop.timestamp"] <- "01-01-9999 00:00:00"

#We will divide our data set into three parts: 1) Col 1 to 4 , 2) Col 1 and Col 5(Request.timestamp) and 3) Col 1 and Col 6 (Drop.timestamp)
#This will allow us to convert the date related colums to single format Timestamp value and then we can merge the all 3 parts using common unique variable Request.id
uberDf1 <- uberDf[, 1:4]
uberDf2 <- uberDf[, c(1,5)]
uberDf3 <- uberDf[, c(1,6)]

#Filter the Request.timestamp data set (uberDf2) into two parts based upon two different formats and then apply data conversion method dmy_hm and dmy_hms respectively. Then combine both the parts together
uberDf2s1 <- subset(uberDf2, str_detect(uberDf2$Request.timestamp, "[0-9]/[0-9]") == TRUE)
uberDf2s2 <- subset(uberDf2, str_detect(uberDf2$Request.timestamp, "[0-9]-[0-9]") == TRUE)
uberDf2s1$Request.timestamp <- dmy_hm(uberDf2s1$Request.timestamp)
uberDf2s2$Request.timestamp <- dmy_hms(uberDf2s2$Request.timestamp)
uberDf2 <- rbind(uberDf2s1,uberDf2s2)
#Validate the number of rows should be same as total rows 6745
nrow(uberDf2) 

#Filter the Drop.timestamp data set (uberDf2) into two parts based upon two different formats and then apply data conversion method dmy_hm and dmy_hms respectively. Then combine both the parts together
uberDf3s1 <- subset(uberDf3, str_detect(uberDf3$Drop.timestamp, "[0-9]/[0-9]") == TRUE)
uberDf3s2 <- subset(uberDf3, str_detect(uberDf3$Drop.timestamp, "[0-9]-[0-9]") == TRUE)
uberDf3s1$Drop.timestamp <- dmy_hm(uberDf3s1$Drop.timestamp)
uberDf3s2$Drop.timestamp <- dmy_hms(uberDf3s2$Drop.timestamp)
uberDf3 <- rbind(uberDf3s1,uberDf3s2)
#Validate the number of rows should be same as total rows 6745
nrow(uberDf3)

#Merge all the three data sets uberDf1,uberDf2 and uberDf3
uberDfm1 <- merge(x = uberDf1, y = uberDf2, by = "Request.id")
uberDfFinal <- merge(x = uberDfm1, y = uberDf3, by = "Request.id")

#Revert back the NA values in variable Drop.timestamp
uberDfFinal[which(year(uberDfFinal$Drop.timestamp) == 9999),6] <- NA 

################################Univariate Analysis###############################################

#Validate the data structure and NA count again after data cleaning
str(uberDfFinal)
summary(uberDfFinal)

#Total number of observations = 6745
#Total Number of Variables = 6
#Request.id        - Data Type : int , Variable type : Ordered Categorical variable(Interval variables), NA = 0 
#Pickup.point      - Data Type : Factor, chr (original) , Variable type : Un-Ordered Categorical variable (Nominal variables), NA = 0 
#Driver.id         - Data Type : int , Variable type : Un-Ordered Categorical variable (Nominal variables), NA = 2650 
#Status            - Data Type : Factor, chr (original), Variable type : Ordered Categorical variable (Ordinal variables), NA = 0  	
#Request.timestamp - Data Type : Timestamp, chr (Original),Variable type : Ordered Categorical variable (Interval variables), NA = 0 
#Drop.timestamp    - Data Type : Timestamp, chr (Original),Variable type : Ordered Categorical variable (Interval variables), NA = 3914 
                 
#######################Request.id#########################
#Min.   :   1
#1st Qu.:1691
#Median :3387
#Mean   :3385 
#3rd Qu.:5080
#Max.   :6766 
requestIdRange = max(uberDfFinal$Request.id) - min(uberDfFinal$Request.id)
#Range shows spread of 6765 requests between max and min but the actual records are 6745, so their will be few missing request Ids in between which are 20 in numbers
print(requestIdRange) 

########################Pickup.point#########################
#AIRPORT:3238
#CITY   :3507

#Calculate Percentage contribution
print(length(uberDfFinal$Pickup.point))
airportPickUpPct <- (3328/length(uberDfFinal$Pickup.point))*100
cityPickUpPct <- (3507/length(uberDfFinal$Pickup.point))*100
print(airportPickUpPct)
print(cityPickUpPct)

#City pick up point contribution  is slighly more than airport pick up point contribution
#AIRPORT PERCENT:	48%
#CITY PERCENT  :	52%


########################Status#########################
#CANCELLED        :1264  
#NO CARS AVAILABLE:2650 
#TRIP COMPLETED   :2831 

#Calculate Percentage contribution
print(length(uberDfFinal$Status))
cancelPct <- (1264/length(uberDfFinal$Status))*100
noCarsPct <- (2650/length(uberDfFinal$Status))*100
tripCompPct <- (2831/length(uberDfFinal$Status))*100
print(cancelPct)
print(noCarsPct)
print(tripCompPct)

#Here we can see that combined percentage of No cars and Canceled is 58% and no cars percent is highest among both (more than double of cancelled)
#CANCELLED        : 19% 
#NO CARS AVAILABLE: 39% 
#TRIP COMPLETED   : 42 

########################Driver.id#########################
#Total Count: 	6745		
#Values populated count: 4095
#NA count : 2650

#Missing Driver percentage is 39% which is same as NO CARS AVAILABLE Percentage
missingDriverPct <- (2650/6745)*100
print(missingDriverPct)

uniqueDriverCount <- length(unique(uberDfFinal$Driver.id))

#Unique Driver Count is 301
print(uniqueDriverCount)

########################Request.timestamp########################
# Hew we can see that Request Timestamp is of type date and time which is uniformly distributed with no NA's
#Min.   :2016-07-11 00:00:0
#1st Qu.:2016-07-12 07:51:00
#Median :2016-07-13 14:23:37 
#Mean   :2016-07-13 13:43:04
#3rd Qu.:2016-07-14 19:39:27
#Max.   :2016-07-15 23:59:58

########################Drop.timestamp########################
# Hew we can see that Drop Timestamp  is of type date and has one missing day 2016-07-14  with  NA's 3914
#Min.   :2016-07-11 00:51:00 
#1st Qu.:2016-07-12 07:42:00
#Median :2016-07-13 12:14:06  
#Mean   :2016-07-13 13:15:33
#3rd Qu.:2016-07-14 19:13:52
#Max.   :2016-07-16 01:09:24 
#NA's   :3914 

#Missing Drop Timestamp Percent = 58%
missingDropTimeStampPercent <- (3914/6745)*100
print(missingDropTimeStampPercent)
##############Plots for univariate analysis#####################

###Request.id
# Divide the x range into 5 bins
binsize <- diff(range(uberDfFinal$Request.id))/5
ggplot(uberDfFinal, aes(x = Request.id)) +
  geom_histogram(binwidth = binsize, fill = "white", colour = "black") +
  labs(title = "Frequency Plot of Request Id", x = "Trip Request Id", y = "Frequency of Trip Request")

#As per Request Id histogram plot , we can see that initial frequency of request for cabs are less then its alonst constant with higher frequency 
#and then their is a dip in frequency again. Further analysis is required to see dip in frequency at the begining and end
  
###Pickup.point

## Bar Plot of Pickup Point
ggplot(uberDfFinal, aes(x=Pickup.point)) + geom_bar() + labs(title = "Bar Plot",subtitle="Frequency of Pickup Point", x = "Pickup Point", y = "Frequency of Pickup Point")

## Percentage distribution of pick up point
ggplot(uberDfFinal, aes(Pickup.point)) + geom_bar(aes(y = (..count..)/sum(..count..)*100)) + labs(y = "Pickup Point Percentage (%)", title = "Bar Plot",subtitle="Percentage Distribution of Pickup Point") 
#We can clearly see that City is slightly ahead than airport for pick up points selection
  
###Driver.id

#Populate the missing driver id with some default value to analyse the missing data also
#Check if 0 default value for missing Driver id already exists or not

print(length(which(uberDfFinal$Driver.id == 0)))
#Print missing driver id count - 2650
length(which(is.na(uberDfFinal$Driver.id)))
#Set default value of missing driver id as 0 and then count it
uberDfFinal[which(is.na(uberDfFinal$Driver.id)),"Driver.id"] <- 0
#Re-validate the missing count
print(length(which(uberDfFinal$Driver.id == 0)))

# Bar Plot of Driver for checking frequency for each driver
#We can see that average count of each driver frequency is around 15 trips for the given data
ggplot(uberDfFinal[which(uberDfFinal$Driver.id != 0),], aes(x=Driver.id)) + geom_bar(width = 2,fill="tomato2") + labs(title = "Bar Chart",subtitle="Frequency of Drivers")


# Bar Plot of Driver with missing Data (we can see that the count of missing drivers is way ahead around 2000 )
ggplot(uberDfFinal, aes(x=Driver.id)) + geom_bar(width = 5,fill="tomato2") + labs(title = "Bar Chart",subtitle="Frequency of Drivers")

#Frequency Plot 
ggplot(uberDfFinal[which(uberDfFinal$Driver.id != 0),], aes(x=Driver.id)) + geom_freqpoly() + labs(title = "Frequency plot for Driver")



###Status  
# Bar Plot of tatus
#We can see that Cancelled and No Cars available has combined higher percentage then completed
#Also No Cars available count is almost same to trip completed count.
ggplot(uberDfFinal, aes(x=Status)) + geom_bar() + labs(title = "Bar Plot",subtitle="Count of Status")
## Percentage distribution of pick up point
ggplot(uberDfFinal, aes(Status)) + geom_bar(aes(y = (..count..)/sum(..count..)*100)) + labs(y = "Status Percentage (%)",title = "Bar Plot",subtitle="Percentage Distribution of Status", x = "Trip Status")
  #We can clearly see that City is slightly ahead than airport for pick up points selection
  
###Request.timestamp
#Request Timestamp range:"2016-07-11 00:00:00 UTC" "2016-07-15 23:59:58 UTC"
print(range(uberDfFinal$Request.timestamp))
#Request Timestamp difference in range: 5 days
print(diff(range(uberDfFinal$Request.timestamp)))

#We can see that number of Trip Requests  per day is almost constant
ggplot(uberDfFinal, aes(x=Request.timestamp)) + geom_histogram(bins = 5) + labs(title = "Histogram Chart",subtitle="Trips Requests by Request Timestamp")


#Count of requests on 11 Jul : 1367
paste("Count of requests on 11 Jul : ", nrow(subset(uberDfFinal, uberDfFinal$Request.timestamp < ymd_hms("2016-07-12 00:00:00") & uberDfFinal$Request.timestamp >= ymd_hms("2016-07-11 00:00:00"))))
#Count of requests on 12 Jul : 1307
paste("Count of requests on 12 Jul : ", nrow(subset(uberDfFinal, uberDfFinal$Request.timestamp < ymd_hms("2016-07-13 00:00:00") & uberDfFinal$Request.timestamp >= ymd_hms("2016-07-12 00:00:00"))))
#Count of requests on 13 Jul : 1337
paste("Count of requests on 13 Jul : ", nrow(subset(uberDfFinal, uberDfFinal$Request.timestamp < ymd_hms("2016-07-14 00:00:00") & uberDfFinal$Request.timestamp >= ymd_hms("2016-07-13 00:00:00"))))
#Count of requests on 14 Jul : 1353
paste("Count of requests on 14 Jul : ", nrow(subset(uberDfFinal, uberDfFinal$Request.timestamp < ymd_hms("2016-07-15 00:00:00") & uberDfFinal$Request.timestamp >= ymd_hms("2016-07-14 00:00:00"))))
#Count of requests on 15 Jul : 1381
paste("Count of requests on 15 Jul : ", nrow(subset(uberDfFinal, uberDfFinal$Request.timestamp < ymd_hms("2016-07-16 00:00:00") & uberDfFinal$Request.timestamp >= ymd_hms("2016-07-15 00:00:00"))))

###Drop.timestamp
#Drop Timestamp range:"2016-07-11 00:51:00 UTC" "2016-07-16 01:09:24 UTC"
print(range(uberDfFinal$Drop.timestamp,na.rm = T))
#Drop Timestamp difference in range: 5 days
print(diff(range(uberDfFinal$Drop.timestamp,na.rm = T)))

#Analyse completed drop trips
uberCompletedTrips <- uberDfFinal[which(!is.na(uberDfFinal$Drop.timestamp)),]
#We can see that number of drop completion is alomost same rate most of the time with sligh up and down. 
#Also number of drop request completed per day is almost halsf of number of request raised which means many request are not getting converted to completion
ggplot(uberCompletedTrips, aes(x=Drop.timestamp)) + geom_histogram(bins = 5) + labs(title = "Histogram Chart",subtitle="Trips completed by Drop Timestamp")

#Count of drop requests on 11 Jul : 582
paste("Count of requests on 11 Jul : ", nrow(subset(uberCompletedTrips, uberCompletedTrips$Drop.timestamp < ymd_hms("2016-07-12 00:00:00") & uberCompletedTrips$Drop.timestamp >= ymd_hms("2016-07-11 00:00:00"))))
#Count of drop requests on 12 Jul : 568
paste("Count of drop requests on 12 Jul : ", nrow(subset(uberCompletedTrips, uberCompletedTrips$Drop.timestamp < ymd_hms("2016-07-13 00:00:00") & uberCompletedTrips$Drop.timestamp >= ymd_hms("2016-07-12 00:00:00"))))
#Count of drop requests on 13 Jul : 569
paste("Count of requests on 13 Jul : ", nrow(subset(uberCompletedTrips, uberCompletedTrips$Drop.timestamp < ymd_hms("2016-07-14 00:00:00") & uberCompletedTrips$Drop.timestamp >= ymd_hms("2016-07-13 00:00:00"))))
#Count of drop requests on 14 Jul :  536
paste("Count of requests on 14 Jul : ", nrow(subset(uberCompletedTrips, uberCompletedTrips$Drop.timestamp < ymd_hms("2016-07-15 00:00:00") & uberCompletedTrips$Drop.timestamp >= ymd_hms("2016-07-14 00:00:00"))))
#Count ofdrop  requests on 15 Jul : 576
paste("Count of requests on 15 Jul : ", nrow(subset(uberCompletedTrips, uberCompletedTrips$Drop.timestamp < ymd_hms("2016-07-16 02:00:00") & uberCompletedTrips$Drop.timestamp >= ymd_hms("2016-07-15 00:00:00"))))

#Analyse missing drop timestamp
#We can see that number of drop completion is alomost same rate most of the time with sligh up and down. 
uberNotCompletedTrips <- uberDfFinal[which(is.na(uberDfFinal$Drop.timestamp)),]
ggplot(uberNotCompletedTrips, aes(x=Request.timestamp)) + geom_histogram(bins = 5) + labs(title = "Histogram Chart",subtitle="Trips did not complete by Request Timestamp")

#Count of requests on 11 Jul : 766
paste("Count of not completed requests on 11 Jul : ", nrow(subset(uberNotCompletedTrips, uberNotCompletedTrips$Request.timestamp < ymd_hms("2016-07-12 00:00:00") & uberNotCompletedTrips$Request.timestamp >= ymd_hms("2016-07-11 00:00:00"))))
#Count of requests on 12 Jul : 745
paste("Count of not completed requests on 12 Jul : ", nrow(subset(uberNotCompletedTrips, uberNotCompletedTrips$Request.timestamp < ymd_hms("2016-07-13 00:00:00") & uberNotCompletedTrips$Request.timestamp >= ymd_hms("2016-07-12 00:00:00"))))
#Count of requests on 13 Jul : 760
paste("Count of not completed requests on 13 Jul : ", nrow(subset(uberNotCompletedTrips, uberNotCompletedTrips$Request.timestamp < ymd_hms("2016-07-14 00:00:00") & uberNotCompletedTrips$Request.timestamp >= ymd_hms("2016-07-13 00:00:00"))))
#Count of requests on 14 Jul : 823
paste("Count of not completed requests on 14 Jul : ", nrow(subset(uberNotCompletedTrips, uberNotCompletedTrips$Request.timestamp < ymd_hms("2016-07-15 00:00:00") & uberNotCompletedTrips$Request.timestamp >= ymd_hms("2016-07-14 00:00:00"))))
#Count of requests on 15 Jul : 820
paste("Count of not completed requests on 15 Jul : ", nrow(subset(uberNotCompletedTrips, uberNotCompletedTrips$Request.timestamp < ymd_hms("2016-07-16 00:00:00") & uberNotCompletedTrips$Request.timestamp >= ymd_hms("2016-07-15 00:00:00"))))


##############Bivariate analysis#####################

##Bar Plot between requests in each Pickup.point distributed by Status
##Combined status of CANCELLED and NO CARS AVAILABLE is almost same in city and Airport pickup points

ggplot(uberDfFinal, aes(x=Pickup.point, fill = Status)) + geom_bar() +  labs(title = "Bar Plot",subtitle="Distribution of status per Pickup.point")

##Scatter plot betwwen Request.timestamp and Drop.timestamp which shows a linear relationship
ggplot(uberDfFinal, aes(x=Request.timestamp, y = Drop.timestamp)) + geom_point() + geom_smooth() +  labs(title = "Scatter Plot",subtitle="Request Timestamp Vs Drop Timestamp")

#Corelation between request id and Driver id is slighly negative : -0.06690595 
# It means we don't have driver for each pick up request raised

print(cor(uberDfFinal$Request.id,uberDfFinal$Driver.id))

##############Derived Metrics#####################
#Pickup Point - PickupIndicator 1 (city) and 0 (Airport)
#Driver Id - MissingDriverIndicator 1 (Available) and 0 (Missing)
#Status - Status Indicator 1 (TRIP COMPLETED) and 0 (CANCELLED or NO CARS AVAILABLE )
#Request.timestamp - Date, Day, Month, Year, Hour, Min , Second, Weekday, 
#                    TimingOfDay (Morning,Afternoon,Evening and Night)
#Drop.timestamp -  Date, Day, Month, Year, Hour, Min , Second, Weekday,
#                  TimingOfDay (Morning,Afternoon,Evening and Night), MissingDropTimeInd  (1 - Available, 0 - Missing)

#Function to populate Pickup Point Indicator
populatePickUpIndicator <- function(pickupPoint){
  if(pickupPoint == "CITY") {
    return(1)
  }
  else {
    return(0)
  }
}

#Function to populate Timing of Day  
populateTimingOfDay <- function(hours){
  if(hours >= 5 & hours < 12) {
    return("Morning")
  }
  else if(hours >= 12 & hours <= 16){
    return("Afternoon")
  }
  else if(hours > 16 & hours <= 21){
    return("Evening")
  }
  else {
    return("Night")
  }
}

#Function to populate Missing Driver Indicator
populateMissingDriverIndicator <- function(driverId){
  if(driverId == 0) {
    return(0)
  }
  else {
    return(1)
  }
}

#Function to populate Status Indicator
populateStatusIndicator <- function(status){
  if(status == "TRIP COMPLETED") {
    return(1)
  }
  else {
    return(0)
  }
}

#Function to populate Missing Drop Timestamp Indicator
populateMissingDropTimeIndicator <- function(dropTimesatmp){
  if(is.na(dropTimesatmp)) {
    return(0)
  }
  else {
    return(1)
  }
}

#Add a new variable PickupIndicator to populate 1 for city and 0 for Airport
uberDfFinal$PickupIndicator <- lapply(uberDfFinal$Pickup.point,populatePickUpIndicator)

#Add a new variable MissingDriverIndicator to populate 1 for missing Driver and 0 for available
uberDfFinal$MissingDriverIndicator <- lapply(uberDfFinal$Driver.id,populateMissingDriverIndicator)

#Add a new variable StatusIndicator to populate 1 for "TRIP COMPLETED" and 0 for other values
uberDfFinal$StatusIndicator <- lapply(uberDfFinal$Status,populateStatusIndicator)

##Derived Metrics from Request.timestamp variable

#Add a new variable RequestDate to populate date from request timestamp
uberDfFinal$RequestDate <- lubridate::date(uberDfFinal$Request.timestamp)

#Add a new variable RequestDay to populate day from request timestamp
uberDfFinal$RequestDay <- lubridate::day(uberDfFinal$Request.timestamp)

#Add a new variable RequestMonth to populate month from request timestamp
uberDfFinal$RequestMonth <- lubridate::month(uberDfFinal$Request.timestamp)

#Add a new variable RequestYear to populate year from request timestamp
uberDfFinal$RequestYear <- lubridate::year(uberDfFinal$Request.timestamp)

#Add a new variable RequestHour to populate hour from request timestamp
uberDfFinal$RequestHour <- lubridate::hour(uberDfFinal$Request.timestamp)

#Add a new variable RequestMinute to populate minute from request timestamp
uberDfFinal$RequestMinute <- lubridate::minute(uberDfFinal$Request.timestamp)

#Add a new variable RequestSecond to populate second from request timestamp
uberDfFinal$RequestSecond <- lubridate::second(uberDfFinal$Request.timestamp)

#Add a new variable RequestWeekDay to populate day name from request date
uberDfFinal$RequestWeekDay <- weekdays(uberDfFinal$RequestDate, abbreviate = FALSE)

#Add a new variable RequestTimingOfDay to populate Morning,Afternnon,Evening and Night based upon hour time
uberDfFinal$RequestTimingOfDay <- lapply(uberDfFinal$RequestHour,populateTimingOfDay)
View(uberDfFinal)


##Derived Metrics from Drop.timestamp variable

#Populate Missing Drop.timestamp with Request Date as most of the times Pick up and drop will be on same day
#Check the number of pick up requests made with hour =  23 and minutes > 0 (to check last hour request just before the change of the day)
View(subset(uberDfFinal,RequestHour == 23 & RequestMinute > 0))
#By checking above data, we can safely assume that the date part of missing Drop time stamp can be same as Request Date part without much impact

#Populate the indicator first for missing Drop Timestamp 
uberDfFinal$MissingDropTimeIndicator <- lapply(uberDfFinal$Drop.timestamp,populateMissingDropTimeIndicator)

#Populate Missing Drop timestamp with date part of Request Timestamp. Additional we will add one indicator to identify that this belongs to missing data
uberDfFinal[which(is.na(uberDfFinal$Drop.timestamp)),"Drop.timestamp"] <- uberDfFinal[which(is.na(uberDfFinal$Drop.timestamp)),"RequestDate"]

#Add a new variable DropDate to populate date from request timestamp
uberDfFinal$DropDate <- lubridate::date(uberDfFinal$Drop.timestamp)

#Add a new variable RequestDay to populate day from request timestamp
uberDfFinal$DropDay <- lubridate::day(uberDfFinal$Drop.timestamp)

#Add a new variable RequestMonth to populate month from request timestamp
uberDfFinal$DropMonth <- lubridate::month(uberDfFinal$Drop.timestamp)

#Add a new variable RequestYear to populate year from request timestamp
uberDfFinal$DropYear <- lubridate::year(uberDfFinal$Drop.timestamp)

#Add a new variable RequestHour to populate hour from request timestamp
uberDfFinal$DropHour <- lubridate::hour(uberDfFinal$Drop.timestamp)

#Add a new variable RequestMinute to populate minute from request timestamp
uberDfFinal$DropMinute <- lubridate::minute(uberDfFinal$Drop.timestamp)

#Add a new variable RequestSecond to populate second from request timestamp
uberDfFinal$DropSecond <- lubridate::second(uberDfFinal$Drop.timestamp)

#Add a new variable RequestWeekDay to populate day name from request date
uberDfFinal$DropWeekDay <- weekdays(uberDfFinal$DropDate, abbreviate = FALSE)

#Add a new variable RequestTimingOfDay to populate Morning,Afternnon,Evening and Night based upon hour time
uberDfFinal$DropTimingOfDay <- lapply(uberDfFinal$DropHour,populateTimingOfDay)

#Convert the discrete categorical variables into factors
#Their are slighly more number of request with city as pickup point compare to Airport
uberDfFinal$PickupIndicator <- as.factor(unlist(uberDfFinal$PickupIndicator))
summary(uberDfFinal$PickupIndicator)

#Their are many cases of drivers not availble due to no cars
uberDfFinal$MissingDriverIndicator <- as.factor(unlist(uberDfFinal$MissingDriverIndicator))
summary(uberDfFinal$MissingDriverIndicator)

#Status with CANCELLED or NO CARS AVAILABLE is much higher than trip completed
uberDfFinal$StatusIndicator <- as.factor(unlist(uberDfFinal$StatusIndicator))
summary(uberDfFinal$StatusIndicator)

#Timing of the day
uberDfFinal$RequestTimingOfDay <- as.factor(unlist(uberDfFinal$RequestTimingOfDay))
summary(uberDfFinal$RequestTimingOfDay)

#Their are many cases of drivers not availble due to no cars
uberDfFinal$MissingDropTimeIndicator <- as.factor(unlist(uberDfFinal$MissingDropTimeIndicator))
summary(uberDfFinal$MissingDropTimeIndicator)

#No of pick up requests are almost constant date wise
uberDfFinal$RequestDate <- as.factor(uberDfFinal$RequestDate)
summary(uberDfFinal$RequestDate)

#No of pick up requests are almost constant day wise which is same as date wise
uberDfFinal$RequestDay <- as.factor(uberDfFinal$RequestDay)
summary(uberDfFinal$RequestDay)

#Can see the demand is very high between 5-9 hour (Early Morning) and then evening hours (17 -22 hour)
uberDfFinal$RequestHour <- as.factor(uberDfFinal$RequestHour)
summary(uberDfFinal$RequestHour)

#No of Drop requests are almost constant date wise except sharp fall at the end
uberDfFinal$DropDate <- as.factor(uberDfFinal$DropDate)
summary(uberDfFinal$DropDate)

#No of Drop requests are almost constant date wise except sharp fall at the end
uberDfFinal$DropDay <- as.factor(uberDfFinal$DropDay)
summary(uberDfFinal$DropDay)

#Can see the demand is very high between 6-10 hour (Morning) and then evening and night hours (18 -23 hour)
#Their is sharp increase in drop time count at 0th hour which might be because of not completed trips
uberDfFinal$DropHour <- as.factor(uberDfFinal$DropHour)
summary(uberDfFinal$DropHour)

#Check corelation between derived metrics Staus Indicator and Missing Driver Indicator
#It shows as positive corelation as 1. So if Status indicator is 0 
#which means trip not completed then MissingDriverIndicator is also 0 and vice versa
cor(as.numeric(uberDfFinal$StatusIndicator),as.numeric(uberDfFinal$MissingDropTimeIndicator))

##Below code is to plot the count of not completed trips by days first and then by hours
uberNotCompletedTrips2 <- subset(uberDfFinal, MissingDropTimeIndicator == 0)
nrow(uberNotCompletedTrips2)

#Plot to see the day wise hourly distribution of all trip requests
#14-Jul  and 15-Jul  has slightly higher count of combined(Trip cancellation or cars not available)
#Also Trips are not getting completed more on moring and evening hours
ggplot(uberNotCompletedTrips2, aes(x=RequestDate,fill=RequestHour)) + geom_bar()  + labs(title = "Bar Chart",subtitle="Date wise Count of not completed trips ")


#Plot to see the day wise hourly distribution of all not completed trip requests
#Here we can clearly see that morning hours and evening hours have more trip cancellation and cars not available
ggplot(uberNotCompletedTrips2, aes(x=RequestHour,fill = RequestTimingOfDay )) + geom_bar() +  geom_text(stat='count',aes(label=..count..),vjust=-1)  + labs(title = "Bar Chart",subtitle="Hour wise Count of not completed trips ", y = "Count of Not Completed Trips", x = "Time Slot")

##Plots to visualise the frequency of requests that get cancelled by hour grouped by each day
# Here we can clearly see the number of cancellations are very high in morning time irespective of the day
ggplot(uberDfFinal[which(uberDfFinal$Status == "CANCELLED"),], aes(x=RequestHour,fill=factor(RequestDay))) + geom_bar()  + labs(title = "Bar Chart",subtitle="Hour wise frequency of requests that get cancelled")

##Plots to visualise the frequency of requests that get cancelled by hour with total counts
ggplot(uberDfFinal[which(uberDfFinal$Status == "CANCELLED"),], aes(x=RequestHour)) + geom_bar(fill="tomato3") +  geom_label(stat='count',aes(label=..count..),vjust=-0.2)  + labs(title = "Bar Chart",subtitle="Hour wise frequency of requests that get cancelled",y = "Count of Canceled Trips")

##Plots to visualise the frequency of requests that has No Cars available by hour grouped by each day
# Here we can clearly see the number of No Cars available are very high in evening time (5 pm to 9 pm) irespective of the day
ggplot(uberDfFinal[which(uberDfFinal$Status == "NO CARS AVAILABLE"),], aes(x=RequestHour,fill=factor(RequestDay))) + geom_bar()  + labs(title = "Bar Chart",subtitle="Hour wise frequency of requests that has No Cars available")

##Plots to visualise the frequency of requests that has No Cars available by hour 
# Here we can clearly see the number of No Cars available are very high in evening time (5 pm to 9 pm)
ggplot(uberDfFinal[which(uberDfFinal$Status == "NO CARS AVAILABLE"),], aes(x=RequestHour)) + geom_bar(fill="tomato3") +  geom_label(stat='count',aes(label=..count..),vjust=-0.2)  + labs(title = "Bar Chart",subtitle="Hour wise frequency of requests that  has No Cars available",y = "Count of No Cars Available Trips")

##Plots to visualise the Hour wise distribution of pickup points for not complted trips
# Here we can clearly see the number of not completed trips are very high in evening (5 pm to 9 pm) for Airport pick up and high in morning (5 am to 9 am) for city pickup
ggplot(uberDfFinal[which(uberDfFinal$StatusIndicator == 0),], aes(x=RequestHour,fill=factor(Pickup.point))) + geom_bar()  + labs(title = "Bar Chart",subtitle="Hour wise distribution of pickup points for not complted trips")

##Plots to visualise the Hour wise distribution of pickup points for cenceled trips
# Here we can clearly see the number of not CANCELLED trips are very high in morning(5 am to 9 am) for city  pick up  compared to Airport pickup
ggplot(uberDfFinal[which(uberDfFinal$Status == "CANCELLED"),], aes(x=RequestHour,fill=factor(Pickup.point))) + geom_bar()  + labs(title = "Bar Chart",subtitle="Hour wise distribution of pickup points for not complted trips and status Cancelled")

##Plots to visualise the Hour wise distribution of pickup points for NO CARS AVAILABLE trips
# Here we can clearly see the number of not NO CARS AVAILABLE trips are very high in evening (5 pm to 9 pm) for airport  pick up  compared to City pickup
ggplot(uberDfFinal[which(uberDfFinal$Status == "NO CARS AVAILABLE"),], aes(x=RequestHour,fill=factor(Pickup.point))) + geom_bar()  + labs(title = "Bar Chart",subtitle="Hour wise distribution of pickup points for not complted trips and status No Cars Available")

#Plot to visualize reason for not completed trips for different pick up points
# Here we can clearly see that the Airport pick up has problem due to no cars available and City pick up has more Cancellation issue
ggplot(uberDfFinal[which(uberDfFinal$StatusIndicator == 0),], aes(x=Pickup.point,fill=factor(Status))) + geom_bar(position = "dodge") +  geom_label(stat='count',aes(label=..count..),vjust=-0.2, hjust=0.5) + labs(title = "Bar Chart",subtitle="Hour wise distribution of pickup points for not complted trips",y = "Count of Pickup Points")

#Plot to visualize Timeslot wise distribution of not complted trips for Pickup Points
ggplot(uberDfFinal[which(uberDfFinal$Status == "CANCELLED"),], aes(x=RequestTimingOfDay,fill=factor(Pickup.point))) + geom_bar(position = "dodge")  + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips for Pickup Points and status Cancelled")

#Plot to visualize Timeslot wise distribution of not complted trips for Pickup Points
ggplot(uberDfFinal[which(uberDfFinal$Status == "NO CARS AVAILABLE"),], aes(x=RequestTimingOfDay,fill=factor(Pickup.point))) + geom_bar(position = "dodge")  + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips for Pickup Points and status Cars not Available", x = "Time Slot")


#Plot to visualize Timeslot wise distribution of not complted trips for Pickup Points
ggplot(uberDfFinal[which(uberDfFinal$StatusIndicator == 0),], aes(x=RequestTimingOfDay,fill=factor(Pickup.point))) + geom_bar(position = "dodge")  + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips for Pickup Points")

#Plot to visualize Timeslot wise distribution of not complted trips count where 0 means not completed and 1 means completed
ggplot(uberDfFinal, aes(x=RequestTimingOfDay,fill=StatusIndicator)) + geom_bar(position = "dodge")  + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips count", x = "Not Completed Trip Percentage Count", y = "Time Slot", fill = "Trip Not Completed: 0 & Trip Completed: 1" )

#Plot to visualize Timeslot wise distribution of not complted trips percentage where 0 means not completed and 1 means completed
ggplot(uberDfFinal, aes(x=RequestTimingOfDay, y = (..count..)/sum(..count..)*100,fill=factor(MissingDropTimeIndicator))) + geom_bar(position = "dodge") + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips Percentage", y = "Not Completed Trip Percentage (%)", x = "Time Slot", fill = "Trip Not Completed: 0 & Trip Completed: 1" )

#Plot to visualize Timeslot wise distribution of missing drivers count where 0 means missing and 1 means driver id populated
ggplot(uberDfFinal, aes(x=RequestTimingOfDay,fill=MissingDriverIndicator)) + geom_bar(position = "dodge")  + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips count", y = "Not Completed Trip Count", x = "Time Slot", fill = "Trip Not Completed: 0 & Trip Completed: 1" )

#Plot to visualize Timeslot wise distribution of missing drivers percentage where 0 means missing and 1 means driver id populated
#Here we can see that Evening and Night have more Drivers not available which means no cars.  Morning is slighly less than Night.
ggplot(uberDfFinal, aes(x=RequestTimingOfDay, y = (..count..)/sum(..count..)*100,fill=factor(MissingDriverIndicator))) + geom_bar(position = "dodge") + scale_colour_manual(values = c("yellow", "pink", "blue")) + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips Percentage Per Time Slot", y = "Not Completed Trip Percentage (%)", x = "Time Slot", fill = "Trip Not Completed: 0 & Trip Completed: 1" )

#Plot to visualize Timeslot wise distribution of missing drivers percentage groupd by trip status where 0 means missing and 1 means driver id populated
#Here we can see that Evening and Night have more Drivers not coming up in evening and night but cancellation is more in morning.
#So, when we combine driver not available and driver cancelling the trip then impact is more in morning and evening time.
ggplot(uberDfFinal, aes(x=RequestTimingOfDay, y = (..count..)/sum(..count..)*100,color=Status,fill=factor(MissingDriverIndicator))) + geom_bar(position = "dodge") + scale_colour_manual(values = c("yellow", "pink", "blue")) + labs(title = "Bar Chart",subtitle="Timeslot wise distribution of not complted trips Percentage", y = "Not Completed Trip Percentage (%)", x = "Time Slot", fill = "Trip Not Completed: 0 & Trip Completed: 1" )


# Highest number of trips not getting completed is in between morning 5-9 am and evening 5-9 pm among 

##Below code is to plot the percentage of not completed trips with in each time slots (Morning, Afternoon, Evening and Night)
## Here notCompletedTripPerTimeSlotPercentage = (notCompletedTripPerTimeSlotCount/countOfAllRequestsReceivedWithInThatTimeSlot)



#####Analysis for morning hours Trip Completion Percentage for all pick up requests received during that time.####
uberDfMrng <- subset(uberDfFinal, as.numeric(RequestHour) >= 5 & as.numeric(RequestHour) <= 11)
numOfRequestInMorningHours <- nrow(uberDfMrng)
#numOfRequestInMorningHours = 2549
print(numOfRequestInMorningHours)

numOfCompletedTripsDf <- subset(uberDfMrng, as.numeric(uberDfMrng$MissingDropTimeIndicator) == 1)
numOfCompletedTripsInMornigHours <- nrow(numOfCompletedTripsDf)
#numOfCompletedTripsInMornigHours = 1048
print(numOfCompletedTripsInMornigHours)
completedTripsPercentageInMrngHrs <- (numOfCompletedTripsInMornigHours/numOfRequestInMorningHours)*100
#completedTripsPercentageInMrngHrs : 41.11 %
paste("Percentage of Completed Trips available between 5 am to 9 am for all requests received : ", completedTripsPercentageInMrngHrs)
numOfMissingTripsInMornigHours <- numOfRequestInMorningHours -  numOfCompletedTripsInMornigHours
print(numOfMissingTripsInMornigHours)
percentageOfTripsMissing <- 100 - completedTripsPercentageInMrngHrs
#numOfMissingTripsInMornigHours : 1501
#percentageOfTripsMissing : 58.89 %
paste("Percentage of Trips missing between 5 am to 9 am for all requests received : ", percentageOfTripsMissing)

#####Analysis for Afternoon hours Trip Completion Percentage for all pick up requests received during that time.####

uberDfAft <- subset(uberDfFinal, as.numeric(RequestHour) >= 11 & as.numeric(RequestHour) < 17)
numOfRequestInAfternnonHours <- nrow(uberDfAft)
#numOfRequestInAfternnonHours = 1065
print(numOfRequestInAfternnonHours)

numOfCompletedTripsDf <- subset(uberDfAft, as.numeric(MissingDropTimeIndicator) == 1)
numOfCompletedTripsInAfternoonHours <- nrow(numOfCompletedTripsDf)
#numOfCompletedTripsInAfternoonHours = 631
print(numOfCompletedTripsInAfternoonHours)
completedTripsPercentageInAftHrs <- (numOfCompletedTripsInAfternoonHours/numOfRequestInAfternnonHours)*100
#completedTripsPercentageInAftHrs : 59.25  %
paste("Percentage of Completed Trips available between 12 pm to 4 pm for all requests received : ", completedTripsPercentageInAftHrs)
numOfMissingTripsInAfternoonHours <- numOfRequestInAfternnonHours -  numOfCompletedTripsInAfternoonHours
print(numOfMissingTripsInAfternoonHours)
percentageOfTripsMissing <- 100 - completedTripsPercentageInAftHrs
#numOfMissingTripsInAfternoonHours : 434 
#percentageOfTripsMissing : 40.75 %
paste("Percentage of Trips missing between 12 pm to 4 pm for all requests received : ", percentageOfTripsMissing)


#####Analysis for evening hours Trip Completion Percentage for all pick up requests received during that time.####

uberDfEvng <- subset(uberDfFinal, as.numeric(RequestHour) >= 17 & as.numeric(RequestHour) < 22)
numOfRequestInEvngHours <- nrow(uberDfEvng)
#numOfRequestInEvngHours = 2052
print(numOfRequestInEvngHours)

numOfCompletedTripsDf <- subset(uberDfEvng, as.numeric(uberDfEvng$MissingDropTimeIndicator) == 1)
numOfCompletedTripsInEvngHours <- nrow(numOfCompletedTripsDf)
#numOfCompletedTripsInEvngHours = 733
print(numOfCompletedTripsInEvngHours)
completedTripsPercentageInEvngHrs <- (numOfCompletedTripsInEvngHours/numOfRequestInEvngHours)*100
#completedTripsPercentageInEvngHrs : 35.72 %
paste("Percentage of Completed Trips available between 5 pm to 9 pm for all requests received : ", completedTripsPercentageInEvngHrs)
numOfMissingTripsInEvngHours <- numOfRequestInEvngHours -  numOfCompletedTripsInEvngHours
print(numOfMissingTripsInEvngHours)
percentageOfTripsMissing <- 100 - completedTripsPercentageInEvngHrs
#numOfMissingTripsInEvngHours : 1319
#percentageOfTripsMissing : 64.28 %
paste("Percentage of Trips missing between 5 pm to 9 pm for all requests received : ", percentageOfTripsMissing)

#####Analysis for night hours Trip Completion Percentage for all pick up requests received during that time.####

uberDfNight <- subset(uberDfFinal, (as.numeric(RequestHour) > 21 & as.numeric(RequestHour) < 24) | (as.numeric(RequestHour) >= 0 & as.numeric(RequestHour) < 5))
numOfRequestInNightHours <- nrow(uberDfNight)
#numOfRequestInNightHours = 1128
print(numOfRequestInNightHours)

numOfCompletedTripsDf <- subset(uberDfNight, as.numeric(uberDfNight$MissingDropTimeIndicator) == 1)
numOfCompletedTripsInNightHours <- nrow(numOfCompletedTripsDf)
#numOfCompletedTripsInNightHours = 432
print(numOfCompletedTripsInNightHours)
completedTripsPercentageInNightHrs <- (numOfCompletedTripsInNightHours/numOfRequestInNightHours)*100
#completedTripsPercentageInNightHrs : 38.30 %
paste("Percentage of Completed Trips available between 10 pm am to 4 am for all requests received : ", completedTripsPercentageInNightHrs)
numOfMissingTripsInNightHours <- numOfRequestInNightHours -  numOfCompletedTripsInNightHours
print(numOfMissingTripsInNightHours)
percentageOfTripsMissing <- 100 - completedTripsPercentageInNightHrs
#numOfMissingTripsInNightHours : 696
#percentageOfTripsMissing : 61.70 %
paste("Percentage of Trips missing between 10 pm to 4 am for all requests received : ", percentageOfTripsMissing)

#Graph Plot between Time of Day and Trips Not Completed Percentage Per Time Slot 

timeOfDay <- c("Morning","Afternoon","Evening","Night")
missingTripsPercentage <- c("58.89","40.75","64.28","61.70")
missingTripsPercentage <- as.numeric(missingTripsPercentage)
missingTripsPctByTimeOfDay <- cbind(timeOfDay,missingTripsPercentage)
missingTripsPctByTimeOfDay <- as.data.frame(missingTripsPctByTimeOfDay)
missingTripsPctByTimeOfDay$timeOfDay = as.factor(missingTripsPctByTimeOfDay$timeOfDay)
missingTripsPctByTimeOfDay$missingTripsPercentage = as.numeric(as.character(missingTripsPctByTimeOfDay$missingTripsPercentage))
str(missingTripsPctByTimeOfDay)

#Here, we can see that maximum trip not completed percentage is in Evening , then night, then morning and last afternoon. 
#Morinig and Night trip non completion percentage are almost same with slight difference.
missingTripsPctPlotByTimeOfDay <- ggplot(missingTripsPctByTimeOfDay, aes(x = timeOfDay, y = missingTripsPercentage,fill = factor(timeOfDay))) + geom_bar(stat = "identity") +  geom_text(aes(label=missingTripsPercentage),vjust=-0.5) + labs(title = "Bar Chart",subtitle="Trips not Completed Percentage Per Time Slot Vs Time of Day", y = "Trip Completion Per Time Slot Percentage(%)", x= "Time Slot")
missingTripsPctPlotByTimeOfDay

#Plot Missing Trip Count per Time Slot
timeOfDay <- c("Morning","Afternoon","Evening","Night")
missingTripsCount <- c("1501","434","1319","696")
missingTripsCount <- as.numeric(missingTripsCount)
missingTripsCntByTimeOfDay <- cbind(timeOfDay,missingTripsCount)
missingTripsCntByTimeOfDay <- as.data.frame(missingTripsCntByTimeOfDay)
missingTripsCntByTimeOfDay$timeOfDay = as.factor(missingTripsCntByTimeOfDay$timeOfDay)
missingTripsCntByTimeOfDay$missingTripsCount = as.numeric(as.character(missingTripsCntByTimeOfDay$missingTripsCount))
str(missingTripsCntByTimeOfDay)

#Here, we can see that maximum trip not completed percentage is in Evening , then night, then morning and last afternoon. 
#Morinig and Night trip non completion percentage are almost same with slight difference.
missingTripsCntPlotByTimeOfDay <- ggplot(missingTripsCntByTimeOfDay, aes(x = timeOfDay, y = missingTripsCount,fill = factor(timeOfDay))) + geom_bar(stat = "identity") +  geom_text(aes(label=missingTripsCount),vjust=-0.5) + labs(title = "Bar Chart",subtitle="Trips not Completed Count Per Time Slot Vs Time of Day", y = "Trip Completion Per Time Slot Count", x= "Time Slot")
missingTripsCntPlotByTimeOfDay

#Arrange Missing Trip  Analysis related plots in single plot
grid.arrange(missingTripsCntPlotByTimeOfDay, missingTripsPctPlotByTimeOfDay, ncol=2)

#Hypothesis based upon above result
# In evening , Percentage of trips not completed w.r.t overall requests  might be less compared to other times of the day because of less number of drivers 
# and more cancellation which might be the reason for higher non completion of trips in evening time.
#Lets validate it by checking the actual percentage

##############Missing Driver Percentage Analysis per Time of Day#################################

##Below code is to plot the percentage of missing drivers with in each time slots (Morning, Afternoon, Evening and Night)
## Here missingDriversPerTimeSlotPercentage = (missingDriversPerTimeSlotCount/countOfAllRequestsReceivedWithInThatTimeSlot)


#####Analysis for morning hours Driver availability for all pick up requests received during that time.####
uberDfMrng <- subset(uberDfFinal, as.numeric(RequestHour) >= 5 & as.numeric(RequestHour) <= 11)
numOfRequestInMorningHours <- nrow(uberDfMrng)
#numOfRequestInMorningHours = 2549
print(numOfRequestInMorningHours)

numOfAvailableDriversDf <- subset(uberDfMrng, as.numeric(uberDfMrng$Driver.id) != 0)
numOfAvailableDriversInMornigHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInMornigHours = 2004
print(numOfAvailableDriversInMornigHours)
availableDriverPercentageInMrngHrs <- (numOfAvailableDriversInMornigHours/numOfRequestInMorningHours)*100
#availableDriverPercentageInMrngHrs : 78.62 %
paste("Percentage of Driver available between 5 am to 9 am for all requests received : ", availableDriverPercentageInMrngHrs)
numOfMissingDriversInMornigHours <- numOfRequestInMorningHours -  numOfAvailableDriversInMornigHours
print(numOfMissingDriversInMornigHours)
percentageOfDriversMissing <- 100 - availableDriverPercentageInMrngHrs
#numOfMissingDriversInMornigHours : 545
#percentageOfDriversMissing : 21.38 %
paste("Percentage of Driver missing between 5 am to 9 am for all requests received : ", percentageOfDriversMissing)

#####Analysis for Afternoon hours Driver availability for all pick up requests received during that time.####

uberDfAft <- subset(uberDfFinal, as.numeric(RequestHour) > 11 & as.numeric(RequestHour) < 17)
numOfRequestInAfternnonHours <- nrow(uberDfAft)
#numOfRequestInAfternnonHours = 822
print(numOfRequestInAfternnonHours)

numOfAvailableDriversDf <- subset(uberDfAft, as.numeric(uberDfAft$Driver.id) != 0)
numOfAvailableDriversInAfternoonHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInAfternoonHours = 599
print(numOfAvailableDriversInAfternoonHours)
availableDriverPercentageInAftHrs <- (numOfAvailableDriversInAfternoonHours/numOfRequestInAfternnonHours)*100
#availableDriverPercentageInAftHrs : 72.87  %
paste("Percentage of Driver available between 12 pm to 4 pm for all requests received : ", availableDriverPercentageInAftHrs)
numOfMissingDriversInAfternoonHours <- numOfRequestInAfternnonHours -  numOfAvailableDriversInAfternoonHours
print(numOfMissingDriversInAfternoonHours)
percentageOfDriversMissing <- 100 - availableDriverPercentageInAftHrs
#numOfMissingDriversInAfternoonHours : 223 
#percentageOfDriversMissing : 27.13 %
paste("Percentage of Driver missing between 12 pm to 4 pm for all requests received : ", percentageOfDriversMissing)


#####Analysis for evening hours Driver availability for all pick up requests received during that time.####

uberDfEvng <- subset(uberDfFinal, as.numeric(RequestHour) >= 17 & as.numeric(RequestHour) < 22)
numOfRequestInEveningHours <- nrow(uberDfEvng)
#numOfRequestInMorningHours = 2052
print(numOfRequestInEveningHours)

numOfAvailableDriversDf <- subset(uberDfEvng, as.numeric(uberDfEvng$Driver.id) != 0)
numOfAvailableDriversInEveningHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInEveningHours = 879
print(numOfAvailableDriversInEveningHours)
availableDriverPercentageInEvngHrs <- (numOfAvailableDriversInEveningHours/numOfRequestInEveningHours)*100
#availableDriverPercentageInEvngHrs : 43%
paste("Percentage of Driver available between 5 am to 9 am for all requests received : ", availableDriverPercentageInEvngHrs)
numOfMissingDriversInEvngHours <- numOfRequestInEveningHours -  numOfAvailableDriversInEveningHours
print(numOfMissingDriversInEvngHours)
percentageOfDriversMissing <- 100 - availableDriverPercentageInEvngHrs
# numOfMissingDriversInEvngHours = 1173
#percentageOfDriversMissing : 57%
paste("Percentage of Driver missing between 5 pm to 9 pm for all requests received : ", percentageOfDriversMissing)

#####Analysis for night hours Driver availability for all pick up requests received during that time.####

uberDfNight <- subset(uberDfFinal, (as.numeric(RequestHour) > 21 & as.numeric(RequestHour) < 24) | (as.numeric(RequestHour) >= 0 & as.numeric(RequestHour) < 5))
numOfRequestInNightHours <- nrow(uberDfNight)
#numOfRequestInNightHours = 1128
print(numOfRequestInNightHours)

numOfAvailableDriversDf <- subset(uberDfNight, as.numeric(uberDfNight$Driver.id) != 0)
numOfAvailableDriversInNightHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInNightHours = 500
print(numOfAvailableDriversInNightHours)
availableDriverPercentageInNightHrs <- (numOfAvailableDriversInNightHours/numOfRequestInNightHours)*100
#availableDriverPercentageInNightHrs : 44.33 %
paste("Percentage of Driver available between 5 am to 9 am for all requests received : ", availableDriverPercentageInNightHrs)
numOfMissingDriversInNightHours <- numOfRequestInNightHours -  numOfAvailableDriversInNightHours
print(numOfMissingDriversInNightHours)
percentageOfDriversMissing <- 100 - availableDriverPercentageInNightHrs
#numOfMissingDriversInNightHours : 628
#percentageOfDriversMissing : 55.67 %
paste("Percentage of Driver missing between 10 pm to 4 am for all requests received : ", percentageOfDriversMissing)

#Graph Plot between Time of Day and Missing Driver Percentage

timeOfDay <- c("Morning","Afternoon","Evening","Night")
missingDriverPercentage <- c("21.38","27.13","57.00","55.67")
missingDriverPercentage <- as.numeric(missingDriverPercentage)
missingDriverPctByTimeOfDay <- cbind(timeOfDay,missingDriverPercentage)
missingDriverPctByTimeOfDay <- as.data.frame(missingDriverPctByTimeOfDay)
missingDriverPctByTimeOfDay$timeOfDay = as.factor(missingDriverPctByTimeOfDay$timeOfDay)
missingDriverPctByTimeOfDay$missingDriverPercentage = as.numeric(as.character(missingDriverPctByTimeOfDay$missingDriverPercentage))
str(missingDriverPctByTimeOfDay)

# Here we can see that, Percentage of drivers available w.r.t overall requests per time slot  is quite less in evening  compared to other times.  
plotMissingDriverPctByTimeSlot <- ggplot(missingDriverPctByTimeOfDay, aes(x = timeOfDay, y = missingDriverPercentage,fill = factor(timeOfDay))) + geom_bar(stat = "identity")  +  geom_text(aes(label=missingDriverPercentage),vjust=-0.5) + labs(title = "Bar Chart",subtitle="Missing Driver Percentage Per Time Slot Vs Time of Day", y = "Missing Driver Per Time Slot Percentage (%)", x = " Time of Day")
plotMissingDriverPctByTimeSlot

##############Trip Canceled Driver Percentage Analysis per Time of Day#################################

##Below code is to plot the percentage of canceled trip drivers with in each time slots (Morning, Afternoon, Evening and Night)
## Here canceledTripDriversPerTimeSlotPercentage = (canceledTripDriversPerTimeSlotCount/countOfAllRequestsReceivedWithInThatTimeSlot)


#####Analysis for morning hours Driver availability for all pick up requests received during that time.####
uberDfMrng <- subset(uberDfFinal, as.numeric(RequestHour) >= 5 & as.numeric(RequestHour) <= 11)
numOfRequestInMorningHours <- nrow(uberDfMrng)
#numOfRequestInMorningHours = 2549
print(numOfRequestInMorningHours)

numOfAvailableDriversDf <- subset(uberDfMrng, as.numeric(uberDfMrng$Driver.id) != 0 & uberDfMrng$Status == 'CANCELLED' )
numOfAvailableDriversInMornigHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInMornigHours = 956
print(numOfAvailableDriversInMornigHours)
canceledDriverPercentageInMrngHrs <- (numOfAvailableDriversInMornigHours/numOfRequestInMorningHours)*100
#canceledDriverPercentageInMrngHrs : 37.5 %
paste("Percentage of Driver Canceled Trip between 5 am to 9 am for all requests received : ", canceledDriverPercentageInMrngHrs)

#####Analysis for Afternoon hours Driver availability for all pick up requests received during that time.####

uberDfAft <- subset(uberDfFinal, as.numeric(RequestHour) > 11 & as.numeric(RequestHour) < 17 )
numOfRequestInAfternnonHours <- nrow(uberDfAft)
#numOfRequestInAfternnonHours = 822
print(numOfRequestInAfternnonHours)

numOfAvailableDriversDf <- subset(uberDfAft, as.numeric(uberDfAft$Driver.id) != 0 & uberDfAft$Status == 'CANCELLED')
numOfAvailableDriversInAfternoonHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInAfternoonHours = 84
print(numOfAvailableDriversInAfternoonHours)
canceledDriverPercentageInAftHrs <- (numOfAvailableDriversInAfternoonHours/numOfRequestInAfternnonHours)*100
#canceledDriverPercentageInAftHrs : 10.22 %
paste("Percentage of Driver Canceled Trip between 12 pm to 4 pm for all requests received : ", canceledDriverPercentageInAftHrs)


#####Analysis for evening hours Driver availability for all pick up requests received during that time.####

uberDfEvng <- subset(uberDfFinal, as.numeric(RequestHour) >= 17 & as.numeric(RequestHour) < 22)
numOfRequestInEveningHours <- nrow(uberDfEvng)
#numOfRequestInMorningHours = 2052
print(numOfRequestInEveningHours)

numOfAvailableDriversDf <- subset(uberDfEvng, as.numeric(uberDfEvng$Driver.id) != 0 & uberDfEvng$Status == 'CANCELLED')
numOfAvailableDriversInEveningHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInEveningHours = 146
print(numOfAvailableDriversInEveningHours)
canceledDriverPercentageInEvngHrs <- (numOfAvailableDriversInEveningHours/numOfRequestInEveningHours)*100
#canceledDriverPercentageInEvngHrs : 7.12%
paste("Percentage of Driver Canceled Trip between 5 pm to 9 pm for all requests received : ", canceledDriverPercentageInEvngHrs)

#####Analysis for night hours Driver availability for all pick up requests received during that time.####

uberDfNight <- subset(uberDfFinal, (as.numeric(RequestHour) > 21 & as.numeric(RequestHour) < 24) | (as.numeric(RequestHour) >= 0 & as.numeric(RequestHour) < 5))
numOfRequestInNightHours <- nrow(uberDfNight)
#numOfRequestInNightHours = 1128
print(numOfRequestInNightHours)

numOfAvailableDriversDf <- subset(uberDfNight, as.numeric(uberDfNight$Driver.id) != 0 & uberDfNight$Status == 'CANCELLED')
numOfAvailableDriversInNightHours <- nrow(numOfAvailableDriversDf)
#numOfAvailableDriversInNightHours = 68
print(numOfAvailableDriversInNightHours)
canceledDriverPercentageInNightHrs <- (numOfAvailableDriversInNightHours/numOfRequestInNightHours)*100
#canceledDriverPercentageInNightHrs : 6 %
paste("Percentage of Driver Canceled Trip between 10 pm to 4 am for all requests received : ", canceledDriverPercentageInNightHrs)

#Graph Plot between Time of Day and Missing Driver Percentage

timeOfDay <- c("Morning","Afternoon","Evening","Night")
canceledDriverPercentage <- c("38","10","7","6")
canceledDriverPercentage <- as.numeric(canceledDriverPercentage)
cancelledDriverPctByTimeOfDay <- cbind(timeOfDay,canceledDriverPercentage)
cancelledDriverPctByTimeOfDay <- as.data.frame(cancelledDriverPctByTimeOfDay)
cancelledDriverPctByTimeOfDay$timeOfDay = as.factor(cancelledDriverPctByTimeOfDay$timeOfDay)
cancelledDriverPctByTimeOfDay$canceledDriverPercentage = as.numeric(as.character(cancelledDriverPctByTimeOfDay$canceledDriverPercentage))
str(cancelledDriverPctByTimeOfDay)

# Here we can see that, Cancellation of trip is highest in morning..
plotMissingCancelledDriverPctByTimeSlot <- ggplot(cancelledDriverPctByTimeOfDay, aes(x = timeOfDay, y = canceledDriverPercentage,fill = factor(timeOfDay))) + geom_bar(stat = "identity")  +  geom_text(aes(label=canceledDriverPercentage),vjust=-0.5) + labs(title = "Bar Chart",subtitle="Canceled Trip Driver Percentage Per Time Slot Vs Time of Day", y = "Canceled Trip Driver Percentage Per Time Slot (%)", x = " Time of Day")
plotMissingCancelledDriverPctByTimeSlot

#Arrange Driver Analysis related plots for not completed trips in single plot
grid.arrange(plotMissingDriverPctByTimeSlot, plotMissingCancelledDriverPctByTimeSlot, ncol=2)

###EDA Conclusion ##########################
## Trips not getting completed is maximum in morning and evening hours. Trip not getting completed in evening hours is
## because of drivers not available and drivers cancelling the requests. Drivers not available has higher impact in evening hours
## Highest impacted route is Airport-to-City.
## Trip not getting completed in morning hours is because of drivers not available and drivers cancelling the requests.
## Drivers cancelling the trip requests has higher impact in morning hours compared to drivers not available. 
## Highest impacted route is City-to-Airport