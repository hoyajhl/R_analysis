library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
##importing dataset
activity <- read.csv("C:/Users/hoyaj/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories <- read.csv("C:/Users/hoyaj/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
intensities <- read.csv("C:/Users/hoyaj/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
sleep <- read.csv("C:/Users/hoyaj/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("C:/Users/hoyaj/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

## formatting change
## *format function returns to character*
str(intensities) ## ActivityHour <chr-> change format into POSIXct> 
## as.POSIX:  Date-time Conversion Functions
## Functions to manipulate objects of classes "POSIXlt" 
## representing calendar dates and times
# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, 
                                    format="%m/%d/%Y %H:%M:%S %OS", 
                                    tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, 
                                 format="%m/%d/%Y %H:%M:%S %OS", 
                                 tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
str(activity)
activity$ActivityDate=as.POSIXct(activity$ActivityDate,
                                  format="%m/%d/%Y", 
                                  tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, 
                          format="%m/%d/%Y %H:%M:%S %OS", 
                          tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")


## check for distinct id for each of the dataset
n_distinct(activity$Id)  ##33
n_distinct(calories$Id)  ##33
n_distinct(intensities$Id) ##33 
n_distinct(sleep$Id)   ##24
n_distinct(weight$Id)  ##8

## check for the relevance
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

##merge data according to id and date
merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")