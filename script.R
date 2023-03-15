library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(skimr)


getwd()
setwd("C:/Swasthik pr/Project 2/datset")
activity = read.csv("dailyActivity merged.csv")
calories = read.csv("hoursly calories.csv")
intensities = read.csv("hourlyIntensities_merged.csv")
sleep = read.csv("sleepDay_merged.csv")
weight = read.csv("weightLogInfo meged.csv")


head(activity)
head(calories)
head(intensities)
head(sleep)
head(weight)



activity$Id[!duplicated(activity$Id,)]
calories$Id[!duplicated(calories$Id,)]
intensities$Id[!duplicated(intensities$Id,)]
sleep$Id[!duplicated(sleep$Id,)]
weight$Id[!duplicated(weight$Id,)]


calories$Id = as.character(calories$Id)
activity$Id = as.character(activity$Id)
intensities$Id = as.character(intensities$Id)
sleep$Id = as.character(sleep$Id)
weight$Id = as.character(weight$Id)



sum(duplicated(activity))
sum(duplicated(calories))
sum(duplicated(intensities))
sum(duplicated(sleep))
sum(duplicated(weight))

# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

# calories
calories$ActivityHour = as.POSIXct(calories$ActivityHour, format = "%m/%d/%y %H:%M", tz = Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")

# sleep
sleep$sleepday = as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
sleep$SleepDay = weekdays(sleep$sleepday)


table(sleep$hours)


head(activity)
head(calories)
head(intensities)
head(sleep)
head(weight)

activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

calories %>%
  select(Calories) %>%
  summary()

sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

weight %>%
  select(WeightKg, BMI) %>%
  summary()




ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")


ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")




###
int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")






