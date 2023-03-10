How does a bike-share navigate speedy success?

1. Ask

Scenario:
In an effort to increase revenue, the marketing department, would like to find a way to maximize the number of annual memberships to our service. In order to better understand the market, I have been asked to analyses the ways in which annual members and casual riders use Cyclitic bikes differently. Data-driven insight into these trends should help the marketing team determine what might make casual riders more likely to buy an annual membership, and this will ultimately shape their digital media strategy. Given that the executive team must approve the marketing strategy, I will include some recommendations that align with the goal of increasing annual memberships.

 ##Objective
Hence, the objective for this analysis is to throw some light on how the two types of customers: annual members and casual riders, use Cyclistic bikeshare differently, based on few parameters that can be calculated/ obtained from existing data.

Deliverables:
Insights on how annual members and casual riders use Cyclistic bikes differently
 Provide effective visuals and relevant data to support insights
Use insights to give three recommendations to convert casual riders to member riders

2. Prepare
After the Ask stage i have moved to Preparing of data.I downloaded the data from the divvy trip data and stored in desktop.
I renamed the folder to make it simple.
I renamed all the files as per the standard naming conventions.
I took 2 data sheets which contains "2097150" Rows of data.

I HAVE MADE SOME CHANGESIN THE NAME OF THE COLUMNSDocumentation, Cleaning and Preparation of data for analysis
The combined size of all the 2 datasets with 2097150 Rows is close to 0.25GB. Data cleaning in spreadsheets will be time-consuming and slow compared to SQL or R. I am choosing R simply because I could do both data wrangling and analysis/ visualizations in the same platform. It is also an opportunity for me to learn R better.

Load libraries:
Then i have added some of the libraries to get the proper results in my commands

#library(tidyverse)
#library(lubridate)
#library(dplyr)
#library(readr)
#library(janitor)
#library(data.table) #libeary(ggplot2)

Load Dataset
Once the packages are installed we need to import the data set to the R-Studio

getwd() 
setwd("C:/Swasthik pr")

I have set the Working Direction As "C:/Swasthik pr"
And now i need to import the Dataset which i have using for this project

q1_2019 <- read_csv("1q2_2019pr.csv")
q2_2019 <- read_csv("2q3_2019pr.csv")

Check column names of each dataset for consistency:

colnames(q1_2019)
colnames(q2_2019)

Check data structures and data types for all data frames

str(q1_2019)
str(q2_2019)

*We will get the structure of the data setsData transformation and cleaning

q1_2019 = mutate(q1_2019, trip_id = as.character(trip_id)
 ,usertype = as.character(usertype))
q2_2019 = mutate(q2_2019, trip_id = as.character(trip_id)
 ,usertype = as.character(usertype))

3. Process

 Combine all the datasets into one single Dataframe
 
all_trips <- bind_rows(q1_2019, q2_2019)
str(all_trips)

This will help us to consolidate the data into into single dataset in R-Studio
Then i changed the Date, Time into a convinient formate using below mentioned commanfd

all_trips$start_time <- as.POSIXct(all_trips$start_time, format="%m/%d/%Y %H: %M")
all_trips$end_time <- as.POSIXct(all_trips$end_time, format="%m/%d/%Y %H: %M")

*changing the date and TimeThen i have find the ride_lenght using the below mentioned command

all_trips$ride_length <- difftime(all_trips$end_time, all_trips$start_time, units="mins")
all_trips$ride_length <- as.numeric(all_trips$ride_length)

#Add new columns that can be used for aggregate functions
column for day of the week, Month and the Year of the trip started

all_trips$day <- weekdays(all_trips$start_time)
all_trips$month <- months(all_trips$start_time)
all_trips$year <- year(all_trips$start_time)

*From above Commandi can succesfully add the 3 columnsall_trips$ride_length <- (as.double(difftime(all_trips$end_time, all_trips$start_time)))/60
 Using this command i can turn ride_length from " Seconds" To " Minutes'
# checking for trip lengths less than 0

all_trips <- filter(all_trips, all_trips$ride_length > 0)

It is important to make sure that customer_type column has only two distinct values. Let's confirm the same.
table(all_trips$usertype)



4. Analyze the data

#CONDUCT DESCRIPTIVE ANALYSIS
avg_trip = mean(all_trips$tripduration) 
median(all_trips$tripduration)
max(all_trips$tripduration)
min(all_trips$tripduration)

The average, Median, Minimum, Maximum Trip Value
*The outputNow we are Aggregating the data using Mean , Median , Max and Min
mean_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN = mean)
median_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN = median)
max_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN = max)
min_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN = min)

*Aggregation of the data

5. Share the data
Data Visualization
