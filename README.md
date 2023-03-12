
# How Does a Bike-Share Navigate Speedy Success?

## Scenario
As a data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore,my team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve my recommendations, so they must be backed up with compelling data insights and professional data visualizations.

## Methodology Used
In order to approach this business task, I have utilized the Ask, Prepare, Process, Analyze, Share and Act methodology. Each step is detailed below along with the crucial guiding questions and answers as we progress through the analysis.

## Ask Stage
This is the first stage in the data analysis process where i need to required to ask a comprehensive list of questions about the project, its goals and the stakeholder expectations. Fortuantely, for this case study, the main stakeholder is the Director of Marketing and has given us a clear goal: **Design marketing strategies aimed at converting casual riders into annual members**. In order to accomplish this, we need to first understand how the two categories of riders differ, why they differ and how can digital media affect their marketing tactics.

We will be focusing on the first question: **How do annual members and casual riders use Cyclistic bikes differently?**

Based on the project goals and expectations, we know that we need to learn any insights on how the data differs between the two categories of Cyclistic Riders and how we can use those insights to make recommendations to the Marketing team.

## Prepare Stage

After the Ask stage i have moved to Preparing of data.I downloaded the data from the divvy trip data and stored in desktop.

1. I renamed the folder to make it simple.

2. I renamed all the files as per the standard naming conventions.

3.  I took 2 data sheets which contains "2097150" Rows of data.





## sample excel workbook of the current project

![App Screenshot](https://github.com/swasthik62/swasthik62.github.io/blob/main/Screenshot%202023-03-12%20170228.jpg)

The combined size of all the 2 datasets with 2097150 Rows is close to 0.25GB. Data cleaning in spreadsheets will be time-consuming and slow compared to SQL or R. I am choosing R simply because I could do both data wrangling and analysis/ visualizations in the same platform. It is also an opportunity for me to learn R better.

# Process
From Here onwards we will begin with our data cleaning, validation and transformation actions. In the previous step, we realized that the data collected is mostly valid but needs some cleaning and transformation for better result

## 1. What tools are we going to use to perform our analysis?
I have used Microsoft Excel to get an overview of the data and get the idea about the dataset im dealing with the entire process of analysis. I have start with some basic sorting and filtering but given the size of the data sets.

I will process, Analize and Visualize the data with R itself hence i chose to use R to perform my analysis. R is incredibly robust when working with large data sets and built-in functions and packages are incredibly easy to configure.

I have imported the main packages that I will be using for the analysis.


### packages



```bash
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
```

Once the packages are getting installed we need to import our Dataset for R-Studio for further process.

```bash
getwd()
setwd("C:/Swasthik pr/New folder")
q1_2016 <- read_csv("q1_2016.csv")
q2_2016 <- read_csv("q2_2016.csv")
q3_2016 <- read_csv("q3_2016.csv")
q4_2016 <- read_csv("q4_2016.csv")
q5_2016 <- read_csv("q5_2016.csv")
q6_2016 <- read_csv("q6_2016.csv")
```

Once the datasets are imported, we need to check for the variables and the other datas is in the same format in both the data sets.

```bash
colnames(q1_2016)
colnames(q2_2016)
colnames(q3_2016)
colnames(q4_2016)
colnames(q5_2016)
colnames(q6_2016)
```
By this code we can check the column manes of the imported dataset.

```bash
str(q1_2016)
str(q2_2016)
str(q3_2016)
str(q4_2016)
str(q5_2016)
str(q6_2016)
```

We need to mutate the dataset using below mentioned code

```bash
q1_2016 = mutate(q1_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q2_2016 = mutate(q2_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q3_2016 = mutate(q3_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q4_2016 = mutate(q4_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q5_2016 = mutate(q5_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q6_2016 = mutate(q6_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
```

By this code we can check the structue of the datasets.

We are going to combine the dataset.
```bash
all_trips <- bind_rows(q1_2016, q2_2016, q3_2016, q4_2016, q5_2016, q6_2016)

```

## 2. What steps did you take to clean the data?


Upon inspection of the variable data, these were some of my observations:

* There were lots of incorrect data points that we need to be rectified and we need to fix that.

*  i have seen there is 3 cahrecter in 'gender' column and i need to filter the data and get it corrected. Also one more data has been accumulated in the "usertype" tbat is also removed from the column

```bash
all_trips = filter(all_trips, gender == "Male" | gender == "Female")

all_trips = filter( all_trips, usertype == "Customer" |usertype == "Subscriber")
```

* Station IDs were a mix of numeric and alphanumeric, however the variables were formatted as text. I decided against converting them to a standard format because there was no way to discern a pattern in order for me to split and concatenate the information.

* Datetime data was measured as per the UNIX Standard which is ideal for numeric calculations that I will utilize in the next step.

* The i will seggregate the data according to the Months, Weekdays and the years for the further use.

The following chunk of code was used to inspect and clean the data. Finally, it was sorted and filtered to remove any discrepancies in the data.

```bash
#To format the date and time
all_trips$starttime <- as.POSIXct(all_trips$starttime, format="%m/%d/%Y %H: %M")
all_trips$stoptime <- as.POSIXct(all_trips$stoptime, format="%m/%d/%Y %H: %M")


#This will give the result for Ride Lenght of the perticular user Types 
all_trips$ride_length <- (as.double(difftime(all_trips$stoptime, all_trips$starttime)))/60
all_trips$ride_length <- as.numeric(all_trips$ride_length)

#By using this code we can seggregate the Weekdays, Months and Years seperately
all_trips$day <- weekdays(all_trips$starttime)
all_trips$month <- months(all_trips$starttime)
all_trips$year <- year(all_trips$starttime)

all_trips <- filter(all_trips, all_trips$ride_length > 0)

table(all_trips$usertype)
```
## 3.  How can you verify that your data is clean and ready to analyze?

I export the cleaned data file in to its own csv file and then run the same exploratory checks that were done previously to verify that the data is ready, the variables are of the suitable type and that there are no discrepencies.

## 4. Have you documented your cleaning process so you can review and share those results?

I documented the entire cleaning process in the original script using comments in the code and this markdown file compiles that in an orderly manner to replicate the same cleaning process.

# Analyze
Once the data was cleaned, sorted and filtered to remove discrepancies, I moved to analyze the data using descriptive statistics. This is the stage where we dig deep into the data to search for trends and relationships between variables. The set of data after cleaning process gives the 2,097,150 records to analyze which is a substantial sample to identify any insights with strong statistical confidence.

```bash
#I have aggregated the data by finding the mean, median, maximum and minimum values

#Grouped data by shift
mean_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  mean)
median_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  median)
max_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  max)
min_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  min)

#Grouped data by weekdays
mean_day = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  mean)
median_day = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  median)
max_day = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  max)
min_day = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  min)

#Grouped data by months
mean_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  mean)
median_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  median)
max_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  max)
min_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  min)

#Grouped data by gender
mean_gender = aggregate(all_trips$ride_length ~ all_trips$gender, FUN =  mean)
median_gender = aggregate(all_trips$ride_length ~ all_trips$gender, FUN =  median)
max_gender = aggregate(all_trips$ride_length ~ all_trips$gender, FUN =  max)
min_gender = aggregate(all_trips$ride_length ~ all_trips$gender, FUN =  min)


#Finding Mean, Mode, Max and Min of ride_lenght
minimum_value_of_ride_lenght = min(all_trips$ride_length)
maximum_value_of_ride_lenght = max(all_trips$ride_length)
mean_of_ride_lenght = mean(all_trips$ride_length)
sd_of_ride_lenght = sd(all_trips$ride_length)

```

In The next procedure im going to visualise the data using some graphical representation

```bash
# Prior to plotting the data, we need to order the days of the week in the order that we want it.
all_trips$day <- ordered(all_trips$day, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#Then if we apply the code we will get the data of Total trips by the customer in a week,
all_trips %>%  
  group_by(usertype, day) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, day)  %>% 
  ggplot(aes(x = day, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by customer type Vs. Week") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  ```

## "Total trips by customer type Vs. Week"

![App Screenshot](https://github.com/swasthik62/swasthik62.github.io/blob/main/Total%20trips%20by%20customer%20type%20vs%20week.png)

```bash
#Prior to plotting the data, we need to order the shift of the day in the order that we want it.
all_trips$shift <- ordered(all_trips$shift,levels = c("MORNING", "AFTERNOON", "EVENING", "April", "NIGHT"))
table(all_trips$shift)

#Then if we apply the code we will get the data of Total trips by the customer in a day, 

all_trips %>%  
  group_by(usertype, shift) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, shift)  %>% 
  ggplot(aes(x = shift, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by customer type Vs.Shift") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

## "Total trips by customer type Vs.Shift"

![App Screenshot](https://github.com/swasthik62/swasthik62.github.io/blob/main/total%20trips%20by%20customer%20type%20vs%20Shift.png)

```bash
#Prior to plotting the data, we need to order the Month of the data in the order that we want it.

all_trips$month <- ordered(all_trips$month,levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#Then if we apply the code we will get the data of Total trips by the customer per every Month,
all_trips %>%  
  group_by(usertype, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

## "Total trips by customer type Vs. Month"

![App Screenshot](https://github.com/swasthik62/swasthik62.github.io/blob/main/Total%20trips%20by%20customer%20type%20vs%20Month.png)



# Share & Act
Once the analysis is completed and we have obtained the insights, now we  have to share this information to the relevant stakeholders. In this case, a presentation will be made for the Director of Marketing outlining the business task, assumptions made using the data we had and translating the insights from the analysis to actionable recommendations.

Based on my analysis, these were my observations:

* A significant amount of subscribed  riders use Cyclistic during the mid weeks, 
notably Monday To Friday and the data shows 
a higher average trip duration of the
 mid week day itself.

* As in the weekend the Subscribers will 
slightly goes down and the slightly raise 
in the random customers.

* We can clearly see in the month of July there is pathtic drop in the users.

* There is more users in the mid year like June and August but as the year end the numbers of user will going down.

* When its come to the average day usage there
is high users in the Evening, and very lesss users 
in the Night time. and the numbers of the 
"customer" is very less in the Mornig and night time.

Most number of casual rider trips were shown to be during the summer months of May, June, August and september and in the month of July there is rapid degrowing in a single month because of the heavy rain.
There is decline during the winter months.

## recommendations
* Provide attractive promotions for casual riders on weekdays so that casual members use the bikeshare services ore uniformly across the entire week.

* Offer discounted membership fee for renewals after the first year. It might nudge casual riders to take up membership.

* Offer discounted pricing during non-busy hours so that casual riders might choose to use bikes more often and level out demand over the day.


