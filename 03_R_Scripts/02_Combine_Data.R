library(tidyverse)
library(janitor)
library(lubridate)

csv_files <- list.files(
  path="01_Raw_Data",
  pattern="\\.csv$",
  full.names=TRUE
)
csv_files

all_trips <- csv_files |> map(read_csv)
combined_trips <- bind_rows(all_trips)
dim(combined_trips)

glimpse(combined_trips)
names(combined_trips)

colSums(is.na(combined_trips)) # count missing values

sum(duplicated(combined_trips$ride_id)) # check for duplicate ride IDs - 35 records returned

duplicate_rides <- combined_trips %>%
  filter(duplicated(ride_id) | duplicated(ride_id, fromLast = TRUE))
View(duplicate_rides) # view 35 duplicate records

duplicate_rides %>%
  arrange(ride_id)  # check whether the rows are truly identical

duplicate_rides %>%
  count(ride_id) %>%
  arrange(desc(n)) # count how many times each duplicate appears

combined_trips <- combined_trips %>%
  distinct()

sum(duplicated(combined_trips$ride_id)) 

glimpse(combined_trips)

combined_trips <- combined_trips %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  )

summary(combined_trips$ride_length)

sum(combined_trips$ride_length <= 0) # invalid ride durations (duratiion time is 0 or negative). This can be deleted
sum(combined_trips$ride_length > 1440) # 1440mins = 24hrs (duration more than 1 day)

cleaned_trips <- combined_trips %>%
  filter(ride_length > 0)

summary(cleaned_trips$ride_length)

options(scipen = 999) # changes exponential outcomes to regular decimal formats
summary(cleaned_trips$ride_length)

nrow(cleaned_trips)

cleaned_trips <- cleaned_trips %>%
  mutate(
    date = as.Date(started_at),
    month = month(started_at,label = TRUE),
    day = day(started_at),
    year = year(started_at),
    day_of_week = wday(started_at, label = TRUE),
    hour = hour(started_at)
  )

glimpse(cleaned_trips)
View(cleaned_trips)

cleaned_trips <- cleaned_trips %>%
  mutate(
    day_type = if_else(
      day_of_week %in% c("Sat","Sun"),
      "Weekend",
      "Weekday"
    )
  )               #created weekend vs weekday

table(cleaned_trips$day_type)     # weekend weekday count table

# Prepare process completed (data cleaning)
# Analysis phase starting (EDA Exploratory Data Analysis)

# 1 member casual count table
table(cleaned_trips$member_casual)   

# 2 Average ride duration by customer type
cleaned_trips %>%
  group_by(member_casual) %>%
  summarize(
    Number_of_Rides = n(),
    Average_Ride = mean(ride_length),
    Median_Ride = median(ride_length),
    Max_Ride = max(ride_length)
  )      

# 3 Average ride duration by bike type
cleaned_trips %>%
  group_by(rideable_type) %>%
  summarize(
    Average_Ride = mean(ride_length),
    Total_Rides = n()
  )       

# 4 Most popular day of week
cleaned_trips %>%
  group_by(day_of_week) %>%
  summarize(
    Total_Rides = n(),
  ) %>%
  arrange(desc(Total_Rides))     

# 5 Average ride duration by weekday/weekend
cleaned_trips %>%
  group_by(day_type,member_casual) %>%
  summarize(
    Total_Rides = n(),
    Average_Ride = mean(ride_length),
  )       

# 6 Most popular hour
cleaned_trips %>%
  group_by(hour) %>%
  summarize(
    Total_Rides = n(),
  ) %>%
  arrange(desc(Total_Rides))     

# Deeper Analysis(EDA) - Deeper Business Insights 

# 1 Rides by Month
cleaned_trips %>%
  group_by(month, member_casual) %>%
  summarise(
    Total_Rides = n(),
    Average_Ride = mean(ride_length)
  ) %>%
  arrange(month)

# 2 Average ride by Day of Week
cleaned_trips %>%
  group_by(day_of_week, member_casual) %>%
  summarise(
    Average_Ride = mean(ride_length),
    Total_Rides = n()
  )

# 3 Ride counts by hour and customer type
cleaned_trips %>%
  group_by(hour, member_casual) %>%
  summarise(
    Total_Rides = n()
  ) %>%
  arrange(hour)

# 4 Bike preference by customer type
cleaned_trips %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    Total_Rides = n(),
    Average_Ride = mean(ride_length)
  )

# 5 Monthly trend
table(cleaned_trips$month)

# Analysis completed
# Creating ggplot2 charts - visualization

library(ggplot2)

# 1 Monthly members vs casual trends
cleaned_trips %>%
  group_by(month, member_casual) %>%
  summarise(
    Total_Rides=n()
  ) %>%
  ggplot(aes(month,
             Total_Rides,
             fill=member_casual))+
  geom_col(position="dodge")+
  labs(
    title="Monthly Ride Count by Customer Type",
    x="Month",
    y="Number of Rides"
  )

# 2 Average ride duration
cleaned_trips %>%
  group_by(member_casual) %>%
  summarise(
    Average_Ride=mean(ride_length)
  ) %>%
  ggplot(aes(member_casual,
             Average_Ride,
             fill=member_casual))+
  geom_col()+
  labs(
    title="Average Ride Duration by Customer Type",
    x="Member Riders & Casual Riders",
    y="Average Rides(Mins)"
  )

# 3 Bike preference
cleaned_trips %>%
  group_by(member_casual,rideable_type) %>%
  summarise(
    Total_Rides=n()
  ) %>%
  ggplot(aes(rideable_type,
             Total_Rides,
             fill=member_casual))+
  geom_col(position="dodge")+
  labs(
    title="Bike Type Preference by Customer Type",
    x="Rideable type",
    y="Number of Rides"
  )

# 4 Hourly rides
cleaned_trips %>%
  group_by(hour,member_casual) %>%
  summarise(
    Total_Rides=n()
  ) %>%
  ggplot(aes(hour,
             Total_Rides,
             color=member_casual))+
  geom_line(size=1)+
  labs(
    title="Hourly Ride Demand by Customer Type",
    x="Hours(0-23hrs)",
    y="Number of Rides"
  )

# 5 Weekday vs Weekend
cleaned_trips %>%
  group_by(day_type,member_casual) %>%
  summarise(
    Total_Rides=n()
  ) %>%
  ggplot(aes(day_type,
             Total_Rides,
             fill=member_casual))+
  geom_col(position="dodge")+
  labs(
    title="Weekday vs Weekend Ride comparison",
    x="Day type",
    y="Number of Rides"
  )

write.csv(cleaned_trips,
"cyclistic_cleaned.csv",
row.names = FALSE)



