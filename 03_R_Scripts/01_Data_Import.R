library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)

getwd()

trip1 <- read_csv("01_Raw_Data/202507-divvy-tripdata.csv")

head(trip1)
names(trip1)
glimpse(trip1)
summary(trip1)
