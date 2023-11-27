setwd("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment")


library(tidyverse)
library(jsonlite)
library(caret)
library(lubridate)

#Clear
cat("\014")  
rm(list=ls())

#Load & save different data
load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/Small Datasets/yelp_user_small.Rda")

summary(user_data_small$yelping_since)
user_data_small[, 8]
# Need to convert empty strings into NA's in Elite
user_data_small <- user_data_small %>%
  mutate(across(elite, ~na_if(., "")))
#Need to convert None into NA's in friends
user_data_small <- user_data_small %>%
  mutate(across(friends, ~na_if(., "None")))
user_data_small[, 8]
# Need to convert 20,20 in yelping_since into 2020 in Elite
user_data_small <- user_data_small %>%
  mutate(elite = str_replace_all(elite, "20,20", "2020"))
save(user_data_small, file = "user_data.Rdata")

load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data.Rdata")


business_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)







save(business_data, file = "business_data.RData")
save(checkin_data, file = "checkin_data.Rdata")
save(tip_data, file = "tip_data.Rdata")
save(review_data_small, file = "review_data.Rdata")
save(user_data_small, file = "user_data.Rdata")




ymd_hms()

