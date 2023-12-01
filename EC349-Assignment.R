setwd("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment")

library(tidyverse)
library(jsonlite)
library(caret)
library(lubridate)


library(glmnet)

#Clear
cat("\014")  
rm(list=ls())
#For big data set
user_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_user.json"))
user_data <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_user.json"))

summary(user_data$yelping_since)
# date in character, convert into dates
user_data <- user_data %>% 
  mutate(yelping_since=ymd_hms(yelping_since))
summary(user_data$yelping_since)
# Convert None to NA in friends otherwise difficult to calculate friends:
user_data <- user_data %>% 
  mutate(friends = if_else(friends == "None", NA, friends))
# Count how many friends a user has:
user_data <- user_data %>% 
  mutate(friend_count = 1 + str_count(friends, ","))
# Convert NA in friend count into 0
user_data <- user_data %>% 
  mutate(friend_count = if_else(is.na(friend_count), 0, friend_count))
# Need to convert 20,20 in yelping_since into 2020 in elite
user_data <- user_data %>%
  mutate(elite = str_replace_all(elite, "20,20", "2020"))
# Need to convert empty strings into NA's in elite to calculate elite years, otherwise because of my 1 + n(,) function, I'd get 1 instead of NA which wwould be hassle to turn into 0
user_data <- user_data %>%
  mutate(across(elite, ~na_if(., "")))
# Count number of years user was elite for
user_data <- user_data %>% 
  mutate(elite_year_count = 1 + str_count(elite, ","))
# Convert NA's into 0 for Elite years
user_data <- user_data %>% 
  mutate(elite_year_count = replace_na(elite_year_count, 0))
# Total compliments for every user
user_data <- user_data %>% 
  mutate(total_compliments = pmap_dbl(select(., 12:22), ~sum(c(...), na.rm = TRUE)))
# Find account age
yelping_start <- year(user_data_cleaned$yelping_since)
user_data_cleaned <- user_data_cleaned %>% 
  mutate(account_age = 2023 - yelping_start)
# Base R, was very annoying to do, should have done dplyr like below
user_data_cleaned <- user_data[, c("user_id", "review_count", "yelping_since", "useful", "funny", "cool", "fans", "average_stars", 
                                         "elite_year_count", "friend_count", "total_compliments")]
# Definitely do this next time!
user_data_cleaned <- user_data %>% 
  select(user_id, review_count, yelping_since, useful, funny, cool, fans, average_stars, elite_year_count, friend_count, total_compliments)

save(user_data_cleaned, file = "user_data_cleaned.Rdata")
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data_cleaned.Rdata")
load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data_cleaned.Rdata")

set.seed(1)
# Create a separate partition within the data frame of the original data set to be called upon to create training vs test sets
# The separate partition can be treated like a separate list and so can be called with a name in the row index
user_partition <- createDataPartition(y = user_data_cleaned$average_stars, 1, p = 0.75)
# The partition is separate in the data frame of user and thus needs to be called when referring to the indices with user_data_small[]
user_training_set <- user_data_cleaned[user_partition[[1]], ]
user_test_set <- user_data_cleaned[-user_partition[[1]], ]

vars_excluded <- c("average_stars", "user_id", "yelping_since")
x <- as.matrix(user_training_set[,setdiff(names(user_training_set), vars_excluded)])
y <- user_training_set$average_stars


krid <- 10^seq(-1,-15, length=150)
cv.ridgeu <- cv.glmnet(x, y, alpha=0, lambda = krid)
print(cv.ridgeu)
plot(cv.ridgeu)
print(cv.ridgeu$lambda.min) # minLambda = 5.306794e-13
lambda.ridge <- cv.ridgeu$lambda.min
#dcGmatrix needs to be converted into proper matrix before conversion into dataframe to be viewed
#matrix is stored as a value and cannot be viewed and saved as data directly to do that you need to convert into data frame
coef.ridge <- as.matrix(coef(cv.ridgeu, s= lambda.ridge))
coef.ridge.df <- as.data.frame(coef.ridge)
min.mse <- min(cv.ridgeu$cvm) 
print(min.mse) # minMSE = 1.371429







business_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
business_data <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json), laptops' path

business_data <- business_data %>% 
  mutate(city = as.factor(city))
business_data <- business_data %>% 
  mutate(state = as.factor(state))
business_data <- business_data %>% 
  mutate(categories=as.factor(categories))

# Counting how many non-empty attributes a business has, the more famous/ubiquitous business should have more attributes since this means
# they have dedicated staff or is more innovative in keeping up with the trend 
# This means number of non-empty attributes could be seen as a measure of engagement with yelp?
# Could be problematic because no one really goes to fill out all 39 attributes including unrelated ones.
# Unless this also carries information about how big the place is because the more general purpose the more attributes?
business_attributes <- business_data %>% 
  select(attributes)
engagement <- rowSums(!is.na(business_attributes))
business_data <- business_data %>% 
  mutate(engagement = engagement)
# Check how businesses choose to fill the attributes according to categories?
business_engagement <- business_data %>% 
  select(name, categories, engagement)


load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/Small Datasets/yelp_review_small.Rda")
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/Small Datasets/yelp_review_small.Rda")

# convert date into date format
review_data_small <- review_data_small %>% 
  mutate(date=ymd_hms(date))

checkin_data  <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json), laptops' path
tip_data  <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json), laptops' path














