setwd("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment")

library(jsonlite)


library(tidyverse)
library(lubridate)

library(caret)
library(glmnet)

#Clear
cat("\014")  
rm(list=ls())
#Convert data into R_data file for more efficient storage and ram usage.
user_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_user.json"))
user_data <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_user.json"))
save(user_data, file = "user_data.Rdata")
rm(user_data)
load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data.Rdata")
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data.Rdata")

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
#Could consider making a variable for each elite year and checking whether that improves performance, just a thought.
# Total compliments for every user
user_data <- user_data %>% 
  mutate(total_compliments = pmap_dbl(select(., 12:22), ~sum(c(...), na.rm = TRUE)))
#Find account age, too crude? Might miss days!
#yelping_start <- year(user_data_cleaned$yelping_since)
#user_data <- user_data %>% 
#  mutate(account_age = 2023 - yelping_start)
user_data <- user_data %>%
  mutate(
    yelping_since_date = as.Date(yelping_since),
    years_on_yelp = as.numeric(difftime(ymd("2023-01-01"), yelping_since_date, units = "days")) / 365.25, # Calculate duration in years
  )
# Renaming user variables so they can be joined well with other data sets:
colnames(user_data)[3] <- "user_review_count"
colnames(user_data)[5] <- "total_useful"
colnames(user_data)[6] <- "total_funny"
colnames(user_data)[7] <- "total_cool"
colnames(user_data)[10] <- "total_fans"
colnames(user_data)[11] <- "user_average_stars"

# Base R, was very annoying to do, should have done dplyr like below
user_data_cleaned <- user_data[, c("user_id", "review_count", "yelping_since", "useful", "funny", "cool", "fans", "average_stars", 
                                         "elite_year_count", "friend_count", "total_compliments")]
# Definitely do this next time!
user_data_cleaned <- user_data %>% 
  select(user_id, user_review_count, years_on_yelp, total_useful, total_funny, total_cool, user_average_stars, elite_year_count, friend_count,
         total_fans, compliment_hot, compliment_more, compliment_profile, compliment_cute, compliment_list, compliment_note, compliment_plain, 
         compliment_cool, compliment_funny, total_compliments)
user_data_cleaned$yelping_since <- NULL

#Rda files less ram intensive and more efficient storage!
save(user_data_cleaned, file = "user_data_cleaned.Rdata")
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data_cleaned.Rdata")
load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/user_data_cleaned.Rdata")

#Check for outliers with review and 

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
# Just a test with cv.ridge!


business_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
business_data <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json), laptops' path
save(business_data, file = "business_data.Rdata")
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/business_data.Rdata")
load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/business_data.Rdata")


business_data <- business_data %>% 
  mutate(city = as.factor(city))
business_data <- business_data %>% 
  mutate(state = as.factor(state))
#business_data <- business_data %>% 
#  mutate(categories=as.factor(categories))

# finding categories as factors looks kinda useless, might need to instead count how many categories a business has


# Counting how many non-empty attributes a business has, the more famous/ubiquitous business should have more attributes since this means
# they have dedicated staff or is more innovative in keeping up with the trend 
# This means number of non-empty attributes could be seen as a measure of engagement with yelp?
# Could be problematic because no one really goes to fill out all 39 attributes including unrelated ones.
# Unless this also carries information about how big the place is because the more general purpose the more attributes?
b_attributes <- business_data %>% 
  select(attributes)
business_data <- business_data %>% 
  mutate(engagement = rowSums(!is.na(b_attributes)))

# Check how businesses choose to fill the attributes according to categories?
business_engagement <- business_data %>% 
  select(name, review_count, categories, engagement)

# Find category count and see how it's distributed:

# business_data <- business_data %>% 
#   mutate(category_count = 1 + str_count(categories, ","))
# ggplot(business_data, aes(x = category_count)) +
#   geom_histogram(binwidth = 1, fill = "blue", color = "black") +
#   labs(title = "Histogram of Category Count", x = "Category Count", y ="Frequency")
# ggplot(business_data, aes(x= engagement, y = category_count)) + 
#   geom_point() + 
#   labs(title = "Engagement vs Category count", x = "Engagement count", y = "Category_count")

# Categories can be attached by algorithm for related terms so may not be useful?
# Looks pretty useless, instead categorise the businesses, bars & restaurants, financial services etc.? Would love to use unsupervised machine learning for classification but I don't think I have the time to actually learn.
# Try creating a character vector to add up all the words between commas and then use unique function to see what unique responses there are?

categories <- paste(business_engagement$categories, collapse = ",")
categories <- strsplit(categories, ",")
categories <- lapply(categories, str_trim)
unique_categories <- unique(unlist(categories))
print(unique_categories)
unique_categories <- str_replace_all(unique_categories, " ", "")
unique_categories_matrix <- matrix(unique_categories, ncol = 1, byrow= TRUE)
unique_categories <- as.data.frame(unique_categories_matrix)

# There are 1300+ unique categories, how would I do this? Should I check which categories are most engaged with yelp or review count by creating a category variable and parsing each business's category strings to determine most important categories?
# unique_categories_list <- list(unique_categories)
# named_unique_categories <- setNames(as.list(unique_categories), unique_categories)
# unique_categories_df <- data.frame(named_unique_categories)

# Clearly too many variables, R cannot handle it, try another way
# Give up, go to Yelp category list, just take main categories form Yelp, can analyse further if I have time. Create a variable for each of them in business_data'
#
# Add fashion and restaurants and bars for special cases because they have great engagement

# main_categories <- c("Active Life, Arts & Entertainment, Automotive, Beauty & Spas, Education, Event Planning & Services, Financial Services, Food, Health & Medical, Home Services, Hotels & Travel, Local Flavor, Local Services, Mass Media, Nightlife, Pets, Professional Services, Public Services & Government, Real Estate, Religious Organizations, Restaurants, Shopping")
# main_categories <- str_replace_all(main_categories, " ", "")
# main_categories <- str_split(main_categories, ",")
# named_main_categories <- setNames(as.list(main_categories), main_categories)
# main_categories.df <- data.frame(named_main_categories)
# Useless, should've just used mutate to sort each category from the start!
business_categories <- business_data %>% 
  select(name, categories)

business_data <- business_data %>% 
  mutate(Active_Life = if_else(str_detect(categories, "Active Life"), 1, 0),
         Arts_Entertainment = if_else(str_detect(categories, "Arts & Entertainment"), 1, 0),
         Automotive = if_else(str_detect(categories, "Automotive"), 1, 0),
         Beauty_Spas = if_else(str_detect(categories, "Beauty & Spas"), 1, 0),
         Education = if_else(str_detect(categories, "Education"), 1, 0),
         Event_Planning_Services = if_else(str_detect(categories, "Event Planning & Services"), 1, 0),
         Financial_Services = if_else(str_detect(categories, "Financial Services"), 1, 0),
         Food = if_else(str_detect(categories, "Food"), 1, 0),
         Health_Medical = if_else(str_detect(categories, "Health & Medical"), 1, 0),
         Home_Services = if_else(str_detect(categories, "Home Services"), 1, 0),
         Hotels_Travel = if_else(str_detect(categories, "Hotels & Travel"), 1, 0),
         Local_Flavor = if_else(str_detect(categories, "Local Flavor"), 1, 0),
         Local_Services = if_else(str_detect(categories, "Local Services"), 1, 0),
         Mass_Media = if_else(str_detect(categories, "Mass Media"), 1, 0),
         Nightlife = if_else(str_detect(categories, "Nightlife"), 1, 0),
         Pets = if_else(str_detect(categories, "Pets"), 1, 0),
         Professional_Services = if_else(str_detect(categories, "Professional Services"), 1, 0),
         Public_Services_Government = if_else(str_detect(categories, "Public Services & Government"), 1, 0),
         Real_Estate = if_else(str_detect(categories, "Real Estate"), 1, 0),
         Religious_Organizations = if_else(str_detect(categories, "Religious Organizations"), 1, 0),
         Restaurants = if_else(str_detect(categories, "Restaurants"), 1, 0),
         Shopping = if_else(str_detect(categories, "Shopping"), 1, 0)
  )

# Need to figure out how to convert things into

# Also need to figure out what to do with opening hours.Some businesses don't bother filling out the times at which they're open, while others leave blank when they're not open, how would I deal with this?
# Either exclude NA observations with opening times or match opening days to the other most similar business's opening hours. I choose the latter.
business_hours <- business_data %>% 
  select(name, review_count, city, state, postal_code, is_open, categories, hours)

# Trying a crude method of isolating observations with all NA's for opening hours
b_hours <- business_data %>% 
  select(hours)
business_hours <- business_hours %>% 
  mutate(days_open = rowSums(!is.na(b_hours)))
business_data <- business_data %>% 
  mutate(days_open = rowSums(!is.na(b_hours)))
# hours_NA <- business_hours %>% 
#   filter(days_open == 0)

# There are 23k observations with all NA's not feasible to match opening days to other most similar business's hours manually
# Perhaps try certain conditions and have them match! Very problematic, even if I want to match a hot dog stand to another hot dog stand in the same city, I can't
# My categories are much too vague for me to try to match to specifically similar businesses.
# open_less_than_3 <- business_hours %>% 
#   filter(days_open > 0 & days_open < 3) 

# These are the problematic businesses, even closed businesses do list opening hours, there is just bad data colleciton here.
# Going online to check, one can easily check with google there is data collection problem here as some with 2 days filled are open on other days
# open_3 <- business_hours %>% 
#   filter(days_open == 3) # More often than not, 3 days open is set manually and the other days are closed, doesn't seem to be a data issue here

# open_2 <- business_hours %>% 
#   filter(days_open == 2) # Doing some google searches, it would seem these opening hours are also set manually but these short opening days seem to be temporary measures

# Owners tend to change them back to more normal 4/5-day-weeks, what to do in this case?
# Could they be outliers in this case? Should I drop them?
# Don't drop them, even if there are some outliers, dropping them when there are also business owners who are genuinely open.
# Try both ways and see what happens? Keep it first and fit a regression tree with it, for now we use days open and see what happens.

# Figure out business attributes now:
# business_attributes <- business_data %>% 
#   select(name, categories, state, postal_code, attributes)
# unique_smoking <- unique(business_attributes$attributes$Smoking)
# print(unique_smoking)


# ggplot(business_data, aes(x = engagement)) +
#   geom_histogram(binwidth = 1, fill = "red", colour = "green") +
#   labs(title = "Histogram of Engagement Count", x = "Engagement count", y = "Frequency")
# Inspect categories:
# business_categories <- business_data %>% 
#   select(name, categories, engagement)

# Inspect attributes:
# business_attributes <- business_data %>% 
#   select(name, categories,attributes)
# Might need to go in and manually fish out useful attributes later on!

# Renaming business variables so no confusion when joining
colnames(business_data)[9] <- "business_stars"
colnames(business_data)[10] <- "business_review_count"


business_data_cleaned <- business_data %>% 
  select(business_id, city, state, latitude, longitude, business_stars, business_review_count, is_open, engagement, days_open, Arts_Entertainment, Automotive, 
         Beauty_Spas, Education, Event_Planning_Services, Financial_Services, Food, Health_Medical, Home_Services, Hotels_Travel, Local_Flavor, 
         Local_Services, Mass_Media, Nightlife, Pets, Professional_Services, Public_Services_Government, Real_Estate, Religious_Organizations, 
         Restaurants, Shopping)


checkin_data  <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json), laptops' path

# Check which businesses have checkins
checkin_data <- checkin_data %>% 
  mutate(checkin_count = 1 + str_count(date, ","))
colnames(checkin_data)[2] <- "checkin_dates"
business_data_cleaned <- left_join(business_data_cleaned, checkin_data, by = "business_id")
business_data_cleaned$checkin_dates <- NULL
sum(is.na(business_data_cleaned$checkin_count))
# 18416 have NA's, that means no one checked in? Need to verify whether there are 0's in the dataset!Cannot use sum(if_else) because of NA's present
summary(business_data_cleaned$checkin_count) # Min = 1, so NA means 0 checkin's
business_data_cleaned <- business_data_cleaned %>% 
  mutate(checkin_count = if_else(is.na(checkin_count), 0, checkin_count))


save(business_data_cleaned, file = "business_data_cleaned.Rdata")

load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/business_data_cleaned.Rdata")
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/business_data_cleaned.Rdata")

# load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/Small Datasets/yelp_review_small.Rda")
# load("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/Small Datasets/yelp_review_small.Rda")

review_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_review.json"))
review_data <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_review.json"))
save(review_data, file = "review_data.Rdata")
rm(review_data)
load("C:/Users/tantr/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/review_data.Rdata")
load("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/R projects/EC349-Assignment/review_data.Rdata")
# 6990247 obs

review_data <- review_data %>% 
  mutate(date = ymd_hms(date))
review_data <- review_data %>%
  mutate(
    review_since_date = as.Date(date),
    review_age = as.numeric(difftime(ymd("2023-01-01"), review_since_date, units = "days")) / 365.25, # Calculate duration in years
  )
review_data$date <- NULL
review_data$review_since_date <- NULL

# Not gonna do sentiment analysis for now, no time
review_data$text <- NULL

# inner_join review to user
review_data <- inner_join(review_data, user_data_cleaned, by = "user_id")
review_data <- inner_join(review_data, business_data_cleaned, by = "business_id")
review_data$yelping_since <- NULL

save(review_data, file = "review_data_cleaned.Rdata")

# To calculate total number of tips a user gave out, make user_id in tip data factor and find how many times each factor shows up?


tip_data <- stream_in(file("C:/Users/Travis Tan/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_tip.json"))
tip_data <- stream_in(file("C:/Users/tantr/OneDrive - University of Warwick/EC349/Assignment 1/Assignment/yelp_academic_dataset_tip.json"))

# 908915 obs for tip data, trying to find how many tips each user and each business gave!
tip_data <- tip_data %>% 
  mutate(as.factor(user_id),
         as.factor(business_id))
# user_tip_count computation
user_level_counts <- table(tip_data$`as.factor(user_id)`)
user_tip_count <- as.data.frame(user_level_counts)
colnames(user_tip_count)[1] <- "user_id"
colnames(user_tip_count)[2] <- "user_tip_count"
user_tip_count <- user_tip_count %>% 
  mutate(user_id = as.character(user_id))

# business_tip_count computation
business_level_counts <- table(tip_data$`as.factor(business_id)`)
business_tip_count <- as.data.frame(business_level_counts)
colnames(business_tip_count)[1] <- "business_id"
colnames(business_tip_count)[2] <- "business_tip_count"
business_tip_count <- business_tip_count %>% 
  mutate(business_id = as.character(business_id))

save(user_tip_count, file = "user_tip_count.Rdata")
save(user_tip_count, file = "business_tip_count.Rdata")

review_data <- left_join(review_data, user_tip_count, by = "user_id")
review_data <- left_join(review_data, business_tip_count, by = "business_id")
sum(is.na(review_data$user_tip_count)) # 4184814 reviews by users who never gave tips
review_data <- review_data %>% 
  mutate(user_tip_count = if_else(is.na(user_tip_count), 0, user_tip_count))
sum(is.na(review_data$business_tip_count)) # 492433 reviews for businesses who never received tips
review_data <- review_data %>% 
  mutate(business_tip_count = if_else(is.na(business_tip_count), 0, business_tip_count))


save(review_data, file = "review_data_cleaned.Rdata")



