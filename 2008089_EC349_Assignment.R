#Load necessary libraries
library(bnstruct)
library(caret)
library(cld3)
library(ggplot2)
library(hexbin)
library(janitor)
library(jsonlite)
library(lubridate)
library(mice)
library(pROC)
library(randomForest)
library(rpart)
library(sentimentr)
library(tidytext)
library(tidyverse)
library(tree)
library(VIM)
library(zoo)

#Clear
cat("\014")  
rm(list=ls())

#Set directory 
setwd("/Users/keith/Desktop/Hustle/Y3/EC349/Assignment")

#Load different data
business_data <- stream_in(file("/Users/keith/Desktop/Hustle/Y3/EC349/Assignment/yelp_academic_dataset_business.json.nosync.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("/Users/keith/Desktop/Hustle/Y3/EC349/Assignment/yelp_academic_dataset_checkin.json.nosync.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data <- stream_in(file("/Users/keith/Desktop/Hustle/Y3/EC349/Assignment/yelp_academic_dataset_tip.json.nosync.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
load(file="/Users/keith/Desktop/Hustle/Y3/EC349/Assignment/Small Datasets.zip.nosync/yelp_review_small.Rda")
load(file="/Users/keith/Desktop/Hustle/Y3/EC349/Assignment/Small Datasets.zip.nosync/yelp_user_small.Rda")

#Merge data sets
combined_data <- merge(merge(review_data_small, user_data_small, by="user_id", all.x=TRUE), business_data, by="business_id", all.x=TRUE)

#Rename columns
combined_data <- combined_data %>% 
  rename(
    stars_review = stars.x,
    stars_business = stars.y,
    useful_review = useful.x,
    useful_user = useful.y,
    funny_review = funny.x,
    funny_user = funny.y,
    cool_review = cool.x,
    cool_user = cool.y,
    review_count_user = review_count.x,
    review_count_business = review_count.y
  )

#Clean data
#Unnest the dataframes
combined_data <- do.call(data.frame, combined_data)
business_data <- do.call(data.frame, business_data)

#Clean names
combined_data <- combined_data %>%
  clean_names() %>%
  rename(attributes_wifi = attributes_wi_fi)

business_data <- business_data %>%
  clean_names() %>%
  rename(attributes_wifi = attributes_wi_fi)

#Convert columns to desired variable types
combined_data$date <- as.Date(combined_data$date, format = "%Y-%m-%d")
combined_data$year <- as.numeric(format(combined_data$date, "%Y"))
combined_data$month <- as.numeric(format(combined_data$date, "%m"))
combined_data <- combined_data %>%
  mutate(city = factor(city),
         state = factor(state),
  )

#Exploratory data analysis
#Check for time trend
combined_data$stars_review <- as.numeric(combined_data$stars_review)
mean_over_time <- aggregate(stars_review ~ month + year, combined_data, mean)
mean_over_time$date <- as.yearmon(paste(mean_over_time$year, mean_over_time$month, sep="-"), format = "%Y-%m")
ggplot(mean_over_time, aes(x = date, y = stars_review)) + geom_line() + labs(y="Mean user rating", x="Date")
#No clear trend, simply looks like a white noise process, especially from Jan 2010 onwards

#Check for seasonality
mean_over_time_subset <- mean_over_time[156:204,]
ggplot(mean_over_time_subset, aes(x = date, y = stars_review)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.05) +
  labs(x = "Date",
       y = "User Rating") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#No clear seasonal pattern

#Drop rows with non-English reviews
combined_data <- combined_data %>%
  mutate(detected_language = sapply(text, detect_language)) %>% 
  filter(detected_language == "en") %>%
  select(-detected_language)

# Generate sentiment scores for each review
combined_data <- combined_data %>%
  rowwise() %>%
  mutate(
    sentiment = mean(sentiment(get_sentences(text))$sentiment, na.rm=TRUE)
  )

#Prepare data to impute missing values for attributes_wifi and attributes_business_accepts_credit_cards
#Clean attributes_wifi column
business_data["attributes_wifi"][business_data["attributes_wifi"] == "u'free'"] <- "'free'"
business_data["attributes_wifi"][business_data["attributes_wifi"] == "u'no'"] <- "'no'"
business_data["attributes_wifi"][business_data["attributes_wifi"] == "None"] <- "'no'"
business_data["attributes_wifi"][business_data["attributes_wifi"] == "u'paid'"] <- "'paid'"

#Clean attributes_business_accepts_credit_cards column
business_data["attributes_business_accepts_credit_cards"][business_data["attributes_business_accepts_credit_cards"] == "None"] <- "False"
business_data <- business_data |>
  mutate(attributes_wifi = factor(attributes_wifi), 
         attributes_business_accepts_credit_cards = factor(attributes_business_accepts_credit_cards))

#Impute missing values for attributes_wifi and attributes_business_accepts_credit_cards using relevant business attributes
business_data_impute <- subset(business_data, select = c("business_id", "attributes_business_accepts_credit_cards", "attributes_wifi", "latitude", "longitude", "stars"))
imputed_data <- mice(business_data, method = "pmm")
business_data_impute <- complete(imputed_data)
combined_data <- merge(combined_data, subset(business_data_impute, select = c(business_id, attributes_wifi, attributes_business_accepts_credit_cards)), by="business_id", all.x=TRUE)

#Remove the original columns
combined_data <- combined_data %>%
  select(-attributes_wifi.x, -attributes_business_accepts_credit_cards.x) %>%
  rename(attributes_wifi = attributes_wifi.y,
         attributes_business_accepts_credit_cards = attributes_business_accepts_credit_cards.y)

#Convert attributes_business_accepts_credit_cards to a logical variable
combined_data$attributes_business_accepts_credit_cards <- combined_data$attributes_business_accepts_credit_cards == "True"

#Prepare data to impute missing values in user-specific features
user_and_review <- merge(review_data_small, user_data_small, by="user_id", all.x=TRUE)
user_and_review <- user_and_review %>% 
  rename(
    stars_review = stars,
    useful_review = useful.x,
    useful_user = useful.y,
    funny_review = funny.x,
    funny_user = funny.y,
    cool_review = cool.x,
    cool_user = cool.y,
    review_count_user = review_count,
  )
#Impute missing values using relevant user attributes
user_and_review <- subset(user_and_review, select = c(-text, -business_id, -user_id, -date, -name, -elite, -compliment_hot, -friends, -compliment_profile, -compliment_cute, -compliment_list, -compliment_list, -compliment_note, -compliment_plain, -compliment_photos))
imputed_data <- mice(user_and_review, method = "pmm")
user_and_review_impute <- complete(imputed_data)
combined_data <- merge(combined_data, subset(user_and_review_impute, select = c(review_id, review_count_user, useful_user, funny_user, cool_user, average_stars, fans)), by="review_id", all.x=TRUE)

#Merge dataframe with imputed data with combined_data dataframe and remove original columns
combined_data <- combined_data %>%
  select(-review_count_user.x, -useful_user.x, -funny_user.x, -cool_user.x) %>%
  rename(review_count_user = review_count_user.y , 
         useful_user = useful_user.y, 
         funny_user = funny_user.y, 
         cool_user = cool_user.y, 
         average_stars = average_stars.y,
         fans = fans.y)

#Split the data into training and test sets
set.seed(1)
sample <- sample(seq_len(nrow(combined_data)), size=10000)
test <- combined_data[sample, ]
train <- combined_data[-sample, ]

#Oversample training data
combined_data$stars_review <- as.factor(combined_data$stars_review)
train_oversample <- upSample(train[c("useful_review", "funny_review", "cool_review", "useful_user", "funny_user", "cool_user", "fans", "average_stars", "attributes_wifi", "attributes_business_accepts_credit_cards", "sentiment", "latitude", "longitude", "stars_business", "review_count_business", "review_count_user")], train$stars_review, yname="stars_review")

#Random Forest model with oversampling
model <- randomForest(train_oversample[c("useful_review", "funny_review", "cool_review", "useful_user", "funny_user", "cool_user", "fans", "average_stars", "attributes_wifi", "attributes_business_accepts_credit_cards", "sentiment", "latitude", "longitude", "stars_business", "review_count_business", "review_count_user")], y=train_oversample[, "stars_review"], replace=TRUE, ntree=64, importance=TRUE)

# Model prediction
prediction_train <- predict(model, train_oversample[c("stars_review", "useful_review", "funny_review", "cool_review", "useful_user", "funny_user", "cool_user", "fans", "average_stars", "attributes_wifi", "attributes_business_accepts_credit_cards", "sentiment", "latitude", "longitude", "stars_business", "review_count_business", "review_count_user")], type="class")
prediction_test <- predict(model, test[c("stars_review", "useful_review", "funny_review", "cool_review", "useful_user", "funny_user", "cool_user", "fans", "average_stars", "attributes_wifi", "attributes_business_accepts_credit_cards", "sentiment", "latitude", "longitude", "stars_business", "review_count_business", "review_count_user")], type="class")

#Evaluate model performance on training data
prediction_train_num <- as.numeric(prediction_train)
multiclass.roc(train_oversample$stars_review, prediction_train_num) 

#Evaluate model performance on test data
prediction_test_num <- as.numeric(prediction_test)
result <- multiclass.roc(test$stars_review, prediction_test_num)

#Visualisations
#Visualise feature importances
varImpPlot(model, sort=TRUE)

#Visualise ROC curves
plot.roc(result$rocs[[1]], xlim=c(1,0),
         legacy.axes = T)
plot.roc(result$rocs[[2]],
         add=T, col = 'red',
         legacy.axes = T,)
plot.roc(result$rocs[[3]],
         add=T, col = 'blue',
         legacy.axes = T,)
plot.roc(result$rocs[[4]],
         add=T, col = 'green',
         legacy.axes = T,)
plot.roc(result$rocs[[5]],
         add=T, col = 'yellow',
         legacy.axes = T,)
plot.roc(result$rocs[[6]],
         add=T, col = 'pink',
         legacy.axes = T,)
plot.roc(result$rocs[[7]],
         add=T, col = 'skyblue',
         legacy.axes = T,)
plot.roc(result$rocs[[8]],
         add=T, col = 'grey',
         legacy.axes = T,)
plot.roc(result$rocs[[9]],
         add=T, col = 'violet',
         legacy.axes = T,)
plot.roc(result$rocs[[10]],
         add=T, col = 'magenta',
         legacy.axes = T,)

#Visualise class imbalance
stars_dist <- table(combined_data$stars_review)
options(scipen=999)
barplot(stars_dist, 
        xlab = "User rating",
        ylab = "Frequency",
        col = "skyblue")