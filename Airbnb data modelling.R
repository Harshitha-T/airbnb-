#libraries
library(dplyr)
library(caret)
library(tidyr)
library(ggplot2)
library(leaps)
library(purrr)
library(imputeTS)
library(randomForest)
library(ranger)
library(stringr)
library(zipcode)

data("zipcode")

# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
#working directory-getwd
getwd()
setwd('C:/Users/harsh/Documents/Harshi/2019/Columbia/Fall/AA Frameworks & Methods 1/Kaggle')

# Read data and construct a simple model
data = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv')

#clean data

  #function to do word count on string
  word_count <- function(string) {
  return(sapply(strsplit(string, " "), length))
    }
  
  comma_count <- function(string) {
      return(str_count(string, ','))
  }
  
  #clean date
  date_string_to_int <- function(string) {
    return(as.numeric(as.POSIXct(string, format="%Y-%m-%d")))
  }
  
  data$first_review <- sapply(data$first_review, date_string_to_int)
  data$last_review <- sapply(data$last_review, date_string_to_int)
  data$host_since <- sapply(data$host_since, date_string_to_int)
  
  #replace N/A
  data$beds <- na_mean(data$beds)  
  data$reviews_per_month <- na_mean(data$reviews_per_month)
  data$cleaning_fee[is.na(data$cleaning_fee)] <- 0
  data$security_deposit[is.na(data$security_deposit)] <- 0
  
  #word count of text columns
  data$name <- sapply(as.character(data$name), word_count)
  data$summary <- sapply(as.character(data$summary), word_count)
  data$space <- sapply(as.character(data$space), word_count)
  data$description <- sapply(as.character(data$description), word_count)
  data$neighborhood_overview <- sapply(as.character(data$neighborhood_overview), word_count)
  data$notes <- sapply(as.character(data$notes), word_count)
  data$transit <- sapply(as.character(data$transit), word_count)
  data$access <- sapply(as.character(data$access), word_count)
  data$interaction <- sapply(as.character(data$interaction), word_count)
  data$house_rules <- sapply(as.character(data$house_rules), word_count)
  data$host_about <- sapply(as.character(data$host_about), word_count)
    
  #zipcode
  data$zipcode[data$zipcode == ""] <- "10002"
  data$zipcode[data$zipcode == "11103-3233"] <- "11103"
  data$zipcode[data$zipcode == "111211"] <- "11221"
  data$zipcode[data$zipcode == "11249\n11249"] <- "11249"
  data$zipcode[data$zipcode == "11385-2308"] <- "11385"
  data$zipcode[data$zipcode == "11413-3220"] <- "11413"
  data$zipcode[data$zipcode == "1m"] <- "10002"
  
  data <- merge(x = data, y = zipcode, by.x = "zipcode", by.y = "zip")
  
  
  #clean amenities
  data$amenities_count <- sapply(data$amenities, comma_count)
  data$amenities <- gsub(" ","",data$amenities)
  #Kitchen, Heating, Wifi, Airconditioning, TV, Internet, Washer, Dryer
  data$amenities_doorman<-as.factor(grepl('doorman',data$amenities,ignore.case=T))
  data$amenities_washer<-as.factor(grepl('washer',data$amenities,ignore.case=T))
  data$amenities_gym<-as.factor(grepl('gym',data$amenities,ignore.case=T))
  data$amenities_tv<-as.factor(grepl('tv',data$amenities,ignore.case=T))
  data$amenities_elevator<-as.factor(grepl('elevator',data$amenities,ignore.case=T))
  data$amenities_crib<-as.factor(grepl('crib',data$amenities,ignore.case=T))
  data$amenities_shampoo<-as.factor(grepl('shampoo',data$amenities,ignore.case=T))
  data$amenities_breakfast<-as.factor(grepl('breakfast',data$amenities,ignore.case=T))
  data$amenities_wifi<-as.factor(grepl('wifi',data$amenities,ignore.case=T))
  data$amenities_heating<-as.factor(grepl('heating',data$amenities,ignore.case=T))
  data$amenities_kitchen<-as.factor(grepl('kitchen',data$amenities,ignore.case=T))
  data$amenities_pool<-as.factor(grepl('pool',data$amenities,ignore.case=T))
  data$amenities_airconditioning<-as.factor(grepl('airconditioning',data$amenities,ignore.case=T))
  data$amenities_essentials<-as.factor(grepl('essentials',data$amenities,ignore.case=T))
  
  #add levels
  levels(data$property_type) <- c(levels(data$property_type),"Casa particular (Cuba)", "Castle", "Farm stay")
  levels(data$neighbourhood_cleansed) <- c(levels(data$neighbourhood_cleansed),"Howland Hook","New Dorp", "New Dorp Beach")
  data$first_review <- na_mean(data$first_review)
  data$last_review <- na_mean(data$last_review)
  data$host_since <- na_mean(data$host_since)

  #review data
    data %>% select(price,
    minimum_nights,
    review_scores_rating,
    bedrooms,
    bathrooms,
    beds,
    review_scores_accuracy,
    review_scores_cleanliness,
    review_scores_checkin,
    review_scores_communication,
    review_scores_location,
    review_scores_value,
    room_type,
    host_is_superhost,
    neighbourhood_group_cleansed,
    accommodates,
    reviews_per_month,
    guests_included,
    extra_people,
    minimum_nights_avg_ntm,
    availability_30,
    availability_90,
    cancellation_policy,
    property_type,
    cleaning_fee,
    number_of_reviews_ltm,
    reviews_per_month,
    instant_bookable,
    name,
    summary,
    space,
    description,
    neighborhood_overview,
    notes,
    transit,
    access,
    interaction,
    house_rules,
    host_about,
    first_review,
    last_review,
    host_since,
    amenities_doorman,
    amenities_washer,
    amenities_gym,
    amenities_tv,
    amenities_elevator,
    amenities_crib,
    amenities_shampoo,
    amenities_breakfast,
    amenities_wifi,
    amenities_heating,
    amenities_kitchen,
    amenities_pool,
    amenities_airconditioning,
    amenities_essentials,
    security_deposit) %>% summary()
  
#train 
set.seed(617)
tuneGrid = expand.grid(mtry=30,splitrule="variance",min.node.size=5)
train_control=trainControl(method='cv', number=10)
model = train(price~
               minimum_nights+
               review_scores_rating+
               bedrooms+
               bathrooms+
               beds+
               review_scores_accuracy+
               review_scores_cleanliness+
               review_scores_checkin+
               review_scores_communication+
               review_scores_location+
               review_scores_value+
               room_type+
               host_is_superhost+
               neighbourhood_group_cleansed+
               accommodates+
               reviews_per_month+
               guests_included+
               extra_people+
               minimum_nights_avg_ntm+
               availability_30+
               availability_90+
               cancellation_policy+
               property_type+
               cleaning_fee+
               number_of_reviews_ltm+
               reviews_per_month+
               instant_bookable+
               name+
               summary+
               space+
               description+
               neighborhood_overview+
               notes+
               transit+
               access+
               interaction+
               house_rules+
               host_about+
               first_review+
               last_review+
               host_since+
               amenities_doorman+
               amenities_washer+
               amenities_gym+
               amenities_tv+
               amenities_elevator+
               amenities_crib+
               amenities_shampoo+
               amenities_breakfast+
               amenities_wifi+
               amenities_heating+
               amenities_kitchen+
               amenities_pool+
               amenities_airconditioning+
               amenities_essentials+
               security_deposit+
                latitude +
                longitude,
data=data, method='ranger',trControl=train_control,tuneGrid=tuneGrid)
model

# Apply on scoringData

  #clean date
  scoringData$first_review <- sapply(scoringData$first_review, date_string_to_int)
  scoringData$last_review <- sapply(scoringData$last_review, date_string_to_int)
  scoringData$host_since <- sapply(scoringData$host_since, date_string_to_int)

  #replace N/A
  scoringData$beds <- na_mean(scoringData$beds)  
  scoringData$reviews_per_month <- na_mean(scoringData$reviews_per_month)
  scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- 0
  scoringData$security_deposit[is.na(scoringData$security_deposit)] <- 0

  #word count of text columns
  scoringData$name <- sapply(as.character(scoringData$name), word_count)
  scoringData$summary <- sapply(as.character(scoringData$summary), word_count)
  scoringData$space <- sapply(as.character(scoringData$space), word_count)
  scoringData$description <- sapply(as.character(scoringData$description), word_count)
  scoringData$neighborhood_overview <- sapply(as.character(scoringData$neighborhood_overview), word_count)
  scoringData$notes <- sapply(as.character(scoringData$notes), word_count)
  scoringData$transit <- sapply(as.character(scoringData$transit), word_count)
  scoringData$access <- sapply(as.character(scoringData$access), word_count)
  scoringData$interaction <- sapply(as.character(scoringData$interaction), word_count)
  scoringData$house_rules <- sapply(as.character(scoringData$house_rules), word_count)
  scoringData$host_about <- sapply(as.character(scoringData$host_about), word_count)

  #zipcode
  scoringData$zipcode[scoringData$zipcode == ""] <- "10002"
  scoringData$zipcode[scoringData$zipcode == "11103-3233"] <- "11103"
  scoringData$zipcode[scoringData$zipcode == "111211"] <- "11221"
  scoringData$zipcode[scoringData$zipcode == "11249\n11249"] <- "11249"
  scoringData$zipcode[scoringData$zipcode == "11385-2308"] <- "11385"
  scoringData$zipcode[scoringData$zipcode == "11413-3220"] <- "11413"
  scoringData$zipcode[scoringData$zipcode == "1m"] <- "10002"

  scoringData <- merge(x = scoringData, y = zipcode, by.x = "zipcode", by.y = "zip")


  #clean amenities
  scoringData$amenities_count <- sapply(scoringData$amenities, comma_count)
  scoringData$amenities <- gsub(" ","",scoringData$amenities)
  #Kitchen, Heating, Wifi, Airconditioning, TV, Internet, Washer, Dryer
  scoringData$amenities_doorman<-as.factor(grepl('doorman',scoringData$amenities,ignore.case=T))
  scoringData$amenities_washer<-as.factor(grepl('washer',scoringData$amenities,ignore.case=T))
  scoringData$amenities_gym<-as.factor(grepl('gym',scoringData$amenities,ignore.case=T))
  scoringData$amenities_tv<-as.factor(grepl('tv',scoringData$amenities,ignore.case=T))
  scoringData$amenities_elevator<-as.factor(grepl('elevator',scoringData$amenities,ignore.case=T))
  scoringData$amenities_crib<-as.factor(grepl('crib',scoringData$amenities,ignore.case=T))
  scoringData$amenities_shampoo<-as.factor(grepl('shampoo',scoringData$amenities,ignore.case=T))
  scoringData$amenities_breakfast<-as.factor(grepl('breakfast',scoringData$amenities,ignore.case=T))
  scoringData$amenities_wifi<-as.factor(grepl('wifi',scoringData$amenities,ignore.case=T))
  scoringData$amenities_heating<-as.factor(grepl('heating',scoringData$amenities,ignore.case=T))
  scoringData$amenities_kitchen<-as.factor(grepl('kitchen',scoringData$amenities,ignore.case=T))
  scoringData$amenities_pool<-as.factor(grepl('pool',scoringData$amenities,ignore.case=T))
  scoringData$amenities_airconditioning<-as.factor(grepl('airconditioning',scoringData$amenities,ignore.case=T))
  scoringData$amenities_essentials<-as.factor(grepl('essentials',scoringData$amenities,ignore.case=T))

  #add levels
  levels(scoringData$property_type) <- c(levels(scoringData$property_type),"Casa particular (Cuba)", "Castle", "Farm stay")
  levels(scoringData$neighbourhood_cleansed) <- c(levels(scoringData$neighbourhood_cleansed),"Howland Hook","New Dorp", "New Dorp Beach")
  scoringData$first_review <- na_mean(scoringData$first_review)
  scoringData$last_review <- na_mean(scoringData$last_review)
  scoringData$host_since <- na_mean(scoringData$host_since)

#scoringData model predictions
pred = predict(model,newdata=scoringData)

# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)