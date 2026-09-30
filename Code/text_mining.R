# Packages: see setup.R

library(tm)
library(tidytext)
library(textTinyR)
library(tidyverse)
library(randomForest)
library(caret)
library(here)

# setwd removed — here::here() handles project-relative paths

desc <- read_csv(here::here("Data", "raw files", "raw_description.csv"))

#Preprocess the text data: convert to lowercase, remove stopwords, and tokenize
clean_text <- function(text_data) {
  text_data %>%
    mutate(text_data = removePunctuation(fund_description),
           text_data = removeNumbers(text_data),
           text_data = stripWhitespace(text_data),
           text_data = tolower(text_data)) 
}

# Full sample group of both categories. Removes description exercept
sampleGroup <- df %>%
  filter(year>=2017) %>%
  left_join(desc[c('campaign_id','fund_name','fund_description')],join_by(id==campaign_id)) %>%
  mutate(
         fund_description = gsub("\",\"fund_description_excerpt\":\".*","",fund_description),
         fund_description = removePunctuation(fund_description),
         fund_description = removeNumbers(fund_description),
         fund_description = stripWhitespace(fund_description),
         fund_description = tolower(fund_description),
         word_count = str_count(fund_description,'\\w+'),
         treatedProb = if_else(category=='financial emergency',1,0),
         category = as.factor(category)
  )

words <- c(' rent ','groceries','food','monthly bills','utilities','unemployment','unemployed','household bills')

sampleGroup <- cbind(sampleGroup, sapply(words, function(x) as.integer(grepl(x, sampleGroup$fund_description))))
sampleGroup <- sampleGroup %>%
  rename(monthly_bills = 'monthly bills', rent = ' rent ', household_bills = 'household bills') 



# Step 3: Preprocess the data (Optional, depending on your dataset)
# In this case, the iris dataset doesn't need much preprocessing, but if you have categorical variables or missing values, you should handle them here.

# Step 4: Split the data into training and testing sets
set.seed(42)  # For reproducibility
#train_index <- sample(1:nrow(sampleGroup), 0.8 * nrow(sampleGroup))  # 80% for training
train_data <- sampleGroup %>%
  filter(date(created_at)>='2020-10-01')
test_data <- sampleGroup %>%
  filter(date(created_at)<'2020-10-01')

# Step 5: Create a Random Forest model

rf_model <- randomForest(category ~ goal_amount + total_photos + 
                          total_updates + state + rent + food + groceries + monthly_bills + utilities + unemployment + unemployed + household_bills + word_count, data=train_data)

# Step 6: Print the model summary
print(rf_model)

# Step 7: Evaluate the model performance on the test set

predictions <- predict(rf_model, newdata=test_data,type='prob')
predictions <- factor(colnames(predictions)[max.col(predictions)])
conf_matrix <- table(colnames(predictions)[max.col(predictions)], test_data[,88])

caret::confusionMatrix(predictions, test_data$category)


test_data <- cbind(test_data,predictions)    
