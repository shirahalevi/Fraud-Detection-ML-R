rm(list = ls())


Sys.setenv(TZ="UTC")
Sys.setlocale("LC_TIME", "English_United States")


install.packages("lubridate")
library(lubridate)

setwd("C:/Users/shirah/OneDrive - Mobileye/Documents/EnglishPath")
fraud.data.raw <- read.csv("fraud_data.csv")
fraud.data.prepered <- fraud.data.raw

str(fraud.data.prepered)
head(fraud.data.prepered)




#-------------------------data preperation-----------------------------#



# Convert 'transDateTransTime' to POSIXct

fraud.data.prepered$transDateTransTime <- as.POSIXct(fraud.data.prepered$transDateTransTime, format = "%m/%d/%y %H:%M")

fraud.data.prepered$month <- month(fraud.data.prepered$transDateTransTime)

fraud.data.prepered$year <- year(fraud.data.prepered$transDateTransTime)

fraud.data.prepered$hour <- hour(fraud.data.prepered$transDateTransTime)

fraud.data.prepered$transaction_weekday <- weekdays(fraud.data.prepered$transDateTransTime)

fraud.data.prepered$transaction_weekday <- as.factor(fraud.data.prepered$transaction_weekday)

str(fraud.data.prepered)
head(fraud.data.prepered)




# Convert categorical columns to factors

fraud.data.prepered$merchant <- as.factor(fraud.data.prepered$merchant)

fraud.data.prepered$category <- as.factor(fraud.data.prepered$category)

fraud.data.prepered$city <- as.factor(fraud.data.prepered$city)

fraud.data.prepered$state <- as.factor(fraud.data.prepered$state)

fraud.data.prepered$job <- as.factor(fraud.data.prepered$job)

str(fraud.data.prepered)



# Handling is_fraud values

install.packages("dplyr")
library(dplyr)


# Clean invalid values in the 'is_fraud' column and remove rows with invalid or NA values

fraud.data.prepered$is_fraud <- ifelse(fraud.data.prepered$is_fraud %in% c("0", "1"), fraud.data.prepered$is_fraud, NA)
fraud.data.prepered <- fraud.data.prepered[!is.na(fraud.data.prepered$is_fraud), ]

# Convert 'is_fraud' to a factor

fraud.data.prepered$is_fraud <- as.factor(fraud.data.prepered$is_fraud)

table(fraud.data.prepered$is_fraud)

# Drop unnecessary columns

fraud.data.prepered <- fraud.data.prepered[, !names(fraud.data.prepered) %in% "trans_num"]

# Convert 'dob' to Date format

fraud.data.prepered$dob <- as.Date(fraud.data.prepered$dob, format = "%m/%d/%Y")

# Replace missing DOB values with the median DOB

median_dob <- median(fraud.data.prepered$dob, na.rm = TRUE)

fraud.data.prepered$dob[is.na(fraud.data.prepered$dob)] <- median_dob

# Calculate age based on 'dob'

fraud.data.prepered$age <- floor(as.numeric(difftime(Sys.Date(), fraud.data.prepered$dob, units = "days")) / 365.25)

# Replace missing 'age' values with the median age

median_age <- median(fraud.data.prepered$age, na.rm = TRUE)

fraud.data.prepered$age[is.na(fraud.data.prepered$age)] <- median_age

table(fraud.data.prepered$is_fraud)




#--------------------------------------------EDA-------------------------------------------------




install.packages("ggplot2")
library(ggplot2)
library(dplyr)




# Grouping rare categories


group_rare_categories <- function(variable, threshold = 0.05) {
  freq <- prop.table(table(variable))
  rare_categories <- names(freq[freq < threshold])
  variable <- as.character(variable)
  variable[variable %in% rare_categories] <- "Other"
  return(as.factor(variable))
}



fraud.data.prepered$job <- group_rare_categories(fraud.data.prepered$job, threshold = 0.01)
table(fraud.data.prepered$job)

fraud.data.prepered$city <- group_rare_categories(fraud.data.prepered$city, threshold = 0.01)
table(fraud.data.prepered$city)

fraud.data.prepered$merchant <- group_rare_categories(fraud.data.prepered$merchant, threshold = 0.002)
table(fraud.data.prepered$merchant)


str(fraud.data.prepered)


# Frequency Encoding for merchant

merchant_frequency <- fraud.data.prepered %>%
  count(merchant) %>%
  rename(merchant_frequency = n)

fraud.data.prepered <- fraud.data.prepered %>%
  left_join(merchant_frequency, by = "merchant") %>%
  mutate(merchant = merchant_frequency) %>%
  select(-merchant_frequency)



# General tests


# Transaction Distribution by City

city_counts <- fraud.data.prepered %>%
  group_by(city) %>%
  summarise(count = n())

ggplot(city_counts, aes(x = reorder(city, -count), y = count)) +
  geom_col(fill = "steelblue") +
  labs(title = "מספר העסקאות לפי עיר", x = "עיר", y = "מספר עסקאות") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# The division of transactions by months

ggplot(fraud.data.prepered, aes(x = as.factor(month))) + 
  geom_bar(fill = "steelblue")

table(fraud.data.prepered$month)




# The division of transactions by hour

ggplot(fraud.data.prepered, aes(x = as.factor(hour))) + 
  geom_bar(fill = "blue") + 
  labs(title = "Number of Transactions by Hour", x = "Hour of the Day", y = "Count of Transactions") +
  theme_minimal()

table(fraud.data.prepered$hour)



# The division of transactions by weekday

table(fraud.data.prepered$transaction_weekday)





# Calculate the distance between cardholder and merchant location


haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth radius in kilometers
  delta_lat <- as.numeric(lat2 - lat1) * pi / 180
  delta_lon <- as.numeric(lon2 - lon1) * pi / 180
  a <- sin(delta_lat/2)^2 + cos(as.numeric(lat1) * pi / 180) * cos(as.numeric(lat2) * pi / 180) * sin(delta_lon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}

fraud.data.prepered$distance_to_merchant <- mapply(haversine_distance, 
                                                   fraud.data.prepered$lat, 
                                                   fraud.data.prepered$long, 
                                                   fraud.data.prepered$merch_lat, 
                                                   fraud.data.prepered$merch_long);

str(fraud.data.prepered)



# Creating Age Groups


install.packages("dplyr") 
library(dplyr)

fraud.data.prepered <- fraud.data.prepered %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 30 ~ "18-30",
    age >= 31 & age <= 45 ~ "31-45",
    age >= 46 & age <= 60 ~ "46-60",
    age > 60 ~ "61+",
    TRUE ~ "Unknown"
  ))

str(fraud.data.prepered)



# Ratio between transaction amount and distance


fraud.data.prepered$amt_to_distance_ratio <- fraud.data.prepered$amt / fraud.data.prepered$distance_to_merchant

str(fraud.data.prepered)


# Analysis of Fraud Occurrence Across Different Age Groups

library(ggplot2)

ggplot(fraud.data.prepered) +
  geom_bar(mapping = aes(x = age_group, fill = is_fraud)) +
  labs(title = "Fraud by Age Group", x = "Age Group", y = "Count") +
  theme_minimal()


ggplot(fraud.data.prepered) +
  geom_bar(mapping = aes(x = age_group, fill = is_fraud), position = 'fill') +
  labs(title = "Fraud Proportion by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal()



# Analysis of Fraud Occurrence Across Different States

ggplot(fraud.data.prepered) +
  geom_bar(mapping = aes(x = state, fill = is_fraud)) +
  labs(title = "Fraud by State", x = "State", y = "Count") +
  theme_minimal()

ggplot(fraud.data.prepered) +
  geom_bar(mapping = aes(x = state, fill = is_fraud), position = 'fill') +
  labs(title = "Fraud Proportion by State", x = "State", y = "Proportion") +
  theme_minimal()



# -----------

# Perform oversampling on the target variable 'is_fraud'

library(lattice)
library(caret)

balanced_data <- upSample(x = fraud.data.prepered[, -which(names(fraud.data.prepered) == "is_fraud")], 
                          y = fraud.data.prepered$is_fraud)

table(balanced_data$Class)

# -----------




# Analysis of Fraud Occurrence Across Different Age Groups -  After Performing Oversampling

ggplot(balanced_data, aes(x = age_group, fill = Class)) +
  geom_bar() +
  labs(title = "Fraud by Age Group (After Oversampling)", x = "Age Group", y = "Count") +
  theme_minimal()

ggplot(balanced_data, aes(x = age_group, fill = Class)) +
  geom_bar(position = 'fill') +
  labs(title = "Fraud Proportion by Age Group (After Oversampling)", x = "Age Group", y = "Proportion") +
  theme_minimal()


# הגרפים לא השתנו כל כך וזה מרמז על כך שלגיל אין השפעה רבה על משתנה המטרה




# Analysis of Fraud Occurrence Across Different States - After Performing Oversampling

ggplot(balanced_data, aes(x = state, fill = Class)) +
  geom_bar() +
  labs(title = "Fraud by State (After Oversampling)", x = "State", y = "Count") +
  theme_minimal()

ggplot(balanced_data, aes(x = state, fill = Class)) +
  geom_bar(position = 'fill') +
  labs(title = "Fraud Proportion by State (After Oversampling)", x = "State", y = "Proportion") +
  theme_minimal()


#הגרפים כן השתנו לכן זה משתנה שאכן משפיע יותר על עמודת המטרה 





# Exploring and Adjusting Transaction Amount Distribution

baseTransactionAmount <- ggplot(fraud.data.prepered, aes(x = amt, fill = is_fraud)) +
  geom_histogram(binwidth = 500) +
  labs(title = "Distribution of Transaction Amounts by Fraud Status", 
       x = "Transaction Amount", 
       y = "Count") +
  theme_minimal()

baseTransactionAmount

summary(fraud.data.prepered$amt)

# Capping Transaction Amounts at $1000
limitTransactionAmount <- function(x, limit) {
  if (x > limit) {
    return(limit)
  }
  return(x)
}

fraud.data.prepered$amt <- sapply(fraud.data.prepered$amt, limitTransactionAmount, limit = 1000)

baseTransactionAmount <- ggplot(fraud.data.prepered, aes(x = amt, fill = is_fraud)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Distribution of Transaction Amounts by Fraud Status (Capped at $1000)", 
       x = "Transaction Amount ($)", 
       y = "Count") +
  theme_minimal()

baseTransactionAmount



# Transaction Amounts by Fraud Status - box plot

ggplot(fraud.data.prepered, aes(x = is_fraud, y = amt, fill = is_fraud)) +
  geom_boxplot() +
  labs(title = "Transaction Amount Distribution by Fraud Status", 
       x = "Fraud Status", 
       y = "Transaction Amount") +
  theme_minimal()



# Examination of correlation between numerical variables

numeric_vars <- fraud.data.prepered[, sapply(fraud.data.prepered, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")

print(cor_matrix)




#--------------------------------------------General data analysis-------------------------------------------------



library(ggplot2)
library(dplyr)

# fraud count by Hourly

fraud_by_hour <- fraud.data.prepered %>%
  group_by(hour) %>%
  summarise(fraud_count = sum(is_fraud == 1), .groups = 'drop')


ggplot(fraud_by_hour, aes(x = hour, y = fraud_count)) +
  geom_col(fill = "red") +
  labs(title = "Fraud Occurrences by Hour of the Day", x = "Hour of the Day", y = "Number of Frauds") +
  theme_minimal()

# Fraud analysis by location

ggplot(fraud.data.prepered[fraud.data.prepered$is_fraud == 1,], aes(x = long, y = lat)) +
  geom_point(alpha = 0.4, color = "orange") +
  labs(title = "Geographic Distribution of Frauds", x = "Longitude", y = "Latitude") +
  theme_minimal()

# The relationship between the fraud and the transaction amount

ggplot(fraud.data.prepered, aes(x = amt, y = as.factor(is_fraud))) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "green") +
  labs(title = "Transaction Amount vs. Fraud Occurrence", x = "Transaction Amount", y = "Fraud Occurrence") +
  theme_minimal()


# Fraud analysis by category

fraud_by_category <- fraud.data.prepered %>%
  group_by(category) %>%
  summarise(fraud_count = sum(is_fraud == 1), total_count = n(), fraud_rate = fraud_count / total_count, .groups = 'drop') %>%
  arrange(desc(fraud_rate))


ggplot(fraud_by_category, aes(x = reorder(category, -fraud_rate), y = fraud_rate)) +
  geom_col(fill = "red") +
  labs(title = "שיעור ההונאות לפי קטגוריית מסחר", x = "קטגוריית מסחר", y = "שיעור ההונאות (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Correlation analysis between transaction amount and merchant distance

# Relationship between Fraud and Distance to Merchant


ggplot(fraud.data.prepered, aes(x = is_fraud, y = distance_to_merchant)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Relationship between Fraud and Distance to Merchant", 
       x = "Fraud (0 = No, 1 = Yes)", 
       y = "Distance to Merchant (km)") +
  theme_minimal()

# Analysis of the average distance
avg_distance <- fraud.data.prepered %>%
  group_by(is_fraud) %>%
  summarise(avg_distance = mean(distance_to_merchant, na.rm = TRUE))

print(avg_distance)




#--------------------------------------------Running models-------------------------------------------------



# --------- Logistic regression ---------


install.packages("caTools")
library(caTools)

set.seed(123)

# Split the data into training (70%) and testing (30%) sets

split <- sample.split(fraud.data.prepered$is_fraud, SplitRatio = 0.7)
train_data <- subset(fraud.data.prepered, split == TRUE)
test_data <- subset(fraud.data.prepered, split == FALSE)



# Align factor levels in test data to match the training data

for (col in names(train_data)) {
  if (is.factor(train_data[[col]])) {
    test_data[[col]] <- factor(test_data[[col]], levels = levels(train_data[[col]]))
  }
}

# Build the logistic regression model

logistic_model <- glm(is_fraud ~ ., data = train_data, family = binomial(link = "logit"))


# predictions 

predictions <- predict(logistic_model, newdata = test_data, type = "response")


# Convert predictions to binary classes

predicted_class <- ifelse(predictions > 0.5, "1", "0")


# Convert predicted and actual classes to factors for evaluation

predicted_class <- as.factor(predicted_class)

test_data$is_fraud <- as.factor(test_data$is_fraud)


# Confusion matrix to evaluate the model

conf_matrix <- confusionMatrix(predicted_class, test_data$is_fraud, positive = "1")


# Print model evaluation metrics

cat("Accuracy: ", conf_matrix$overall['Accuracy'], "\n")

cat("Sensitivity (Recall): ", conf_matrix$byClass['Sensitivity'], "\n")

cat("Specificity: ", conf_matrix$byClass['Specificity'], "\n")

cat("Precision: ", conf_matrix$byClass['Pos Pred Value'], "\n")

cat("Negative Predictive Value: ", conf_matrix$byClass['Neg Pred Value'], "\n")

cat("F1 Score: ", conf_matrix$byClass['F1'], "\n")



install.packages("pROC")
library(pROC)

# Calculate ROC curve for the logistic regression model

logistic_roc <- roc(test_data$is_fraud, predictions)

# Reset margins to avoid the "figure margins too large" error

par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust margins (bottom, left, top, right)

# Plot the ROC curve

plot(logistic_roc, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression Model")

# Add a diagonal line representing random guessing

abline(a = 0, b = 1, lty = 2, col = "red")

# Display the AUC value

auc_value <- auc(logistic_roc)
cat("AUC: ", auc_value, "\n")




# --------- XGBoost Model ---------


install.packages("xgboost")
install.packages("Matrix")
install.packages("pROC")
library(xgboost)
library(Matrix)
library(pROC)


# Convert categorical variables to numeric using one-hot encoding

train_matrix <- model.matrix(is_fraud ~ . - 1, data = train_data)  # -1 to avoid adding an intercept

test_matrix <- model.matrix(is_fraud ~ . - 1, data = test_data)


# Convert labels to numeric (0/1)

train_label <- as.numeric(train_data$is_fraud) - 1

test_label <- as.numeric(test_data$is_fraud) - 1


# Convert data to DMatrix format

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)

dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Set model parameters

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.1,
  max_depth = 6,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train the model

set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

# Make predictions

predictions <- predict(xgb_model, dtest)
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix

confusion_matrix <- table(Predicted = predicted_class, Actual = test_label)

# Accuracy

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Precision, Recall, F1 Score

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Display results

print("Confusion Matrix:")
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# Calculate the ROC curve and AUC

XGBoost_roc <- roc(test_label, predictions)
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for XGBoost Model")
abline(a = 0, b = 1, lty = 2, col = "red")
auc_value <- auc(roc_curve)
cat("AUC: ", auc_value, "\n")




# --------- Support Vector Machine (SVM) ---------

install.packages("e1071")
install.packages("caTools")
library(e1071)
library(caTools)

# Dividing the data into a training set and a test set

set.seed(123)
split <- sample.split(fraud.data.prepered$is_fraud, SplitRatio = 0.7)
train_data <- subset(fraud.data.prepered, split == TRUE)
test_data <- subset(fraud.data.prepered, split == FALSE)

# Building the SVM Model 
svm_model <- svm(is_fraud ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1, probability = TRUE)

# Prediction Evaluation

predictions <- predict(svm_model, newdata = test_data)

# Evaluation

confusion_matrix <- table(Predicted = predictions, Actual = test_data$is_fraud)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Precision, Recall, F1 Score

precision <- confusion_matrix["1", "1"] / sum(confusion_matrix["1", ])
recall <- confusion_matrix["1", "1"] / sum(confusion_matrix[, "1"])
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Results

print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# Prediction probabilities for ROC curve

library(pROC)
svm_probabilities <- attr(predict(svm_model, newdata = test_data, probability = TRUE), "probabilities")[, "1"]

# Calculate the ROC curve and AUC

svm_roc <- roc(test_data$is_fraud, svm_probabilities, levels = c("0", "1"), direction = "<")

# Plot the ROC curve

plot(svm_roc, col = "blue", lwd = 2, main = "ROC Curve for SVM Model")
abline(a = 0, b = 1, lty = 2, col = "red")

# Display the AUC

auc_value <- auc(svm_roc)
cat("AUC: ", auc_value, "\n")





# --------- AdaBoost ---------


# --------- AdaBoost ---------

install.packages("adabag")
install.packages("caTools")
library(adabag)
library(caTools)

# Split the Data into Training and Test Sets 
set.seed(123)
split <- sample.split(fraud.data.prepered$is_fraud, SplitRatio = 0.7)
train_data <- subset(fraud.data.prepered, split == TRUE)
test_data <- subset(fraud.data.prepered, split == FALSE)

# Train the AdaBoost Model 
adaboost_model <- boosting(
  is_fraud ~ .,
  data = train_data,
  boos = TRUE,
  mfinal = 50  # Number of boosting iterations
)

# Make Predictions 
predictions <- predict(adaboost_model, newdata = test_data)

# Confusion Matrix 
confusion_matrix <- table(
  Predicted = predictions$class,
  Actual = test_data$is_fraud
)

# Accuracy Calculation 
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Precision, Recall, F1 Score 
precision <- confusion_matrix["1", "1"] / sum(confusion_matrix["1", ])
recall <- confusion_matrix["1", "1"] / sum(confusion_matrix[, "1"])
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Results 
print("Confusion Matrix:")
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# Calculate the ROC curve and AUC
AdaBoost_roc <- roc(test_data$is_fraud, predictions$prob[, 2], levels = c("0", "1"), direction = "<")

# Plot the ROC curve
plot(AdaBoost_roc, col = "blue", lwd = 2, main = "ROC Curve for AdaBoost Model")
abline(a = 0, b = 1, lty = 2, col = "red")

# Display the AUC
auc_value <- auc(AdaBoost_roc)
cat("AUC: ", auc_value, "\n")



# --------- Decision Tree ---------



install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
library(caret)


# Splitting the Data into Training and Test Sets

set.seed(123)
split <- sample.split(fraud.data.prepered$is_fraud, SplitRatio = 0.7)
train_data <- subset(fraud.data.prepered, split == TRUE)
test_data <- subset(fraud.data.prepered, split == FALSE)

# Building the Decision Tree Model

decision_tree_model <- rpart(is_fraud ~ ., data = train_data, method = "class", control = rpart.control(cp = 0.01))

# Visualizing the Decision Tree

rpart.plot(decision_tree_model)

# Making Predictions

predictions <- predict(decision_tree_model, newdata = test_data, type = "class")

# Evaluating the Model

confusion_matrix <- table(Predicted = predictions, Actual = test_data$is_fraud)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Calculating Precision, Recall, and F1 Score

precision <- confusion_matrix["1", "1"] / sum(confusion_matrix["1", ])
recall <- confusion_matrix["1", "1"] / sum(confusion_matrix[, "1"])
f1_score <- 2 * ((precision * recall) / (precision + recall))


# Results

print("Confusion Matrix:")
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))


# Calculate the ROC curve and AUC

Tree_roc <- roc(test_data$is_fraud, predictions, levels = c("0", "1"), direction = "<")

# Plot the ROC curve

plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Decision Tree Model")
abline(a = 0, b = 1, lty = 2, col = "red")

# Display the AUC

auc_value <- auc(roc_curve)
cat("AUC: ", auc_value, "\n")



#-------------------------------------------- Comparison between the models -------------------------------------------------



# a data frame

results <- data.frame(
  Model = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# Logistic Regression Results

logistic_auc <- auc(logistic_roc)
results <- rbind(results, data.frame(
  Model = "Logistic Regression",
  Accuracy = conf_matrix$overall['Accuracy'],
  Precision = conf_matrix$byClass['Pos Pred Value'],
  Recall = conf_matrix$byClass['Sensitivity'],
  F1_Score = conf_matrix$byClass['F1'],
  AUC = logistic_auc
))

# XGBoost Results

xgboost_auc <- auc(XGBoost_roc)
results <- rbind(results, data.frame(
  Model = "XGBoost",
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  AUC = xgboost_auc
))

# SVM Results

svm_auc <- auc(svm_roc)
results <- rbind(results, data.frame(
  Model = "SVM",
  Accuracy = accuracy,  # Use the results from the SVM section
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  AUC = svm_auc
))

# AdaBoost Results

adaboost_auc <- auc(AdaBoost_roc)
results <- rbind(results, data.frame(
  Model = "AdaBoost",
  Accuracy = accuracy,  # Use the results from the AdaBoost section
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  AUC = adaboost_auc
))

# Decision Tree Results

tree_auc <- auc(Tree_roc)
results <- rbind(results, data.frame(
  Model = "Decision Tree",
  Accuracy = accuracy,  # Use the results from the Decision Tree section
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  AUC = tree_auc
))

# Display the comparison table

print(results)




# Visual comparison -  ROC 

library(pROC)
library(ggplot2)

# Plot all ROC curves on the same graph

plot(logistic_roc, col = "blue", lwd = 2, main = "Comparison of ROC Curves for All Models", print.auc = TRUE)
plot(XGBoost_roc, col = "green", lwd = 2, add = TRUE, print.auc = TRUE)
plot(svm_roc, col = "purple", lwd = 2, add = TRUE, print.auc = TRUE)
plot(AdaBoost_roc, col = "orange", lwd = 2, add = TRUE, print.auc = TRUE)
plot(Tree_roc, col = "red", lwd = 2, add = TRUE, print.auc = TRUE)

# Add a diagonal line representing random guessing

abline(a = 0, b = 1, lty = 2, col = "gray")

# Add a legend to identify each model

legend("bottomright", legend = c("Logistic Regression", "XGBoost", "SVM", "AdaBoost", "Decision Tree"),
       col = c("blue", "green", "purple", "orange", "red"), lwd = 2)





#-------------------------------------------- A potential economic model -------------------------------------------------



# Set assumptions and data

average_loss_per_fraud <- 1000  # Average financial loss per fraud case (in dollars)

fraud_detection_rate <- 0.85  # Recall of the model (e.g., 85% of fraud cases are detected)

total_transactions_per_year <- 144447  # Total number of transactions processed per year (According to the data)

estimated_fraud_rate <- 0.02  # Estimated percentage of transactions that are fraudulent (e.g., 2%)

# Calculate the estimated number of fraud cases per year

estimated_fraud_cases_per_year <- total_transactions_per_year * estimated_fraud_rate

# Calculate the number of fraud cases detected by the model

detected_fraud_cases <- estimated_fraud_cases_per_year * fraud_detection_rate

# Calculate the potential financial savings

potential_savings <- detected_fraud_cases * average_loss_per_fraud

# Estimate the cost of running the fraud detection system (optional)

model_operational_cost_per_year <- 20000  # Assumed cost of running the system (e.g., server costs, software, etc.)

# Calculate the net savings

net_savings <- potential_savings - model_operational_cost_per_year

# results

cat("Estimated Number of Fraud Cases per Year:", round(estimated_fraud_cases_per_year), "\n")

cat("Number of Fraud Cases Detected by the Model:", round(detected_fraud_cases), "\n")

cat("Potential Financial Savings:", potential_savings, "dollars\n")

cat("Model Operational Cost per Year:", model_operational_cost_per_year, "dollars\n")

cat("Net Financial Savings:", net_savings, "dollars\n")








#-------------------------------------------- End of project -------------------------------------------------