### PERFORMING LOGISTIC REGRESSION

#reading the data

bank_data<-read.csv("bank.csv",sep = ";")


bank_data
#necessary libraries
library(dplyr)
library(caTools)
library(ggplot2)
library(caret)

# Convert the target variable 'y' to a binary factor
bank_data$y <- as.factor(ifelse(bank_data$y == "yes", 1, 0))

# Convert categorical variables to factors
bank_data <- bank_data %>%
  mutate_if(is.character, as.factor)
bank_data
# checking the type of data again
str(bank_data)

set.seed(123)

#splitting the data into training and testing
train_index <- sample(1:nrow(bank_data), 0.8 * nrow(bank_data))
train_data <- bank_data[train_index, ]
test_data <- bank_data[-train_index, ]
#performing logistic regression
logistic_model <- glm(y ~ ., data = train_data, family = "binomial")
#Summary of logistic model
summary(logistic_model)


# Predict the probabilities of the test data
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions based on a cutoff value
cutoff <- 0.5
test_data$predicted <- ifelse(test_data$predicted_prob >= cutoff, 1, 0)

# Confusion matrix
confusion_matrix <- table(test_data$y, test_data$predicted)
confusion_matrix


# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

# Sensitivity (True Positive Rate)
sensitivity <- confusion_matrix[2, 2] / (confusion_matrix[2, 1] + confusion_matrix[2, 2])
sensitivity

# Specificity (True Negative Rate)
specificity <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2])
specificity


##### PERFORMING DISCRIMINATe ANALYSIS

bank_data<-read.csv("bank.csv",sep=";")
bank_data

# Load the bank marketing dataset (assuming it is already loaded)

# Define the predictor variables
predictors <- bank_data[, !(names(bank_data) %in% "y")]

# Define the response variable
response <- bank_data$y

# Perform LDA6
summarize.class<-function(original, classify) {
  class.table<-table(original, classify)
  numb<-rowSums(class.table)
  prop<-round(class.table/numb,4)
  overall<-round(sum(diag(class.table))/sum(class.table),4)
  list(class.table = class.table, prop = prop, overall.correct = overall)
}


library(MASS)

#Performing linear discriminate analysis
DA4 <- lda(formula = y ~ ., data = bank_data, CV = TRUE)
head(DA4$posterior)

# Summarize the classification results
summarize.class(original = bank_data$y, classify = DA5$class)

#performing quadratic discriminate analysis
DA5 <- qda(formula = y ~ ., data = bank_data, CV = TRUE)
head(DA5$posterior)

# Summarize the classification results
summarize.class(original = bank_data$y, classify = DA5$class)


# visualizations
library(ggplot2)   # for data visualization
library(dplyr)     # for data manipulation

bank_data<-read.csv("bank.csv",sep = ";")
bank_data
ggplot(bank_data, aes(x = job)) +
  geom_bar() +
  xlab("Job") +
  ylab("Count") +
  ggtitle("Distribution of Jobs")

ggplot(bank_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Distribution of Age")
 
ggplot(bank_data, aes(x = age, y = balance)) +
  geom_point() +
  xlab("Age") +
  ylab("Balance") +
  ggtitle("Balance vs. Age")

ggplot(bank_data, aes(x = job, y = balance)) +
  geom_boxplot() +
  xlab("Job") +
  ylab("Balance") +
  ggtitle("Balance Distribution by Job")




