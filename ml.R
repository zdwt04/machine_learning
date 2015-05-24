## Predicting Manner for Exercise Activity

# Based upon data collected by a group of exercise enthusiasts, the manner in which they preform an exercise activity will be predicted.  Each of 6 participants collected data based on belt, arm, forearm, and dumbbell accelerometers during particular barbell exercises. This data will constitute the training data for the predictive models.
# 
# The training data for this analysis is here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# 
# The test data are available here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

library(caret)

# Load the training dataset
traindf <- read.csv("train/pml-training.csv", header = TRUE, na.strings=c("NA", ""), stringsAsFactors = TRUE)

# Tidy the training data by removing columns with mostly NAs
mostly_good <- apply(!is.na(traindf),2,sum)>=nrow(traindf)
training <- traindf[, mostly_good]

# Remove junk columns from training data
training <- training[, -grep("X|time|window|user_name", names(training))]

# Load the test dataset
testdf <- read.csv("test/pml-testing.csv", header = TRUE, na.strings=c("NA", ""), stringsAsFactors = TRUE)

# Tidy the testing data by removing columns with mostly NAs
mostly_good <- apply(!is.na(testdf),2,sum)>=nrow(testdf)
testing <- testdf[, mostly_good]

# Remove junk columns from testing data
testing <- testing[, -grep("X|time|window|user_name", names(testing))]

# Subset the training data to create additional training and testing data sets
set.seed(9876)
splitIndex <- createDataPartition(y=training$classe, p = .75, list = FALSE, times = 1)
cv_training <- training[splitIndex,]
cv_testing <- training[-splitIndex,]

# Use Random Forest as the training method to create the prediction model on %75 subset of the training data
mf <- train(classe~.,data=cv_training, method="rf")
plot(varImp(mf))

#pred <- predict(mf, newdata=cv_testing)
pred <- predict(mf, newdata=testdf)
print(mf)
print(mf$finalModel)

## Model the training data using the Random Forest modeling method
#ptm <- proc.time()
#mf <- train(classe~.,data=training, method="rf")

## Predict the values using the test data
#pred <- predict(mf, newdata=testing)
#pred
#proc.time() - ptm

predictions <- predict(mf, newdata = cv_testing)
outofsample.error <- 1 - (sum(predictions == cv_testing$classe) / length(predictions))


