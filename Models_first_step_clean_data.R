library(caTools)
set.seed(123)
split <- sample.split(clean_train$Churn, SplitRatio = 0.75)
clean_training_set <- subset(clean_train, split == TRUE)
clean_test_set <- subset(clean_train, split == FALSE)


# Naive Bayes -------------------------------------------------------------


# Fitting NB to the Training set
#install.packages('e1071')
library(e1071)
clean_classifier_nb = naiveBayes(x = clean_training_set[-212],
                           y = clean_training_set$Churn)

# Predicting the Test set results
y_pred_nb_clean = predict(clean_classifier_nb, newdata = clean_test_set[-212])

# Making the Confusion Matrix
cm_nb <- table(clean_test_set[, 212], y_pred_nb_clean)
#Results: 
# True Negatives Raw data: 2304/11582 (19.89%)
# True Positives Raw data: 822/918 (89.54%)

# True Negatives clean data: 1144/11582 (9.88%)
# True Positives clean data: 822/918 (93.9%)

# Random Forest -----------------------------------------------------------

#It is required to omit the columns which have 0 variance
NoManyCat <- function(dat) {
  out <- sapply(dat, function(x) nlevels(x))
  want <- which(!out <= 53)
  unlist(want)
}

train_categoric_rf <- subset(train_categoric, select = -NoManyCat(train_categoric))
train_rf <- cbind(train_numeric, train_categoric_rf, Churn)

library(caTools)
set.seed(123)
split <- sample.split(train_rf$Churn, SplitRatio = 0.75)
training_set_rf <- subset(train_rf, split == TRUE)
test_set_rf <- subset(train_rf, split == FALSE)


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier_rf = randomForest(x = training_set_rf[-187],
                             y = training_set_rf$Churn,
                             ntree = 100)

classif_rf<-train(Churn~., data = training_set_rf, method = "ranger", importance = TRUE)

a<-sapply(train_rf, function(x) nlevels(x))

# Predicting the Test set results
y_pred_rf = predict(classifier_rf, newdata = test_set_rf[,-187])

# Making the Confusion Matrix
cm_rf = table(test_set_rf[, 187], y_pred_rf)
# True Negatives clean data: 11,306/12,216 (92.55%)
# True Positives clean data: 8/284 (2.82%)

library(plyr)
count(y_pred_rf)


library(caret) 
confusionMatrix(cm_rf)
