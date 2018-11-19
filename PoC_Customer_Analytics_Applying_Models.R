
#Importing the dataset
library(readr)
orange_train<-read.csv("ORANGE_TRAIN_DATA.csv")
orange_test <-read.csv("ORANGE_TEST_DATA.csv")
objectives <- read.csv("KDD_CUP_2009_OBJECTIVE_COLUMNS_LABELS.csv")

# Encoding the target feature as factor (In this case, Churn)
objectives$Churn[objectives$Churn==-1]<-0
Churn <- factor(objectives$Churn, levels = c(0, 1))

#JC: Logistic, Naive Bayes, Decision Tree, Random Forest.
#Cristian: K-NN, SVM, Kernel SVM

# Logistic Regression -----------------------------------------------------

#It is required to omit the columns which have 0 variance
NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

train_log <- subset(orange_train, select = -NoVar(orange_train))

#which(names(train)=="Var190")
#ncol(train[1:160])
dataset_log <- cbind(train_log[1:160],Churn)

#Replacing missing values with the respective mean of each colum
library(zoo)
df <- dataset_log
df[] <- lapply(df, na.aggregate)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(321)
split_log = sample.split(df$Churn, SplitRatio = 0.75)
training_set_log = subset(df, split_log == TRUE)
test_set_log = subset(df, split == FALSE)


# Fitting Logistic Regression to the Training set

classifier_log = glm(formula = Churn ~ .,
                 family = binomial,
                 data = training_set_log)

# Predicting the Test set results
prob_pred_log = predict(classifier_log, type = 'response', newdata = test_set_log[-161])
y_pred_log = ifelse(prob_pred_log > 0.5, 1, 0)


# Making the Confusion Matrix
cm_log = table(test_set[, 161], y_pred_log > 0.5)
#Results: 
# True Negatives: 11573/11582 (99.92%)
# True Positives: 1/918 (0.11%)    


# Performing the splitting to the rough dataset-------------------------------------------------------------

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
train <- cbind(orange_train, Churn)

library(caTools)
set.seed(123)
split = sample.split(train$Churn, SplitRatio = 0.75)
training_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)


# Naive Bayes -------------------------------------------------------------


# Fitting NB to the Training set
#install.packages('e1071')
library(e1071)
classifier_nb = naiveBayes(x = training_set[-231],
                        y = training_set$Churn)

# Predicting the Test set results
y_pred_nb = predict(classifier_nb, newdata = test_set[-231])

# Making the Confusion Matrix
cm_nb = table(test_set[, 231], y_pred_nb)
#Results: 
# True Negatives: 2304/11582 (19.89%)
# True Positives: 822/918 (89.54%)


# Decision Tree -----------------------------------------------------------

# Fitting Decision Tree Classification to the Training set
#install.packages('rpart')
library(rpart)
classifier_dt = rpart(formula = Churn ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred_dt = predict(classifier_dt, newdata = test_set[-231], type = 'class')

# Making the Confusion Matrix
cm_dt = table(test_set[, 231], y_pred_dt)
#Results: 
# True Negatives: 11164/11582 (96.39%)
# True Positives: 51/918 (5.56%)


# Random Forest -----------------------------------------------------------

#RF does not allow NA's in predictors
training_set[!complete.cases(training_set),]

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier_rf = randomForest(x = training_set[-231],
                          y = training_set$Churn,
                          ntree = 500)

# Predicting the Test set results
y_pred_rf = predict(classifier_rf, newdata = test_set[-231])

# Making the Confusion Matrix
cm_rf = table(test_set[, 231], y_pred_rf)
