
#Importing the dataset
library(readr)
orange_train<-read.csv("ORANGE_TRAIN_DATA.csv")
orange_test <-read.csv("ORANGE_TEST_DATA.csv")
objectives <- read.csv("KDD_CUP_2009_OBJECTIVE_COLUMNS_LABELS.csv")

# Encoding the target feature as factor (In this case, Churn)
objectives$Churn[objectives$Churn==-1]<-0
Churn <- factor(objectives$Churn, levels = c(0, 1))

#It is required to omit the columns which have 0 variance
NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}
train <- subset(orange_train, select = -NoVar(orange_train))
#View(train)

#lets quickly check the ‘missings’ pattern using mice::md.pattern.
library(mice)
md.pattern(train)

which(names(train)=="Var190")
train_numeric <- train[,1:174]

for(i in 1:ncol(train_numeric)){
  train_numeric[is.na(train_numeric[,i]), i] <- mean(train_numeric[,i], na.rm = TRUE)
}
View(train_numeric)

miceMod <- mice(train[,175:212], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)
#> FALSE
View(miceOutput)

data <-cbind(train_numeric, miceOutput)

df <- cbind(data, Churn)

NomanyCat <- function(dat) {
  out <- lapply(dat,function(x) nlevels(x))
  want <- which(!out > 53)
  unlist(want)
}
out <- lapply(final,function(x) nlevels(x))
View(out)

final <- subset(df, select = -NomanyCat(df[,175:212]))

library(caTools)
set.seed(123)
split <- sample.split(final$Churn, SplitRatio = 0.75)
training_set <- subset(final, split == TRUE)
test_set <- subset(final, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier_rf = randomForest(x = training_set[,-188],
                             y = training_set$Churn,
                             ntree = 500)
warnings()
# Predicting the Test set results
y_pred_rf = predict(classifier_rf, newdata = test_set[-231])

# Making the Confusion Matrix
cm_rf = table(test_set[, 231], y_pred_rf)