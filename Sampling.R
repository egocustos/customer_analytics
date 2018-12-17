library(caret)

Orange_DS<-downSample(orange_train,Churn)

Orange_US<-upSample(orange_train,Churn)

NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}
train_DS <- subset(Orange_DS, select = -NoVar(Orange_DS))
train_numeric_DS <- train_DS[,1:174]

library(doParallel)
library(tictoc)
library(mice)

tic("MICE on Numerical Data:")

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

#cl <- makePSOCKcluster(4)
#registerDoParallel(cl)


#md.pattern(train_numeric)

miceMod_num <- mice(train_numeric_DS, method="rf")  # perform mice imputation, based on random forests.
train_numeric_DS <- complete(miceMod_num)  # generate the completed data.
anyNA(train_numeric_DS)


stopCluster(cl)
toc()

colnames(train_numeric_DS)[colSums(is.na(train_numeric_DS)) > 0]

library(dplyr)
distinct(train_numeric_DS$Var2)

summary(train_numeric_DS$Var2)
summary(Orange_DS$Var2)

out_DS <- lapply(Orange_DS, function(x) length(unique(x)))
View(out_DS)
unique(Orange_DS$Var21)

library(parallel)

#imp <- parlmice(data = train_numeric_DS, n.core = 7, n.imp.core = 2)

tic('Using parlMICE:')

imp <- parlMICE(train_numeric_DS, method ='rf', seed = 31)

train_numeric_DS_parl <- complete(imp)  # generate the completed data.
anyNA(train_numeric_DS_parl)
var(train_numeric_DS_parl$Var66)
toc()

colnames(train_numeric_DS_parl)[colSums(is.na(train_numeric_DS_parl)) > 0]

library(dplyr)
distinct(train_numeric_DS$Var2)

summary(train_numeric_DS$Var71)
summary(Orange_DS$Var2)

out_DS <- lapply(Orange_DS, function(x) length(unique(x)))
View(out_DS)
unique(Orange_DS$Var21)


# Performing the splitting to the rough dataset-------------------------------------------------------------

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
#train_DS <- cbind(DS, Churn)

library(caTools)
set.seed(31)
split = sample.split(DS$Class, SplitRatio = 0.75)
training_set = subset(DS, split == TRUE)
test_set = subset(DS, split == FALSE)


# Naive Bayes -------------------------------------------------------------


# Fitting NB to the Training set
#install.packages('e1071')
library(e1071)
classifier_nb = naiveBayes(x = training_set[-231],
                           y = training_set$Class)

# Predicting the Test set results
y_pred_nb = predict(classifier_nb, newdata = test_set[-231])

# Making the Confusion Matrix
cm_nb = table(test_set[, 231], y_pred_nb)
#Results: 
# True Negatives: 614/918 (66.88%)
# True Positives: 444/918 (48.37%)
