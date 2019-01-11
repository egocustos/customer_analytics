#Techniques -> NoFullNA, NoZeroVar, DownSampling, Mice Imputation on numericals,
#             Chi-Square (or copies) for categorical vars, Mice inputation on categoricals, 
#             CorrMatrix on numericals, BinaryEncoder, CorrMatrix on dummies, 
#             Modelling Functions (trainControl, expand.grid(), train, predict, roc, auc)


#Importing the dataset
library(readr)
orange_train<-read.csv("ORANGE_TRAIN_DATA.csv")
# orange_test <-read.csv("ORANGE_TEST_DATA.csv")
objectives <- read.csv("KDD_CUP_2009_OBJECTIVE_COLUMNS_LABELS.csv")

# Encoding the target feature as factor (In this case, Churn)
objectives$Churn[objectives$Churn==-1]<-0
Churn <- factor(objectives$Churn, levels = c(0, 1))

library(caret)

Orange_DS<-downSample(orange_train,Churn) #Downsampling Orange_train 



NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1) 
  unlist(want)
}

Orange_DS <- subset(Orange_DS, select = -NoVar(Orange_DS))

library("dplyr")

Orange_numeric_DS <- select_if(Orange_DS, is.numeric) #selects all numeric variables from the downsampling

Orange_numeric_DS_with_NAs <- select_if(Orange_DS, is.numeric)

corrmatrix_with_NAs <-cor(Orange_numeric_DS_with_NAs,use="pairwise.complete.obs")
CorrMatrix_with_NAs <- round(corrmatrix_with_NAs, 2)


#Findcorrelation does not work with NA values. We replace NA values with 0
for(i in 1:ncol(CorrMatrix_with_NAs)){
  CorrMatrix_with_NAs[is.na(CorrMatrix_with_NAs[,i]), i] <- 0
}
highlyCorrelated_with_NAs <- findCorrelation(CorrMatrix_with_NAs, cutoff=0.99, names = F)  #Selecting highly correlated variables for then taking them out from  the dataset
Orange_numeric_DS <- Orange_numeric_DS_with_NAs[,-c(highlyCorrelated_with_NAs)] #Deletes previously selected high-correlated variables


#Replacing NA values for the mean for numeric variables
for(i in 1:ncol(Orange_numeric_DS)){
  Orange_numeric_DS[is.na(Orange_numeric_DS[,i]), i] <- mean(Orange_numeric_DS[,i], na.rm = TRUE)
}

#Deleting No variance variables after replacing NAs with mean
Orange_numeric_DS <- subset(Orange_numeric_DS, select = -NoVar(Orange_numeric_DS))

Orange_Cat_DS <- select_if(Orange_DS[,-ncol(Orange_DS)],is.factor)

for(i in 1:ncol(Orange_Cat_DS)){
  Orange_Cat_DS[is.na(Orange_Cat_DS[,i]), i] <- 'other' 
}

anyNA(Orange_Cat_DS)


#for applying this to a dataframe df call
#df <- df %>% lapply(function(x) categoryToOther(x,50)) %>% data.frame()
# 
# #Starting from scratch, we are using a different set of tools. Only numeric variables.
# Orange_numeric_DS <- subset(Orange_DS, select=names(Orange_numeric_DS))
# 
# library(mice)
# library(parallel)
# library(tictoc)
# 
# #sort(colSums(is.na(Orange_DS[,1:174])> 0), decreasing = T)
# #sort(colSums(is.na(Orange_numeric_DS)> 0), decreasing = T)
# 
# tic('Using parlMICE on numerical features without correlation:')
# 
# imp <- parlmice(Orange_numeric_DS, method = 'rf' ,seed = 31, n.core = 3, n.imp.core = 1) #Error in serialize(data, node$con) : error writing to connection
# 
# Orange_numeric_DS_parl <- complete(imp)  # generate the completed data.
# toc()
# 
# anyNA(Orange_numeric_DS_parl)
# #Only if anyNA is false:
# Orange_numeric_DS <- Orange_numeric_DS_parl
# 
# 
# #Mice on categoricals
# Orange_Cat_DS <- select_if(Orange_DS[,-ncol(Orange_DS)],is.factor)
# 
# tic('Using parlMICE on categorical:')
# 
# imp_c <- parlmice(Orange_Cat_DS, method = 'rf' ,seed = 31, n.core = 3, n.imp.core = 1) #Error in serialize(data, node$con) : error writing to connection
# 
# Orange_Cat_DS_parl <- complete(imp_c)  # generate the completed data.
# Orange_Cat_DS <- Orange_Cat_DS_parl
# 
# toc()

binaryEncoding <- function(col) {
  if(is.factor(col)) {
    col_num <- col %>% as.numeric() %>% intToBits() %>% 
      as.integer() %>% matrix(ncol=length(col)) %>% t() %>% data.frame() 
    ind <- col_num %>% apply(2,function(x) all(x==0)) %>% {which(.==FALSE)} %>% 
      max()
    return(col_num[,1:ind])
  }
  else return(col)
}
#for applying this to a dataframe df call
Orange_Cat_DS <- Orange_Cat_DS %>% lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()

Class <- Orange_DS$Class
Orange_DS_Final <-cbind(Orange_numeric_DS, Orange_Cat_DS, Class)

# Performing the splitting to the rough dataset-------------------------------------------------------------

# Splitting the dataset into the Training set and Test set

library(caTools)
set.seed(31)
split = sample.split(Orange_DS_Final$Class, SplitRatio = 0.75)
training_set = subset(Orange_DS_Final, split == TRUE)
test_set = subset(Orange_DS_Final, split == FALSE)

# Random Forest -----------------------------------------------------------

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(31)
classifier_rf = randomForest(x = training_set[,-ncol(training_set)],
                             y = training_set$Class,
                             ntree = 500)

# OOB estimate of  error rate: 36.2%
# Confusion matrix:
#   0    1 class.error
# 0 1755  999   0.3627451
# 1  995 1759   0.3612927


# Predicting the Test set results
y_pred_rf = predict(classifier_rf, newdata = test_set[,-ncol(training_set)])

# Making the Confusion Matrix
cm_rf = table(test_set[, ncol(training_set)], y_pred_rf)
# y_pred_rf
# 0   1
# 0 572 346
# 1 315 603



# 10 trees
#   y_pred_rf
#    0   1
#  0 562 356
#  1 428 490
# True Negatives: 562/918 (61.22%)
# True Positives: 490/918 (53.38%)
# Accuracy: 1052/1836 (57.3%)

# 100 trees
#   y_pred_rf
#    0   1
#  0 584 334
#  1 366 552
# True Negatives: 584/918 (63.62%)
# True Positives: 552/918 (60.13%)
# Accuracy: 1144/1836 (62.31%)

# 500 trees
#  y_pred_rf
#    0   1
# 0 572 346
# 1 315 603
# True Negatives: 572/918 (62.31%)
# True Positives: 603/918 (65.69%)
# Accuracy: 1175/1836 (64%)

# Using Caret's train function -----------------------------------------------------------

set.seed(321)
fitControl <- trainControl(method = "CV",
                           number = 3,
                           verboseIter = TRUE,
                           classProbs= TRUE,
                           summaryFunction = twoClassSummary)

#Random forest

rf_grid <- expand.grid(mtry=80,splitrule="gini",min.node.size=seq(10,100,10))

rf <- train(x=training_set[,-ncol(training_set)], 
            y=make.names(training_set$Class), 
            method="ranger", metric="ROC",num.trees=500,
            trControl=fitControl,tuneGrid=rf_grid)

# RF
# min.node.size  ROC        Sens       Spec     
# 10            0.7036736  0.6234568  0.6619463
# 20            0.7062411  0.6289034  0.6673929
# 30            0.7081207  0.6299927  0.6659405
# 40            0.7085752  0.6252723  0.6663036
# 50            0.7095613  0.6328976  0.6732026
# 60            0.7095554  0.6307190  0.6688453
# 70            0.7084478  0.6285403  0.6713871
# 80            0.7098754  0.6259985  0.6742919
# 90            0.7099829  0.6220044  0.6793755
# 100            0.7109762  0.6245461  0.6804648



rf_pred <- predict(rf,test_set[,-ncol(test_set)],type="prob")
table(rf_pred$X1>0.5,test_set$Class)

#Calculating the AUC
library(pROC)
roc_obj <- roc(test_set$Class, rf_pred$X1)
#         0   1
# FALSE 583 269
# TRUE  335 649


auc(roc_obj) #Area under the curve: 0.687
#Area under the curve: 0.727
