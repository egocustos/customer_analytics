#Importing the dataset
library(readr)
data<-read.csv("bank.csv")
data <- subset(data, select = -duration )
#For measuring time
library(tictoc)


#### Data Preprocessing ####

#Preprocessing with customized functions
library(magrittr)
library(dplyr)

time_frame=matrix(nrow=200,ncol=8)

#NULL Remover
time_frame[1,1] <- "Short"
time_frame[1,2] <- "NullRemover:0.9"
time_frame[1,3] <-   system.time(
  #tic("NullRemover:0.9")
  NoNullData0.9 <- NULLremover(data,0.9) )[3]
time_frame[1,4] <- nrow(data)
time_frame[1,5] <- ncol(data)
time_frame[1,6] <- nrow(NoNullData0.9)
time_frame[1,7] <- ncol(NoNullData0.9)
time_frame[1,8] <- "No changes in dimmensionality"
  

#NULL Remover 0.7
time_frame[2,1] <- "Short"
time_frame[2,2] <- "NullRemover:0.7"
time_frame[2,3] <-   system.time(
  #tic("NullRemover:0.5")
  NoNullData0.7 <- NULLremover(data,0.7) )[3]
time_frame[2,4] <- nrow(data)
time_frame[2,5] <- ncol(data)
time_frame[2,6] <- nrow(NoNullData0.7)
time_frame[2,7] <- ncol(NoNullData0.7)
time_frame[2,8] <- "No changes in dimmensionality"

#NULL Remover 0.5
time_frame[3,1] <- "Short"
time_frame[3,2] <- "NullRemover:0.5"
time_frame[3,3] <-   system.time(
  #tic("NullRemover:0.5")
  NoNullData0.5 <- NULLremover(data,0.5) )[3]
time_frame[3,4] <- nrow(data)
time_frame[3,5] <- ncol(data)
time_frame[3,6] <- nrow(NoNullData0.5)
time_frame[3,7] <- ncol(NoNullData0.5)
time_frame[3,8] <- "No changes in dimmensionality"

#NO SD Remover
time_frame[4,1] <- "Short"
time_frame[4,2] <- "NoSDRemover"
time_frame[4,3] <-   system.time(
  No0VarData <- SDremover(data,0)  )[3]
time_frame[4,4] <- nrow(data)
time_frame[4,5] <- ncol(data)
time_frame[4,6] <- nrow(No0VarData)
time_frame[4,7] <- ncol(No0VarData)
time_frame[4,8] <- "No changes in dimmensionality"

#Category To Other
#CategoryToOther1000
time_frame[5,1] <- "Short"
time_frame[5,2] <- "CategoryToOther(1000)"
time_frame[5,3] <-   system.time(
  CatToOther_1000 <-lapply(data,function(x) categoryToOther(x,1000)) %>% data.frame()  )[3]
time_frame[5,4] <- nrow(data)
time_frame[5,5] <- ncol(data)
time_frame[5,6] <- nrow(CatToOther_1000)
time_frame[5,7] <- ncol(CatToOther_1000)
#levels_raw <-colSums(as.data.frame(sapply(data[,sapply(data, is.factor)], nlevels)))
#levels_CatToOther_1000 <-colSums(as.data.frame(sapply(CatToOther_1000[,sapply(CatToOther_1000, is.factor)], nlevels)))
time_frame[5,8] <- "Dim changes nlevels total from 46 to 22"

#CategoryToOther500
time_frame[6,1] <- "Short"
time_frame[6,2] <- "CategoryToOther(500)"
time_frame[6,3] <-   system.time(
  CatToOther_500 <-lapply(data,function(x) categoryToOther(x,500)) %>% data.frame()  )[3]
time_frame[6,4] <- nrow(data)
time_frame[6,5] <- ncol(data)
time_frame[6,6] <- nrow(CatToOther_500)
time_frame[6,7] <- ncol(CatToOther_500)
#levels_raw <-colSums(as.data.frame(sapply(data[,sapply(data, is.factor)], nlevels)))
#levels_CatToOther_500 <-colSums(as.data.frame(sapply(CatToOther_500[,sapply(CatToOther_500, is.factor)], nlevels)))
time_frame[6,8] <- "Dim changes nlevels total from 46 to 29"

#CategoryToOther100
time_frame[7,1] <- "Short"
time_frame[7,2] <- "CategoryToOther(100)"
time_frame[7,3] <-   system.time(
  CatToOther_100 <-lapply(data,function(x) categoryToOther(x,100)) %>% data.frame()  )[3]
time_frame[7,4] <- nrow(data)
time_frame[7,5] <- ncol(data)
time_frame[7,6] <- nrow(CatToOther_100)
time_frame[7,7] <- ncol(CatToOther_100)
#levels_raw <-colSums(as.data.frame(sapply(data[,sapply(data, is.factor)], nlevels)))
#levels_CatToOther_100 <-colSums(as.data.frame(sapply(CatToOther_100[,sapply(CatToOther_100, is.factor)], nlevels)))
time_frame[7,8] <- "Dim changes nlevels total from 46 to 42"

#CategoryToOther50
time_frame[8,1] <- "Short"
time_frame[8,2] <- "CategoryToOther(50)"
time_frame[8,3] <-   system.time(
  CatToOther_50 <-lapply(data,function(x) categoryToOther(x,50)) %>% data.frame()  )[3]
time_frame[8,4] <- nrow(data)
time_frame[8,5] <- ncol(data)
time_frame[8,6] <- nrow(CatToOther_50)
time_frame[8,7] <- ncol(CatToOther_50)
#levels_raw <-colSums(as.data.frame(sapply(data[,sapply(data, is.factor)], nlevels)))
#levels_CatToOther_50 <-colSums(as.data.frame(sapply(CatToOther_50[,sapply(CatToOther_50, is.factor)], nlevels)))
time_frame[8,8] <- "Dim changes nlevels total from 46 to 45"

#CategoryToOther10
time_frame[9,1] <- "Short"
time_frame[9,2] <- "CategoryToOther(10)"
time_frame[9,3] <-   system.time(
  CatToOther_10 <-lapply(data,function(x) categoryToOther(x,10)) %>% data.frame()  )[3]
time_frame[9,4] <- nrow(data)
time_frame[9,5] <- ncol(data)
time_frame[9,6] <- nrow(CatToOther_10)
time_frame[9,7] <- ncol(CatToOther_10)
#levels_raw <-colSums(as.data.frame(sapply(data[,sapply(data, is.factor)], nlevels)))
#levels_CatToOther_10 <-colSums(as.data.frame(sapply(CatToOther_10[,sapply(CatToOther_10, is.factor)], nlevels)))
time_frame[9,8] <- "No changes in dimmensionality"


#Binary Encoding
time_frame[10,1] <- "Short"
time_frame[10,2] <- "BinaryEncoder"
time_frame[10,3] <-   system.time(
  BinaryEncodingData <-lapply(data,function(x) x <- binaryEncoding(x)) %>% data.frame())[3]
time_frame[10,4] <- nrow(data)
time_frame[10,5] <- ncol(data)
time_frame[10,6] <- nrow(BinaryEncodingData)
time_frame[10,7] <- ncol(BinaryEncodingData)
time_frame[10,8] <- "Number of columns increase from 16 to 32"

#OneHotEncoder
library(fastDummies)
library("dplyr")
time_frame[11,1] <- "Short"
time_frame[11,2] <- "OneHotEncoder"
time_frame[11,3] <-   
  system.time(
  OneHotEncodingData <- OneHotEncoder(data) ) [3] +
  system.time(
  OneHotEncodingData <- select_if(OneHotEncodingData, is.numeric))[3]
time_frame[11,4] <- nrow(data)
time_frame[11,5] <- ncol(data)
time_frame[11,6] <- nrow(OneHotEncodingData)
time_frame[11,7] <- ncol(OneHotEncodingData)
time_frame[11,8] <- "Number of columns increase from 16 to 32"

#Basic Imputation
time_frame[12,1] <- "Short"
time_frame[12,2] <- "BasicImputation"
time_frame[12,3] <-   
  system.time(
    BasicImputedData <- data %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .))))
  )[3]
time_frame[12,4] <- nrow(data)
time_frame[12,5] <- ncol(data)
time_frame[12,6] <- nrow(BasicImputedData)
time_frame[12,7] <- ncol(BasicImputedData)
#anyNA(data)
#Study the histogram:
#hist(BasicImputedData$age)
#hist(data$age)
time_frame[12,8] <- "No Nas in Data"

#MICE Imputation
time_frame[13,1] <- "Short"
time_frame[13,2] <- "MICE_Imputation"
time_frame[13,3] <-   
  system.time(
    MICE_Imp <-parlmice(data, method = 'rf' ,seed = 123, n.core = 3, n.imp.core = 1)) [3] +
  system.time(  
  MiceImputedData<- complete(MICE_Imp) )[3]
time_frame[13,4] <- nrow(data)
time_frame[13,5] <- ncol(data)
time_frame[13,6] <- nrow(MiceImputedData)
time_frame[13,7] <- ncol(MiceImputedData)
#anyNA(data)
#Study the histogram:
#hist(BasicImputedData$age)
#hist(data$age)
time_frame[13,8] <- "No Nas in Data: That generates an Error in MICE"


#Sampling
#DownSampling
library(caret)
time_frame[14,1] <- "Short"
time_frame[14,2] <- "DownSampling"
time_frame[14,3] <-   system.time(
  data_DS <- downSample(x = data[ , names(data) != "y"], y = data$y))[3]
time_frame[14,4] <- nrow(data)
time_frame[14,5] <- ncol(data)
time_frame[14,6] <- nrow(data_DS)
time_frame[14,7] <- ncol(data_DS)
time_frame[14,8] <- "Number of rows decrease from 4521 to 1042"

#Sampling
#UpSampling
library(caret)
time_frame[15,1] <- "Short"
time_frame[15,2] <- "UpSampling"
time_frame[15,3] <-   system.time(
  data_US <- upSample(x = data[ , names(data) != "y"], y = data$y))[3]
time_frame[15,4] <- nrow(data)
time_frame[15,5] <- ncol(data)
time_frame[15,6] <- nrow(data_US)
time_frame[15,7] <- ncol(data_US)
time_frame[15,8] <- "Number of rows increase from 4521 to 8000"


#Correlation Matrix
#Using One  Hot Encoding Data
library(corrplot)
time_frame[16,1] <- "Short"
time_frame[16,2] <- "Correlation Matrix: One Hot Encoding"
time_frame[16,3] <-   
  system.time(
  corrmatrix_OH <-cor(OneHotEncodingData,use="pairwise.complete.obs")) [3] +
  system.time(
  CorrMatrix_OH <- round(corrmatrix_OH, 2))[3] +
  system.time(
  highly_correlated_OH <- findCorrelation(CorrMatrix_OH, cutoff=0.9, names = TRUE))[3] +
  system.time(
  NoCorrelated_OH_Data <- OneHotEncodingData[,!(colnames(OneHotEncodingData) %in% highly_correlated_OH)])[3]
time_frame[16,4] <- nrow(OneHotEncodingData)
time_frame[16,5] <- ncol(OneHotEncodingData)
time_frame[16,6] <- nrow(NoCorrelated_OH_Data)
time_frame[16,7] <- ncol(NoCorrelated_OH_Data)
corrplot(CorrMatrix_OH, order = "hclust", tl.cex = 0.5)
time_frame[16,8] <- "Number of columns decrease from 52 to 48"

#Correlation Matrix
#Using Binary Encoding Data
library(caret)
library(mlbench)
time_frame[17,1] <- "Short"
time_frame[17,2] <- "Feature Importance"
time_frame[17,3] <-   
  system.time(
    control <- trainControl(method="repeatedcv", number=10, repeats=3)) [3] +
  system.time(
    model <- train(y~., data=data, method="lvq", preProcess="scale", trControl=control))[3] +
  system.time(
    importance <- varImp(model, scale=FALSE))[3]
time_frame[17,4] <- nrow(data)
time_frame[17,5] <- ncol(data)
time_frame[17,6] <- nrow(data)
time_frame[17,7] <- ncol(data)
# summarize importance
print(importance)
# plot importance
plot(importance)
time_frame[17,8] <- "This require additional study: Shows a 0.1 Imp diff between first and last"

#### Machine Learning Models ####

#Control for ML algorithms
##########################
fitControl <- trainControl(method = "CV", #Cross-validation
                           number = 3, #3-fold
                           verboseIter = TRUE, #Output while running
                           classProbs= TRUE, #needed for ROC
                           summaryFunction = twoClassSummary ) #needed for ROC

#Removing the response variable: y_no
OneHotEncodingData <- subset(OneHotEncodingData, select = -c(y_no))
#Removing the response variable: y.X2
BinaryEncodingData <- subset(BinaryEncodingData, select = -c(y.X2))



#### MODELLING DS OH (Short Data) ####

data_DS_OH <- downSample(x = OneHotEncodingData[ , names(OneHotEncodingData) != "y_yes"], y = as.factor(OneHotEncodingData$y_yes))

###Splitting the data###

set.seed(31)
ind_DS_OH <- sample(1:nrow(data_DS_OH),0.8*(nrow(data_DS_OH)))
train_DS_OH <- data_DS_OH[ind_DS_OH,-ncol(data_DS_OH)]
test_DS_OH <- data_DS_OH[-ind_DS_OH,-ncol(data_DS_OH)]

#train_DS_BI$Class <- factor(train_DS_BI$Class, levels = c(0, 1))
train_labels_DS_OH <- data_DS_OH$Class[ind_DS_OH]
test_labels_DS_OH <- data_DS_OH$Class[-ind_DS_OH]

time_frame[18,1] <- "Short"
time_frame[18,2] <- "Model RF on DS OH"
time_frame[18,3] <-   system.time({
 

#Train RF DS OH
rf_grid_DS_OH <- expand.grid(mtry=ceiling(ncol(train_DS_OH)/2),splitrule="gini",min.node.size=100)
set.seed(1)
rf_DS_OH <- train(x=train_DS_OH, 
               y=make.names(train_labels_DS_OH), 
               method="ranger", metric="ROC",num.trees=500,
               trControl=fitControl,tuneGrid=rf_grid_DS_OH,importance = 'impurity')
rf_pred_DS_OH <- predict(rf_DS_OH,test_DS_OH,type="prob")
cm_rf_DS_OH <-table(rf_pred_DS_OH$X1>0.5,test_labels_DS_OH)
library(pROC)
roc_rf_DS_OH <- roc(test_labels_DS_OH, rf_pred_DS_OH$X1)
auc_rf_DS_OH <-auc(roc_rf_DS_OH) 
})[3]

time_frame[18,4] <- nrow(OneHotEncodingData)
time_frame[18,5] <- ncol(OneHotEncodingData)
time_frame[18,6] <- nrow(OneHotEncodingData)
time_frame[18,7] <- ncol(OneHotEncodingData)
time_frame[18,8] <- "AUC: 0.755"


#GBM DS OH
time_frame[19,1] <- "Short"
time_frame[19,2] <- "Model GBM on DS OH"
time_frame[19,3] <-   system.time({

gbm_grid_DS_OH <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                           n.minobsinnode=10)
set.seed(2)
gbm_DS_OH <- train(x=train_DS_OH, 
                y=make.names(train_labels_DS_OH), 
                method="gbm", metric="ROC",
                trControl=fitControl,tuneGrid=gbm_grid_DS_OH)
gbm_pred_DS_OH <- predict(gbm_DS_OH,test_DS_OH,type="prob")
cm_gbm_DS_OH <- table(gbm_pred_DS_OH$X1>0.5,test_labels_DS_OH)
roc_gbm_DS_OH <- roc(test_labels_DS_OH, gbm_pred_DS_OH$X1)
auc_gbm_DS_OH <- auc(roc_gbm_DS_OH)

})[3]
time_frame[19,4] <- nrow(OneHotEncodingData)
time_frame[19,5] <- ncol(OneHotEncodingData)
time_frame[19,6] <- nrow(OneHotEncodingData)
time_frame[19,7] <- ncol(OneHotEncodingData)
time_frame[19,8] <- "AUC: 0.769"

#SVM DS OH

time_frame[20,1] <- "Short"
time_frame[20,2] <- "Model SVM on DS OH"
time_frame[20,3] <-   system.time({
  
svm_grid_DS_OH <- expand.grid(C=1,sigma=0.001)
set.seed(3)
svm_DS_OH <- train(x=train_DS_OH, 
                y=make.names(train_labels_DS_OH),
                method = "svmRadial",
                metric="ROC",
                preProcess = c("center","scale"), #necessary for svm
                trControl=fitControl,
                tuneGrid=svm_grid_DS_OH)
svm_pred_DS_OH <- predict(svm_DS_OH,test_DS_OH,type="prob")
cm_svm_DS_OH <- table(svm_pred_DS_OH$X1>0.5,test_labels_DS_OH)
roc_svm_DS_OH <- roc(test_labels_DS_OH, svm_pred_DS_OH$X1)
auc_svm_DS_OH <- auc(roc_svm_DS_OH)

})[3]
time_frame[20,4] <- nrow(OneHotEncodingData)
time_frame[20,5] <- ncol(OneHotEncodingData)
time_frame[20,6] <- nrow(OneHotEncodingData)
time_frame[20,7] <- ncol(OneHotEncodingData)
time_frame[20,8] <- "AUC: 0.727"


#NN DS OH

time_frame[21,1] <- "Short"
time_frame[21,2] <- "Model NN on DS OH"
time_frame[21,3] <-   system.time({

nn_grid_DS_OH <- data.frame(size=6)
set.seed(4)
nn_DS_OH <- train(x=train_DS_OH, 
               y=make.names(train_labels_DS_OH), 
               method="mlp", metric="ROC",
               trControl=fitControl,tuneGrid=nn_grid_DS_OH,
               preProcess = c("center","scale"))
nn_pred_DS_OH <- predict(nn_DS_OH,test_DS_OH,type="prob")
cm_nn_DS_OH <- table(nn_pred_DS_OH$X1>0.5,test_labels_DS_OH)
roc_nn_DS_OH <- roc(test_labels_DS_OH, nn_pred_DS_OH$X1)
auc_nn_DS_OH <- auc(roc_nn_DS_OH)

})[3]
time_frame[21,4] <- nrow(OneHotEncodingData)
time_frame[21,5] <- ncol(OneHotEncodingData)
time_frame[21,6] <- nrow(OneHotEncodingData)
time_frame[21,7] <- ncol(OneHotEncodingData)
time_frame[21,8] <- "AUC: 0.675"

#LR BI

time_frame[22,1] <- "Short"
time_frame[22,2] <- "Model LR on DS OH"
time_frame[22,3] <-   system.time({

set.seed(5)
lr_DS_OH <- train(x=train_DS_OH, 
               y=make.names(train_labels_DS_OH), 
               method="glm", metric="ROC",
               trControl=fitControl#,tuneGrid=lr_grid
               #preProcess = c("center","scale")
)
lr_pred_DS_OH <- predict(lr_DS_OH,test_DS_OH,type="prob")
cm_lr_DS_OH <- table(lr_pred_DS_OH$X1>0.5,test_labels_DS_OH)
roc_lr_DS_OH <- roc(test_labels_DS_OH, lr_pred_DS_OH$X1)
auc_lr_DS_OH <- auc(roc_lr_DS_OH)

})[3]
time_frame[22,4] <- nrow(OneHotEncodingData)
time_frame[22,5] <- ncol(OneHotEncodingData)
time_frame[22,6] <- nrow(OneHotEncodingData)
time_frame[22,7] <- ncol(OneHotEncodingData)
time_frame[22,8] <- "AUC: 0.766"

#### MODELLING DS BE (Short Data) ####

data_DS_BE <- downSample(x = BinaryEncodingData[ , names(BinaryEncodingData) != "y.X1"], y = as.factor(BinaryEncodingData$y.X1))

###Splitting the data###

set.seed(31)
ind_DS_BE <- sample(1:nrow(data_DS_BE),0.8*(nrow(data_DS_BE)))
train_DS_BE <- data_DS_BE[ind_DS_BE,-ncol(data_DS_BE)]
test_DS_BE <- data_DS_BE[-ind_DS_BE,-ncol(data_DS_BE)]

#train_DS_BI$Class <- factor(train_DS_BI$Class, levels = c(0, 1))
train_labels_DS_BE <- data_DS_BE$Class[ind_DS_BE]
test_labels_DS_BE <- data_DS_BE$Class[-ind_DS_BE]


time_frame[23,1] <- "Short"
time_frame[23,2] <- "Model RF on DS BE"
time_frame[23,3] <-   system.time({
  
  
  #Train RF DS BE
  rf_grid_DS_BE <- expand.grid(mtry=ceiling(ncol(train_DS_BE)/2),splitrule="gini",min.node.size=100)
  set.seed(1)
  rf_DS_BE <- train(x=train_DS_BE, 
                    y=make.names(train_labels_DS_BE), 
                    method="ranger", metric="ROC",num.trees=500,
                    trControl=fitControl,tuneGrid=rf_grid_DS_BE,importance = 'impurity')
  rf_pred_DS_BE <- predict(rf_DS_BE,test_DS_BE,type="prob")
  cm_rf_DS_BE <-table(rf_pred_DS_BE$X1>0.5,test_labels_DS_BE)
  library(pROC)
  roc_rf_DS_BE <- roc(test_labels_DS_BE, rf_pred_DS_BE$X1)
  auc_rf_DS_BE <-auc(roc_rf_DS_BE) 
})[3]

time_frame[23,4] <- nrow(BinaryEncodingData)
time_frame[23,5] <- ncol(BinaryEncodingData)
time_frame[23,6] <- nrow(BinaryEncodingData)
time_frame[23,7] <- ncol(BinaryEncodingData)
time_frame[23,8] <- "AUC: 0.7378"


#GBM DS BE
time_frame[24,1] <- "Short"
time_frame[24,2] <- "Model GBM on DS BE"
time_frame[24,3] <-   system.time({
  
  gbm_grid_DS_BE <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                                n.minobsinnode=10)
  set.seed(2)
  gbm_DS_BE <- train(x=train_DS_BE, 
                     y=make.names(train_labels_DS_BE), 
                     method="gbm", metric="ROC",
                     trControl=fitControl,tuneGrid=gbm_grid_DS_BE)
  gbm_pred_DS_BE <- predict(gbm_DS_BE,test_DS_BE,type="prob")
  cm_gbm_DS_BE <- table(gbm_pred_DS_BE$X1>0.5,test_labels_DS_BE)
  roc_gbm_DS_BE <- roc(test_labels_DS_BE, gbm_pred_DS_BE$X1)
  auc_gbm_DS_BE <- auc(roc_gbm_DS_BE)
  
})[3]
time_frame[24,4] <- nrow(BinaryEncodingData)
time_frame[24,5] <- ncol(BinaryEncodingData)
time_frame[24,6] <- nrow(BinaryEncodingData)
time_frame[24,7] <- ncol(BinaryEncodingData)
time_frame[24,8] <- "AUC: 0.743"

#SVM DS BE

time_frame[25,1] <- "Short"
time_frame[25,2] <- "Model SVM on DS BE"
time_frame[25,3] <-   system.time({
  
  svm_grid_DS_BE <- expand.grid(C=1,sigma=0.001)
  set.seed(3)
  svm_DS_BE <- train(x=train_DS_BE, 
                     y=make.names(train_labels_DS_BE),
                     method = "svmRadial",
                     metric="ROC",
                     preProcess = c("center","scale"), #necessary for svm
                     trControl=fitControl,
                     tuneGrid=svm_grid_DS_BE)
  svm_pred_DS_BE <- predict(svm_DS_BE,test_DS_BE,type="prob")
  cm_svm_DS_BE <- table(svm_pred_DS_BE$X1>0.5,test_labels_DS_BE)
  roc_svm_DS_BE <- roc(test_labels_DS_BE, svm_pred_DS_BE$X1)
  auc_svm_DS_BE <- auc(roc_svm_DS_BE)
  
})[3]
time_frame[25,4] <- nrow(BinaryEncodingData)
time_frame[25,5] <- ncol(BinaryEncodingData)
time_frame[25,6] <- nrow(BinaryEncodingData)
time_frame[25,7] <- ncol(BinaryEncodingData)
time_frame[25,8] <- "AUC: 0.723"


#NN DS BE

time_frame[26,1] <- "Short"
time_frame[26,2] <- "Model NN on DS BE"
time_frame[26,3] <-   system.time({
  
  nn_grid_DS_BE <- data.frame(size=6)
  set.seed(4)
  nn_DS_BE <- train(x=train_DS_BE, 
                    y=make.names(train_labels_DS_BE), 
                    method="mlp", metric="ROC",
                    trControl=fitControl,tuneGrid=nn_grid_DS_BE,
                    preProcess = c("center","scale"))
  nn_pred_DS_BE <- predict(nn_DS_BE,test_DS_BE,type="prob")
  cm_nn_DS_BE <- table(nn_pred_DS_BE$X1>0.5,test_labels_DS_BE)
  roc_nn_DS_BE <- roc(test_labels_DS_BE, nn_pred_DS_BE$X1)
  auc_nn_DS_BE <- auc(roc_nn_DS_BE)
  
})[3]
time_frame[26,4] <- nrow(BinaryEncodingData)
time_frame[26,5] <- ncol(BinaryEncodingData)
time_frame[26,6] <- nrow(BinaryEncodingData)
time_frame[26,7] <- ncol(BinaryEncodingData)
time_frame[26,8] <- "AUC: 0.7159"

#LR DS BE

time_frame[27,1] <- "Short"
time_frame[27,2] <- "Model LR on DS BE"
time_frame[27,3] <-   system.time({
  
  set.seed(5)
  lr_DS_BE <- train(x=train_DS_BE, 
                    y=make.names(train_labels_DS_BE), 
                    method="glm", metric="ROC",
                    trControl=fitControl#,tuneGrid=lr_grid
                    #preProcess = c("center","scale")
  )
  lr_pred_DS_BE <- predict(lr_DS_BE,test_DS_BE,type="prob")
  cm_lr_DS_BE <- table(lr_pred_DS_BE$X1>0.5,test_labels_DS_BE)
  roc_lr_DS_BE <- roc(test_labels_DS_BE, lr_pred_DS_BE$X1)
  auc_lr_DS_BE <- auc(roc_lr_DS_BE)
  
})[3]
time_frame[27,4] <- nrow(BinaryEncodingData)
time_frame[27,5] <- ncol(BinaryEncodingData)
time_frame[27,6] <- nrow(BinaryEncodingData)
time_frame[27,7] <- ncol(BinaryEncodingData)
time_frame[27,8] <- "AUC: 0.7399"


#### MODELLING US OH (Short Data) ####

data_US_OH <- upSample(x = OneHotEncodingData[ , names(OneHotEncodingData) != "y_yes"], y = as.factor(OneHotEncodingData$y_yes))

###Splitting the data###

set.seed(31)
ind_US_OH <- sample(1:nrow(data_US_OH),0.8*(nrow(data_US_OH)))
train_US_OH <- data_US_OH[ind_US_OH,-ncol(data_US_OH)]
test_US_OH <- data_US_OH[-ind_US_OH,-ncol(data_US_OH)]

#train_US_BI$Class <- factor(train_US_BI$Class, levels = c(0, 1))
train_labels_US_OH <- data_US_OH$Class[ind_US_OH]
test_labels_US_OH <- data_US_OH$Class[-ind_US_OH]

time_frame[28,1] <- "Short"
time_frame[28,2] <- "Model RF on US OH"
time_frame[28,3] <-   system.time({
  
  
  #Train RF US OH
  rf_grid_US_OH <- expand.grid(mtry=ceiling(ncol(train_US_OH)/2),splitrule="gini",min.node.size=100)
  set.seed(1)
  rf_US_OH <- train(x=train_US_OH, 
                    y=make.names(train_labels_US_OH), 
                    method="ranger", metric="ROC",num.trees=500,
                    trControl=fitControl,tuneGrid=rf_grid_US_OH,importance = 'impurity')
  rf_pred_US_OH <- predict(rf_US_OH,test_US_OH,type="prob")
  cm_rf_US_OH <-table(rf_pred_US_OH$X1>0.5,test_labels_US_OH)
  library(pROC)
  roc_rf_US_OH <- roc(test_labels_US_OH, rf_pred_US_OH$X1)
  auc_rf_US_OH <-auc(roc_rf_US_OH) 
})[3]

time_frame[28,4] <- nrow(OneHotEncodingData)
time_frame[28,5] <- ncol(OneHotEncodingData)
time_frame[28,6] <- nrow(OneHotEncodingData)
time_frame[28,7] <- ncol(OneHotEncodingData)
time_frame[28,8] <- "AUC: 0.944"


#GBM US OH
time_frame[29,1] <- "Short"
time_frame[29,2] <- "Model GBM on US OH"
time_frame[29,3] <-   system.time({
  
  gbm_grid_US_OH <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                                n.minobsinnode=10)
  set.seed(2)
  gbm_US_OH <- train(x=train_US_OH, 
                     y=make.names(train_labels_US_OH), 
                     method="gbm", metric="ROC",
                     trControl=fitControl,tuneGrid=gbm_grid_US_OH)
  gbm_pred_US_OH <- predict(gbm_US_OH,test_US_OH,type="prob")
  cm_gbm_US_OH <- table(gbm_pred_US_OH$X1>0.5,test_labels_US_OH)
  roc_gbm_US_OH <- roc(test_labels_US_OH, gbm_pred_US_OH$X1)
  auc_gbm_US_OH <- auc(roc_gbm_US_OH)
  
})[3]
time_frame[29,4] <- nrow(OneHotEncodingData)
time_frame[29,5] <- ncol(OneHotEncodingData)
time_frame[29,6] <- nrow(OneHotEncodingData)
time_frame[29,7] <- ncol(OneHotEncodingData)
time_frame[29,8] <- "AUC: 0.8363"

#SVM US OH

time_frame[30,1] <- "Short"
time_frame[30,2] <- "Model SVM on US OH"
time_frame[30,3] <-   system.time({
  
  svm_grid_US_OH <- expand.grid(C=1,sigma=0.001)
  set.seed(3)
  svm_US_OH <- train(x=train_US_OH, 
                     y=make.names(train_labels_US_OH),
                     method = "svmRadial",
                     metric="ROC",
                     preProcess = c("center","scale"), #necessary for svm
                     trControl=fitControl,
                     tuneGrid=svm_grid_US_OH)
  svm_pred_US_OH <- predict(svm_US_OH,test_US_OH,type="prob")
  cm_svm_US_OH <- table(svm_pred_US_OH$X1>0.5,test_labels_US_OH)
  roc_svm_US_OH <- roc(test_labels_US_OH, svm_pred_US_OH$X1)
  auc_svm_US_OH <- auc(roc_svm_US_OH)
  
})[3]
time_frame[30,4] <- nrow(OneHotEncodingData)
time_frame[30,5] <- ncol(OneHotEncodingData)
time_frame[30,6] <- nrow(OneHotEncodingData)
time_frame[30,7] <- ncol(OneHotEncodingData)
time_frame[30,8] <- "AUC: 0.7495"


#NN US OH

time_frame[31,1] <- "Short"
time_frame[31,2] <- "Model NN on US OH"
time_frame[31,3] <-   system.time({
  
  nn_grid_US_OH <- data.frame(size=6)
  set.seed(4)
  nn_US_OH <- train(x=train_US_OH, 
                    y=make.names(train_labels_US_OH), 
                    method="mlp", metric="ROC",
                    trControl=fitControl,tuneGrid=nn_grid_US_OH,
                    preProcess = c("center","scale"))
  nn_pred_US_OH <- predict(nn_US_OH,test_US_OH,type="prob")
  cm_nn_US_OH <- table(nn_pred_US_OH$X1>0.5,test_labels_US_OH)
  roc_nn_US_OH <- roc(test_labels_US_OH, nn_pred_US_OH$X1)
  auc_nn_US_OH <- auc(roc_nn_US_OH)
  
})[3]
time_frame[31,4] <- nrow(OneHotEncodingData)
time_frame[31,5] <- ncol(OneHotEncodingData)
time_frame[31,6] <- nrow(OneHotEncodingData)
time_frame[31,7] <- ncol(OneHotEncodingData)
time_frame[31,8] <- "AUC: 0.8387"

#LR BI

time_frame[32,1] <- "Short"
time_frame[32,2] <- "Model LR on US OH"
time_frame[32,3] <-   system.time({
  
  set.seed(5)
  lr_US_OH <- train(x=train_US_OH, 
                    y=make.names(train_labels_US_OH), 
                    method="glm", metric="ROC",
                    trControl=fitControl#,tuneGrid=lr_grid
                    #preProcess = c("center","scale")
  )
  lr_pred_US_OH <- predict(lr_US_OH,test_US_OH,type="prob")
  cm_lr_US_OH <- table(lr_pred_US_OH$X1>0.5,test_labels_US_OH)
  roc_lr_US_OH <- roc(test_labels_US_OH, lr_pred_US_OH$X1)
  auc_lr_US_OH <- auc(roc_lr_US_OH)
  
})[3]
time_frame[32,4] <- nrow(OneHotEncodingData)
time_frame[32,5] <- ncol(OneHotEncodingData)
time_frame[32,6] <- nrow(OneHotEncodingData)
time_frame[32,7] <- ncol(OneHotEncodingData)
time_frame[32,8] <- "AUC: 0.7472"



#### MODELLING US BE (Short Data) ####
data_US_BE <- upSample(x = BinaryEncodingData[ , names(BinaryEncodingData) != "y.X1"], y = as.factor(BinaryEncodingData$y.X1))

###Splitting the data###

set.seed(31)
ind_US_BE <- sample(1:nrow(data_US_BE),0.8*(nrow(data_US_BE)))
train_US_BE <- data_US_BE[ind_US_BE,-ncol(data_US_BE)]
test_US_BE <- data_US_BE[-ind_US_BE,-ncol(data_US_BE)]

#train_US_BI$Class <- factor(train_US_BI$Class, levels = c(0, 1))
train_labels_US_BE <- data_US_BE$Class[ind_US_BE]
test_labels_US_BE <- data_US_BE$Class[-ind_US_BE]


time_frame[33,1] <- "Short"
time_frame[33,2] <- "Model RF on US BE"
time_frame[33,3] <-   system.time({
  
  
  #Train RF US BE
  rf_grid_US_BE <- expand.grid(mtry=ceiling(ncol(train_US_BE)/2),splitrule="gini",min.node.size=100)
  set.seed(1)
  rf_US_BE <- train(x=train_US_BE, 
                    y=make.names(train_labels_US_BE), 
                    method="ranger", metric="ROC",num.trees=500,
                    trControl=fitControl,tuneGrid=rf_grid_US_BE,importance = 'impurity')
  rf_pred_US_BE <- predict(rf_US_BE,test_US_BE,type="prob")
  cm_rf_US_BE <-table(rf_pred_US_BE$X1>0.5,test_labels_US_BE)
  library(pROC)
  roc_rf_US_BE <- roc(test_labels_US_BE, rf_pred_US_BE$X1)
  auc_rf_US_BE <-auc(roc_rf_US_BE) 
})[3]

time_frame[33,4] <- nrow(BinaryEncodingData)
time_frame[33,5] <- ncol(BinaryEncodingData)
time_frame[33,6] <- nrow(BinaryEncodingData)
time_frame[33,7] <- ncol(BinaryEncodingData)
time_frame[33,8] <- "AUC: 0.9386"


#GBM US BE
time_frame[34,1] <- "Short"
time_frame[34,2] <- "Model GBM on US BE"
time_frame[34,3] <-   system.time({
  
  gbm_grid_US_BE <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                                n.minobsinnode=10)
  set.seed(2)
  gbm_US_BE <- train(x=train_US_BE, 
                     y=make.names(train_labels_US_BE), 
                     method="gbm", metric="ROC",
                     trControl=fitControl,tuneGrid=gbm_grid_US_BE)
  gbm_pred_US_BE <- predict(gbm_US_BE,test_US_BE,type="prob")
  cm_gbm_US_BE <- table(gbm_pred_US_BE$X1>0.5,test_labels_US_BE)
  roc_gbm_US_BE <- roc(test_labels_US_BE, gbm_pred_US_BE$X1)
  auc_gbm_US_BE <- auc(roc_gbm_US_BE)
  
})[3]
time_frame[34,4] <- nrow(BinaryEncodingData)
time_frame[34,5] <- ncol(BinaryEncodingData)
time_frame[34,6] <- nrow(BinaryEncodingData)
time_frame[34,7] <- ncol(BinaryEncodingData)
time_frame[34,8] <- "AUC: 0.8338"

#SVM US BE

time_frame[35,1] <- "Short"
time_frame[35,2] <- "Model SVM on US BE"
time_frame[35,3] <-   system.time({
  
  svm_grid_US_BE <- expand.grid(C=1,sigma=0.001)
  set.seed(3)
  svm_US_BE <- train(x=train_US_BE, 
                     y=make.names(train_labels_US_BE),
                     method = "svmRadial",
                     metric="ROC",
                     preProcess = c("center","scale"), #necessary for svm
                     trControl=fitControl,
                     tuneGrid=svm_grid_US_BE)
  svm_pred_US_BE <- predict(svm_US_BE,test_US_BE,type="prob")
  cm_svm_US_BE <- table(svm_pred_US_BE$X1>0.5,test_labels_US_BE)
  roc_svm_US_BE <- roc(test_labels_US_BE, svm_pred_US_BE$X1)
  auc_svm_US_BE <- auc(roc_svm_US_BE)
  
})[3]
time_frame[35,4] <- nrow(BinaryEncodingData)
time_frame[35,5] <- ncol(BinaryEncodingData)
time_frame[35,6] <- nrow(BinaryEncodingData)
time_frame[35,7] <- ncol(BinaryEncodingData)
time_frame[35,8] <- "AUC: 0.7435"


#NN US BE

time_frame[36,1] <- "Short"
time_frame[36,2] <- "Model NN on US BE"
time_frame[36,3] <-   system.time({
  
  nn_grid_US_BE <- data.frame(size=6)
  set.seed(4)
  nn_US_BE <- train(x=train_US_BE, 
                    y=make.names(train_labels_US_BE), 
                    method="mlp", metric="ROC",
                    trControl=fitControl,tuneGrid=nn_grid_US_BE,
                    preProcess = c("center","scale"))
  nn_pred_US_BE <- predict(nn_US_BE,test_US_BE,type="prob")
  cm_nn_US_BE <- table(nn_pred_US_BE$X1>0.5,test_labels_US_BE)
  roc_nn_US_BE <- roc(test_labels_US_BE, nn_pred_US_BE$X1)
  auc_nn_US_BE <- auc(roc_nn_US_BE)
  
})[3]
time_frame[36,4] <- nrow(BinaryEncodingData)
time_frame[36,5] <- ncol(BinaryEncodingData)
time_frame[36,6] <- nrow(BinaryEncodingData)
time_frame[36,7] <- ncol(BinaryEncodingData)
time_frame[36,8] <- "AUC: 0.8102"

#LR US BE

time_frame[37,1] <- "Short"
time_frame[37,2] <- "Model LR on US BE"
time_frame[37,3] <-   system.time({
  
  set.seed(5)
  lr_US_BE <- train(x=train_US_BE, 
                    y=make.names(train_labels_US_BE), 
                    method="glm", metric="ROC",
                    trControl=fitControl#,tuneGrid=lr_grid
                    #preProcess = c("center","scale")
  )
  lr_pred_US_BE <- predict(lr_US_BE,test_US_BE,type="prob")
  cm_lr_US_BE <- table(lr_pred_US_BE$X1>0.5,test_labels_US_BE)
  roc_lr_US_BE <- roc(test_labels_US_BE, lr_pred_US_BE$X1)
  auc_lr_US_BE <- auc(roc_lr_US_BE)
  
})[3]
time_frame[37,4] <- nrow(BinaryEncodingData)
time_frame[37,5] <- ncol(BinaryEncodingData)
time_frame[37,6] <- nrow(BinaryEncodingData)
time_frame[37,7] <- ncol(BinaryEncodingData)
time_frame[37,8] <- "AUC: 0.7353"


#### MODELLING OR OH (Short Data) ####

data_OR_OH <- OneHotEncodingData 

###Splitting the data###

set.seed(31)
ind_OR_OH <- sample(1:nrow(data_OR_OH),0.8*(nrow(data_OR_OH)))
train_OR_OH <- data_OR_OH[ind_OR_OH,-ncol(data_OR_OH)]
test_OR_OH <- data_OR_OH[-ind_OR_OH,-ncol(data_OR_OH)]

#train_OR_BI$Class <- factor(train_OR_BI$Class, levels = c(0, 1))
train_labels_OR_OH <- data_OR_OH$y_yes[ind_OR_OH]
test_labels_OR_OH <- data_OR_OH$y_yes[-ind_OR_OH]


time_frame[38,1] <- "Short"
time_frame[38,2] <- "Model RF on OR OH"
time_frame[38,3] <-   system.time({
  
  
  #Train RF OR OH
  rf_grid_OR_OH <- expand.grid(mtry=ceiling(ncol(train_OR_OH)/2),splitrule="gini",min.node.size=100)
  set.seed(1)
  rf_OR_OH <- train(x=train_OR_OH, 
                    y=make.names(train_labels_OR_OH), 
                    method="ranger", metric="ROC",num.trees=500,
                    trControl=fitControl,tuneGrid=rf_grid_OR_OH,importance = 'impurity')
  rf_pred_OR_OH <- predict(rf_OR_OH,test_OR_OH,type="prob")
  cm_rf_OR_OH <-table(rf_pred_OR_OH$X1>0.5,test_labels_OR_OH)
  library(pROC)
  roc_rf_OR_OH <- roc(test_labels_OR_OH, rf_pred_OR_OH$X1)
  auc_rf_OR_OH <-auc(roc_rf_OR_OH) 
})[3]

time_frame[38,4] <- nrow(OneHotEncodingData)
time_frame[38,5] <- ncol(OneHotEncodingData)
time_frame[38,6] <- nrow(OneHotEncodingData)
time_frame[38,7] <- ncol(OneHotEncodingData)
time_frame[38,8] <- "AUC: 0.798"


#GBM OR OH
time_frame[39,1] <- "Short"
time_frame[39,2] <- "Model GBM on OR OH"
time_frame[39,3] <-   system.time({
  
  gbm_grid_OR_OH <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                                n.minobsinnode=10)
  set.seed(2)
  gbm_OR_OH <- train(x=train_OR_OH, 
                     y=make.names(train_labels_OR_OH), 
                     method="gbm", metric="ROC",
                     trControl=fitControl,tuneGrid=gbm_grid_OR_OH)
  gbm_pred_OR_OH <- predict(gbm_OR_OH,test_OR_OH,type="prob")
  cm_gbm_OR_OH <- table(gbm_pred_OR_OH$X1>0.5,test_labels_OR_OH)
  roc_gbm_OR_OH <- roc(test_labels_OR_OH, gbm_pred_OR_OH$X1)
  auc_gbm_OR_OH <- auc(roc_gbm_OR_OH)
  
})[3]
time_frame[39,4] <- nrow(OneHotEncodingData)
time_frame[39,5] <- ncol(OneHotEncodingData)
time_frame[39,6] <- nrow(OneHotEncodingData)
time_frame[39,7] <- ncol(OneHotEncodingData)
time_frame[39,8] <- "AUC: 0.7882"

#SVM OR OH

time_frame[40,1] <- "Short"
time_frame[40,2] <- "Model SVM on OR OH"
time_frame[40,3] <-   system.time({
  
  svm_grid_OR_OH <- expand.grid(C=1,sigma=0.001)
  set.seed(3)
  svm_OR_OH <- train(x=train_OR_OH, 
                     y=make.names(train_labels_OR_OH),
                     method = "svmRadial",
                     metric="ROC",
                     preProcess = c("center","scale"), #necessary for svm
                     trControl=fitControl,
                     tuneGrid=svm_grid_OR_OH)
  svm_pred_OR_OH <- predict(svm_OR_OH,test_OR_OH,type="prob")
  cm_svm_OR_OH <- table(svm_pred_OR_OH$X1>0.5,test_labels_OR_OH)
  roc_svm_OR_OH <- roc(test_labels_OR_OH, svm_pred_OR_OH$X1)
  auc_svm_OR_OH <- auc(roc_svm_OR_OH)
  
})[3]
time_frame[40,4] <- nrow(OneHotEncodingData)
time_frame[40,5] <- ncol(OneHotEncodingData)
time_frame[40,6] <- nrow(OneHotEncodingData)
time_frame[40,7] <- ncol(OneHotEncodingData)
time_frame[40,8] <- "AUC: 0.7182"


#NN OR OH

time_frame[41,1] <- "Short"
time_frame[41,2] <- "Model NN on OR OH"
time_frame[41,3] <-   system.time({
  
  nn_grid_OR_OH <- data.frame(size=6)
  set.seed(4)
  nn_OR_OH <- train(x=train_OR_OH, 
                    y=make.names(train_labels_OR_OH), 
                    method="mlp", metric="ROC",
                    trControl=fitControl,tuneGrid=nn_grid_OR_OH,
                    preProcess = c("center","scale"))
  nn_pred_OR_OH <- predict(nn_OR_OH,test_OR_OH,type="prob")
  cm_nn_OR_OH <- table(nn_pred_OR_OH$X1>0.5,test_labels_OR_OH)
  roc_nn_OR_OH <- roc(test_labels_OR_OH, nn_pred_OR_OH$X1)
  auc_nn_OR_OH <- auc(roc_nn_OR_OH)
  
})[3]
time_frame[41,4] <- nrow(OneHotEncodingData)
time_frame[41,5] <- ncol(OneHotEncodingData)
time_frame[41,6] <- nrow(OneHotEncodingData)
time_frame[41,7] <- ncol(OneHotEncodingData)
time_frame[41,8] <- "AUC: 0.7483"

#LR BI

time_frame[42,1] <- "Short"
time_frame[42,2] <- "Model LR on OR OH"
time_frame[42,3] <-   system.time({
  
  set.seed(5)
  lr_OR_OH <- train(x=train_OR_OH, 
                    y=make.names(train_labels_OR_OH), 
                    method="glm", metric="ROC",
                    trControl=fitControl#,tuneGrid=lr_grid
                    #preProcess = c("center","scale")
  )
  lr_pred_OR_OH <- predict(lr_OR_OH,test_OR_OH,type="prob")
  cm_lr_OR_OH <- table(lr_pred_OR_OH$X1>0.5,test_labels_OR_OH)
  roc_lr_OR_OH <- roc(test_labels_OR_OH, lr_pred_OR_OH$X1)
  auc_lr_OR_OH <- auc(roc_lr_OR_OH)
  
})[3]
time_frame[42,4] <- nrow(OneHotEncodingData)
time_frame[42,5] <- ncol(OneHotEncodingData)
time_frame[42,6] <- nrow(OneHotEncodingData)
time_frame[42,7] <- ncol(OneHotEncodingData)
time_frame[42,8] <- "AUC: 0.7553"




#### MODELLING OR BE (Short Data)

data_OR_BE <- BinaryEncodingData

###Splitting the data###

set.seed(31)
ind_OR_BE <- sample(1:nrow(data_OR_BE),0.8*(nrow(data_OR_BE)))
train_OR_BE <- data_OR_BE[ind_OR_BE,-ncol(data_OR_BE)]
test_OR_BE <- data_OR_BE[-ind_OR_BE,-ncol(data_OR_BE)]

#train_OR_BI$Class <- factor(train_OR_BI$Class, levels = c(0, 1))
train_labels_OR_BE <- data_OR_BE$y.X1[ind_OR_BE]
test_labels_OR_BE <- data_OR_BE$y.X1[-ind_OR_BE]


time_frame[43,1] <- "Short"
time_frame[43,2] <- "Model RF on OR BE"
time_frame[43,3] <-   system.time({
  
  
  #Train RF OR BE
  rf_grid_OR_BE <- expand.grid(mtry=ceiling(ncol(train_OR_BE)/2),splitrule="gini",min.node.size=100)
  set.seed(1)
  rf_OR_BE <- train(x=train_OR_BE, 
                    y=make.names(train_labels_OR_BE), 
                    method="ranger", metric="ROC",num.trees=500,
                    trControl=fitControl,tuneGrid=rf_grid_OR_BE,importance = 'impurity')
  rf_pred_OR_BE <- predict(rf_OR_BE,test_OR_BE,type="prob")
  cm_rf_OR_BE <-table(rf_pred_OR_BE$X1>0.5,test_labels_OR_BE)
  library(pROC)
  roc_rf_OR_BE <- roc(test_labels_OR_BE, rf_pred_OR_BE$X1)
  auc_rf_OR_BE <-auc(roc_rf_OR_BE) 
})[3]

time_frame[43,4] <- nrow(BinaryEncodingData)
time_frame[43,5] <- ncol(BinaryEncodingData)
time_frame[43,6] <- nrow(BinaryEncodingData)
time_frame[43,7] <- ncol(BinaryEncodingData)
time_frame[43,8] <- "AUC: 0.778"


#GBM OR BE
time_frame[44,1] <- "Short"
time_frame[44,2] <- "Model GBM on OR BE"
time_frame[44,3] <-   system.time({
  
  gbm_grid_OR_BE <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                                n.minobsinnode=10)
  set.seed(2)
  gbm_OR_BE <- train(x=train_OR_BE, 
                     y=make.names(train_labels_OR_BE), 
                     method="gbm", metric="ROC",
                     trControl=fitControl,tuneGrid=gbm_grid_OR_BE)
  gbm_pred_OR_BE <- predict(gbm_OR_BE,test_OR_BE,type="prob")
  cm_gbm_OR_BE <- table(gbm_pred_OR_BE$X1>0.5,test_labels_OR_BE)
  roc_gbm_OR_BE <- roc(test_labels_OR_BE, gbm_pred_OR_BE$X1)
  auc_gbm_OR_BE <- auc(roc_gbm_OR_BE)
  
})[3]
time_frame[44,4] <- nrow(BinaryEncodingData)
time_frame[44,5] <- ncol(BinaryEncodingData)
time_frame[44,6] <- nrow(BinaryEncodingData)
time_frame[44,7] <- ncol(BinaryEncodingData)
time_frame[44,8] <- "AUC: 0.7579"

#SVM OR BE

time_frame[45,1] <- "Short"
time_frame[45,2] <- "Model SVM on OR BE"
time_frame[45,3] <-   system.time({
  
  svm_grid_OR_BE <- expand.grid(C=1,sigma=0.001)
  set.seed(3)
  svm_OR_BE <- train(x=train_OR_BE, 
                     y=make.names(train_labels_OR_BE),
                     method = "svmRadial",
                     metric="ROC",
                     preProcess = c("center","scale"), #necessary for svm
                     trControl=fitControl,
                     tuneGrid=svm_grid_OR_BE)
  svm_pred_OR_BE <- predict(svm_OR_BE,test_OR_BE,type="prob")
  cm_svm_OR_BE <- table(svm_pred_OR_BE$X1>0.5,test_labels_OR_BE)
  roc_svm_OR_BE <- roc(test_labels_OR_BE, svm_pred_OR_BE$X1)
  auc_svm_OR_BE <- auc(roc_svm_OR_BE)
  
})[3]
time_frame[45,4] <- nrow(BinaryEncodingData)
time_frame[45,5] <- ncol(BinaryEncodingData)
time_frame[45,6] <- nrow(BinaryEncodingData)
time_frame[45,7] <- ncol(BinaryEncodingData)
time_frame[45,8] <- "AUC: 0.6714"


#NN OR BE

time_frame[46,1] <- "Short"
time_frame[46,2] <- "Model NN on OR BE"
time_frame[46,3] <-   system.time({
  
  nn_grid_OR_BE <- data.frame(size=6)
  set.seed(4)
  nn_OR_BE <- train(x=train_OR_BE, 
                    y=make.names(train_labels_OR_BE), 
                    method="mlp", metric="ROC",
                    trControl=fitControl,tuneGrid=nn_grid_OR_BE,
                    preProcess = c("center","scale"))
  nn_pred_OR_BE <- predict(nn_OR_BE,test_OR_BE,type="prob")
  cm_nn_OR_BE <- table(nn_pred_OR_BE$X1>0.5,test_labels_OR_BE)
  roc_nn_OR_BE <- roc(test_labels_OR_BE, nn_pred_OR_BE$X1)
  auc_nn_OR_BE <- auc(roc_nn_OR_BE)
  
})[3]
time_frame[46,4] <- nrow(BinaryEncodingData)
time_frame[46,5] <- ncol(BinaryEncodingData)
time_frame[46,6] <- nrow(BinaryEncodingData)
time_frame[46,7] <- ncol(BinaryEncodingData)
time_frame[46,8] <- "AUC: 0.7038"

#LR OR BE

time_frame[47,1] <- "Short"
time_frame[47,2] <- "Model LR on OR BE"
time_frame[47,3] <-   system.time({
  
  set.seed(5)
  lr_OR_BE <- train(x=train_OR_BE, 
                    y=make.names(train_labels_OR_BE), 
                    method="glm", metric="ROC",
                    trControl=fitControl#,tuneGrid=lr_grid
                    #preProcess = c("center","scale")
  )
  lr_pred_OR_BE <- predict(lr_OR_BE,test_OR_BE,type="prob")
  cm_lr_OR_BE <- table(lr_pred_OR_BE$X1>0.5,test_labels_OR_BE)
  roc_lr_OR_BE <- roc(test_labels_OR_BE, lr_pred_OR_BE$X1)
  auc_lr_OR_BE <- auc(roc_lr_OR_BE)
  
})[3]
time_frame[47,4] <- nrow(BinaryEncodingData)
time_frame[47,5] <- ncol(BinaryEncodingData)
time_frame[47,6] <- nrow(BinaryEncodingData)
time_frame[47,7] <- ncol(BinaryEncodingData)
time_frame[47,8] <- "AUC: 0.7306"
