library(dplyr)
library(caret)
library(pROC)

# Functions ---------------------------------------------------------------

NULLremover <- function(data,perc) {
  a <- lapply(data, function(x) sum(is.na(x))/length(x)) %>% unlist()
  ind <- which(a<perc)
  return(data[,ind])
}


SDremover <- function(data,perc) {
  a <- lapply(data, function(x) sd(as.numeric(x),na.rm=T)/
                (max(as.numeric(x),na.rm=T)-min(as.numeric(x),na.rm = T))) %>%
    unlist()
  ind <- which(a>perc)
  return(data[,ind])
}

#Function taking a column and replacing all categories with a Frequency less than
#thres with the category "Other" - in case of numeric variables no change.
categoryToOther <- function(col,thres) {
  check <- 0
  out <- col
  if(is.factor(col)) {
    t <- table(col)
    r <- which(t<thres)
    for(i in seq(1,max(1,length(r)),100)) {
      if(length(r)-i>=100) {
        out <- gsub(paste(names(r)[i:(i+99)],collapse="|"),"other",out) %>% factor()
      }
      else {
        out <- gsub(paste(names(r)[i:length(r)],collapse="|"),"other",out) %>% factor()
      }
    }
    check <- 1
  }
  cat(check) #just to see how far the process got
  return(out)
}
#for applying this to a dataframe df call
#df <- df %>% lapply(function(x) categoryToOther(x,50)) %>% data.frame()

#
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

#####Program

# Load data ---------------------------------------------------------------

#setwd("E:/Users/Richard.Vogg/Documents/Customer Analytics/PoC_ML")

#data <- read.table("data/orange_small_train.data.csv",sep=",",header=T)
#labels <- read.table("data/labels.csv",sep=",",header=T)
data_orig <- data
data[data==""] <- NA


# Preprocessing with functions from Data review.R ----------------------------
data2 <- data %>%
  select_if(function(x) is.numeric(x) | nlevels(x)<5100) %>% #removes Var217,Var214,Var202,Var200
  select(-Var198,-Var220) %>% #as they are identical to Var222
  NULLremover(1) %>%
  SDremover(0) %>%  data.frame()#until here is the standard cleaning
  





# Sampling ----------------------------------------------------------------
##########

train_upsample <- downSample(x = data2, y = Churn)

train_upsample <- train_upsample %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .)))) %>%
  lapply(function(x) categoryToOther(x,100)) %>% data.frame() #%>%
#lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()

library(fastDummies)
train_cat_DS <- select_if(train_upsample[,-ncol(train_upsample)], is.factor)
dummy_cat_DS <- dummy_cols(train_cat_DS, select_columns = NULL, remove_first_dummy = FALSE,
                           remove_most_frequent_dummy = FALSE)

dummy_cat_DS_num <- dummy_cat_DS[,29:210]

train_num_DS <-select_if(train_upsample, is.numeric)
Class <- train_upsample$Class
train_upsample <-cbind(train_num_DS,dummy_cat_DS_num,Class)

# Test Train Split --------------------------------------------------------
#################

set.seed(31)
ind <- sample(1:nrow(train_upsample),0.8*(nrow(train_upsample)))
train <- train_upsample[ind,]
test <- train_upsample[-ind,]

train_upsample$Class <- factor(train_upsample$Class, levels = c(0, 1))
train_labels <- train_upsample$Class[ind]
test_labels <- train_upsample$Class[-ind]


#Control for ML algorithms
##########################

fitControl <- trainControl(method = "CV", #Cross-validation
                           number = 3, #3-fold
                           verboseIter = TRUE, #Output while running
                           classProbs= TRUE, #needed for ROC
                           summaryFunction = twoClassSummary ) #needed for ROC

##############
# Churn prediction --------------------------------------------------------
##############


#Random forest
##############

rf_grid <- expand.grid(mtry=90,splitrule="gini",min.node.size=100)
set.seed(1)
rf <- train(x=train_upsample[,-ncol(train_upsample)],
            y=make.names(train_upsample$Class),
            method="ranger", metric="ROC",num.trees=500,
            trControl=fitControl,tuneGrid=rf_grid,importance = 'impurity')
rf_pred <- predict(rf,test,type="prob")
table(rf_pred$X1>0.5,test_labels)


#Gradient boosting trees
########################

gbm_grid <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01,
                        n.minobsinnode=10)
set.seed(1)
gbm <- train(x=train_upsample[,-ncol(train_upsample)],
             y=make.names(train_upsample$Class),
             method="gbm", metric="ROC",
             trControl=fitControl,tuneGrid=gbm_grid)
gbm_pred <- predict(gbm,test,type="prob")
#table(gbm_pred$X1>0.5,test_labels$Churn)


#Support vector machine
#######################

svm_grid <- expand.grid(C=1,sigma=c(0.001,0.002))
set.seed(1)
svm <- train(x=train_upsample[,-ncol(train_upsample)],
             y=make.names(train_upsample$Class),
             method = "svmRadial",
             metric="ROC",
             preProcess = c("center","scale"), #necessary for svm
             trControl=fitControl,
             tuneGrid=svm_grid)
svm_pred <- predict(svm,test,type="prob")
#table(svm_pred$X1>0.5,test_labels$Churn)


#Neural Network
################

nn_grid <- data.frame(size=5:10)
set.seed(1)
nn <- train(x=train_upsample[,-ncol(train_upsample)],
            y=make.names(train_upsample$Class),
            method="mlp", metric="ROC",
            trControl=fitControl,tuneGrid=nn_grid,
            preProcess = c("center","scale")
)
nn_pred <- predict(nn,test,type="prob")
#table(nn_pred,test_labels$Churn)


#Logistic Regression
####################

set.seed(1)
lr <- train(x=train_upsample[,-ncol(train_upsample)],
            y=make.names(train_upsample$Class),
            method="glm", metric="ROC",
            trControl=fitControl#,tuneGrid=lr_grid
            #preProcess = c("center","scale")
)
lr_pred <- predict(lr,test,type="prob")
#table(lr_pred$X1>0.5,test_labels$Churn)


#Calculating the AUC

roc_rf <- roc(test_labels, rf_pred$X1)
auc(roc_rf)

roc_gbm <- roc(test_labels, gbm_pred$X1)
auc(roc_gbm)

roc_nn <- roc(test_labels, nn_pred$X1)
auc(roc_nn)

roc_svm <- roc(test_labels, svm_pred$X1)
auc(roc_svm)

roc_lr <- roc(test_labels, lr_pred$X1)
auc(roc_lr)

plot(roc_rf)
lines(roc_gbm,col="red")
lines(roc_nn,col="blue")
lines(roc_svm,col="green")
lines(roc_lr,col="orange")

library(pROC)
library(ggplot2)

roc_rf_BE
roc_gbm_BE
roc_svm_BE
roc_nn_BE
roc_lr_BE

roc_rf_OH
roc_gbm_OH
roc_svm_OH
roc_nn_OH
roc_lr_OH



# Multiple curves:
g_rf <- ggroc(list(Basic_Imputation=roc_rf_BE, MICE=roc_rf_OH)) + theme_minimal() 
g_gbm <- ggroc(list(Basic_Imputation=roc_gbm_BE, MICE=roc_gbm_OH)) + theme_minimal() 
g_nn <- ggroc(list(Basic_Imputation=roc_nn_BE, MICE=roc_nn_OH)) + theme_minimal() 
g_lr <- ggroc(list(Basic_Imputation=roc_lr_BE, MICE=roc_lr_OH)) + theme_minimal() 
g_svm <- ggroc(list(Basic_Imputation=roc_svm_BE, MICE=roc_svm_OH)) + theme_minimal() 

library(ggpubr)
ggarrange(  
  g_rf  + ggtitle("Random Forest")
  ,g_gbm + ggtitle("Gradient Boosted Machine")
  ,g_nn + ggtitle("Neural Networks")
  ,g_lr + ggtitle("Logistic Regression")
  ,g_svm + ggtitle("Suppport Vector Machines")
  ,nrow=2 ,ncol=3
)