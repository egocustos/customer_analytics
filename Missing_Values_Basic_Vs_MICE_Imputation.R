
#Importing the dataset
library(readr)
orange_train<-read.csv("ORANGE_TRAIN_DATA.csv")
#orange_test <-read.csv("ORANGE_TEST_DATA.csv")
objectives <- read.csv("KDD_CUP_2009_OBJECTIVE_COLUMNS_LABELS.csv")

# Encoding the target feature as factor (In this case, Churn)
objectives$Churn[objectives$Churn==-1]<-0
Churn <- factor(objectives$Churn, levels = c(0, 1))

data <- cbind(orange_train, Churn)

#Preprocessing with functions from Data review.R
library(magrittr)
library(dplyr)
data2 <- data %>% 
  select_if(function(x) is.numeric(x) | nlevels(x)<5100) %>% #removes Var217,Var214,Var202,Var200
  select(-Var198,-Var220) %>% #as they are identical to Var222
  NULLremover(0.5) %>% 
  SDremover(0) %>%  #until here is the standard cleaning
 # mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
#  mutate_if(is.factor, as.character) %>%
#  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .)))) %>%
  #lapply(function(x) categoryToOther(x,100)) %>% data.frame() %>%
  lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()


#Test Train Split
#################

set.seed(31)
ind <- sample(1:nrow(data2),0.8*(nrow(data2)))
train <- data2[ind,]
test_orig <- data2[-ind,]

labels <- objectives
train_labels <- labels$Churn[ind]
test_labels <- labels$Churn[-ind]

#Sampling
##########

library(caret)
train_DS_orig <- downSample(x = train, y = factor(train_labels))

library(corrplot)
highly_correlated <- train_DS_orig %>% lapply(function(x) x <- binaryEncoding(x)) %>% data.frame() %>% 
                      cor(.) %>% round(., 2) %>% corrplot(., order = "hclust", tl.cex = 0.2) %>% 
                      findCorrelation(., cutoff=0.99)

train_DS_orig <- train_DS_orig[,-c(highly_correlated,181,182)] #181: Churn.X0 / 182:Churn.X1

# Basic Imputation (BI)-----------------------------------------------------------

train_DS_BI <- train_DS_orig %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .))))

# MICE Imputation (MICE)-----------------------------------------------------------

train_DS_MICE <- train_DS_orig %>% parlmice(., method = 'rf' ,seed = 123, n.core = 3, n.imp.core = 1) %>% 
                  complete() 

Mice_Imputation <- function(df) {
  imp_cat <- parlmice(df, method = 'rf' ,seed = 123, n.core = 3, n.imp.core = 1)  # perform mice imputation, based on random forests.
  Mice_Imputed <- complete(imp_cat)  # generate the completed data.
  #anyNA(train_cat_DS)
  return(Mice_Imputed)
}

#only for Random Forest without Binary Encoding
#data2 <- data2 %>% select_if(function(x) is.numeric(x) | nlevels(x)<50)



#PCA and test train split
#########################

MICE_list <- pcafunction(data2,labels)
MICE_train <- MICE_list[[2]]
MICE_test_orig <- MICE_list[[1]]

pca_train_labels <- pca_list[[4]]
pca_test_labels <- pca_list[[3]]

#Sampling
##########

pca_train_upsample_orig <- downSample(x = pca_train, y = factor(pca_train_labels[,1]))



#Test Train Split
#################

set.seed(31)
ind <- sample(1:nrow(data2),0.8*(nrow(data2)))
train <- data2[ind,]
test_orig <- data2[-ind,]

labels <- objectives
train_labels <- labels$Churn[ind]
test_labels <- labels$Churn[-ind]




#VarImp
########

rf_grid <- expand.grid(mtry=90,splitrule="gini",min.node.size=100)
set.seed(1)
rf <- train(x=train_DS_orig[,-ncol(train_DS_orig)], 
            y=make.names(train_upsample$Class), 
            method="ranger", metric="ROC",num.trees=500,
            trControl=fitControl,tuneGrid=rf_grid,importance = 'impurity')


imp <- varImp(rf)
imp <- imp$importance %>% mutate(col=rownames(.)) %>% arrange(desc(Overall)) %>% 
  select(col)



#Control for ML algorithms
##########################


fitControl <- trainControl(method = "CV", #Cross-validation
                           number = 3, #3-fold
                           verboseIter = TRUE, #Output while running
                           classProbs= TRUE, #needed for ROC
                           summaryFunction = twoClassSummary ) #needed for ROC


#Train function with try catch
#################
#try catch to avoid errors


train_rf <- function(n_features) {
  out <- tryCatch(
    {
      rf_grid <- expand.grid(mtry=ceiling(ncol(train_DS)/2),splitrule="gini",min.node.size=100)
      set.seed(1)
      rf <- train(x=train_DS[,-ncol(train_DS)], 
                  y=make.names(train_DS$Class), 
                  method="ranger", metric="ROC",num.trees=500,
                  trControl=fitControl,tuneGrid=rf_grid,importance = 'impurity')
      rf_pred <- predict(rf,test,type="prob")
      #table(rf_pred$X1>0.5,test_labels$Churn)
      roc_rf <- roc(test_labels, rf_pred$X1)
      return(auc(roc_rf))
    },
    error=function(cond) {
      message(paste("Method not working: Random forest"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )    
  return(out)
}

train_gbm <- function(n_features) {
  out <- tryCatch(
    {
      gbm_grid <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                              n.minobsinnode=10)
      set.seed(1)
      gbm <- train(x=train_DS[,-ncol(train_DS)], 
                   y=make.names(train_DS$Class), 
                   method="gbm", metric="ROC",
                   trControl=fitControl,tuneGrid=gbm_grid)
      gbm_pred <- predict(gbm,test,type="prob")
      #table(gbm_pred$X1>0.5,test_labels$Churn)
      roc_gbm <- roc(test_labels, gbm_pred$X1)
      return(auc(roc_gbm))
    },
    error=function(cond) {
      message(paste("Method not working: Gradient boosted trees"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )    
  return(out)
}

train_svm <- function(n_features) {
  out <- tryCatch(
    {
      svm_grid <- expand.grid(C=1,sigma=0.001)
      set.seed(1)
      svm <- train(x=train_DS[,-ncol(train_DS)], 
                   y=make.names(train_DS$Class),
                   method = "svmRadial",
                   metric="ROC",
                   preProcess = c("center","scale"), #necessary for svm
                   trControl=fitControl,
                   tuneGrid=svm_grid)
      svm_pred <- predict(svm,test,type="prob")
      #table(svm_pred$X1>0.5,test_labels$Churn)
      roc_svm <- roc(test_labels, svm_pred$X1)
      return(auc(roc_svm))
    },
    error=function(cond) {
      message(paste("Method not working: Support vector machine"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )    
  return(out)
}

train_nn <- function(n_features) {
  out <- tryCatch(
    {
      nn_grid <- data.frame(size=6)
      set.seed(1)
      nn <- train(x=train_DS[,-ncol(train_DS)], 
                  y=make.names(train_DS$Class), 
                  method="mlp", metric="ROC",
                  trControl=fitControl,tuneGrid=nn_grid,
                  preProcess = c("center","scale")
      )
      nn_pred <- predict(nn,test,type="prob")
      #table(nn_pred,test_labels$Churn)
      roc_nn <- roc(test_labels, nn_pred$X1)
      return(auc(roc_nn))
    },
    error=function(cond) {
      message(paste("Method not working: Neural Network"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )    
  return(out)
}

train_lr <- function(n_features) {
  out <- tryCatch(
    {
      set.seed(1)
      lr <- train(x=train_DS[,-ncol(train_DS)], 
                  y=make.names(train_DS$Class), 
                  method="glm", metric="ROC",
                  trControl=fitControl#,tuneGrid=lr_grid
                  #preProcess = c("center","scale")
      )
      lr_pred <- predict(lr,test,type="prob")
      #table(lr_pred$X1>0.5,test_labels$Churn)
      roc_lr <- roc(test_labels, lr_pred$X1)
      return(auc(roc_lr))
    },
    error=function(cond) {
      message(paste("Method not working: Logistic Regression"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )    
  return(out)
}


auc_frame=matrix(nrow=50,ncol=6)
time_frame=matrix(nrow=50,ncol=6)

pca_auc_frame=matrix(nrow=25,ncol=6)
pca_time_frame=matrix(nrow=25,ncol=6)

for(i in seq(2,100,2)) {
  #Decide how many of the top N variables you need
  auc_frame[i/2,1] <- i
  time_frame[i/2,1] <- i
  train_DS <- train_DS_orig %>% select(which(colnames(.) %in% imp[1:i,]),Class)
  test <- test_orig %>% select(which(colnames(.) %in% imp[1:i,]))
  
  
  #Random forest
  ##############
  time_frame[i/2,2] <- system.time(
    auc_frame[i/2,2] <- train_rf(i)
  )[3]
  
  #Gradient boosting trees
  ########################
  
  time_frame[i/2,3] <- system.time(
    auc_frame[i/2,3] <- train_gbm(i)
  )[3]
  
  #Support vector machine
  #######################
  
  time_frame[i/2,4] <- system.time(
    auc_frame[i/2,4] <- train_svm(i)
  )[3]
  
  #Neural Network
  ################
  
  time_frame[i/2,5] <- system.time(
    auc_frame[i/2,5] <- train_nn(i)
  )[3]
  
  #Logistic Regression
  ####################
  
  time_frame[i/2,6] <- system.time(
    auc_frame[i/2,6] <- train_lr(i)
  )[3]
  
  
  pca_auc_frame[i/2,1] <- i
  pca_time_frame[i/2,1] <- i
  train_upsample <- pca_train_upsample_orig %>% select(1:i,Class)
  test <- pca_test_orig %>% select(1:i)
  
  
  ##############
  #Churn prediction
  ##############
  
  
  #Random forest
  ##############
  pca_time_frame[i/2,2] <- system.time(
    pca_auc_frame[i/2,2] <- train_rf(i)
  )[3]
  
  #Gradient boosting trees
  ########################
  
  pca_time_frame[i/2,3] <- system.time(
    pca_auc_frame[i/2,3] <- train_gbm(i)
  )[3]
  
  #Support vector machine
  #######################
  
  pca_time_frame[i/2,4] <- system.time(
    pca_auc_frame[i/2,4] <- train_svm(i)
  )[3]
  
  #Neural Network
  ################
  
  pca_time_frame[i/2,5] <- system.time(
    pca_auc_frame[i/2,5] <- train_nn(i)
  )[3]
  
  #Logistic Regression
  ####################
  
  pca_time_frame[i/2,6] <- system.time(
    pca_auc_frame[i/2,6] <- train_lr(i)
  )[3]
  
  message(i)
}

compare_output <- function(frame,measured_value) {
  auc_df <- data.frame(frame)
  colnames(auc_df) <- c("number_features","Random forest","Gradient Boosted Trees",
                        "Support Vector Machine","Neural Network","Log Regression")
  auc_df_melt <- melt(auc_df,id.vars="number_features")
  ggplot(auc_df_melt,aes(x=number_features))+
    geom_line(aes(y=value,col=variable),size=2)+
    labs(y=measured_value,x="Number of features included")+
    scale_color_discrete(guide=FALSE)+
    theme(text = element_text(size=20)) +
    ylim(low=0,high=105)
}

compare_output(pca_auc_frame,"AUC value")
compare_output(pca_time_frame,"time [sec]")
compare_output(auc_frame,"AUC value")
compare_output(time_frame,"time [sec]")
