
#Importing the dataset
library(readr)
orange_train<-read.csv("ORANGE_TRAIN_DATA.csv")
#orange_test <-read.csv("ORANGE_TEST_DATA.csv")
objectives <- read.csv("KDD_CUP_2009_OBJECTIVE_COLUMNS_LABELS.csv")

# Encoding the target feature as factor (In this case, Churn)
objectives$Churn[objectives$Churn==-1]<-0
Churn <- factor(objectives$Churn, levels = c(0, 1))

data <- orange_train

#Preprocessing with functions from Data review.R
library(magrittr)
library(dplyr)
data2 <- data %>% 
  select_if(function(x) is.numeric(x) | nlevels(x)<5100) %>% #removes Var217,Var214,Var202,Var200
  select(-Var198,-Var220) %>% #as they are identical to Var222
  NULLremover(0.9) %>% 
  SDremover(0) %>%  #until here is the standard cleaning
  # mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  #  mutate_if(is.factor, as.character) %>%
  #  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .)))) %>%
  #lapply(function(x) categoryToOther(x,100)) %>% data.frame() %>%
  lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()

#Sampling
##########

library(caret)
train_DS_orig <- downSample(x = data2, y = Churn)

#library(corrplot)
corr_matrix_with_NAs <- train_DS_orig[,-ncol(train_DS_orig)] %>% cor(.,use="pairwise.complete.obs") %>% round(., 2) 
#corrplot(., order = "hclust", tl.cex = 0.2) %>% 
for(i in 1:ncol(corr_matrix_with_NAs)){
  corr_matrix_with_NAs[is.na(corr_matrix_with_NAs[,i]), i] <- 0
}
highly_correlated <- findCorrelation(corr_matrix_with_NAs, cutoff=0.9)

train_DS_orig <- train_DS_orig[,-c(highly_correlated)] #181: Churn.X0 / 182:Churn.X1

#Control for ML algorithms
##########################


fitControl <- trainControl(method = "CV", #Cross-validation
                           number = 3, #3-fold
                           verboseIter = TRUE, #Output while running
                           classProbs= TRUE, #needed for ROC
                           summaryFunction = twoClassSummary ) #needed for ROC


# Basic Imputation (BI)-----------------------------------------------------------

train_DS_BI <- train_DS_orig %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .))))

#Test Train Split BI
#################

set.seed(31)
ind_BI <- sample(1:nrow(train_DS_BI),0.8*(nrow(train_DS_BI)))
train_BI <- train_DS_BI[ind_BI,-ncol(train_DS_BI)]
test_BI <- train_DS_BI[-ind_BI,-ncol(train_DS_BI)]

train_DS_BI$Class <- factor(train_DS_BI$Class, levels = c(0, 1))
train_labels_BI <- train_DS_BI$Class[ind_BI]
test_labels_BI <- train_DS_BI$Class[-ind_BI]

#Train RF BI
rf_grid_BI <- expand.grid(mtry=ceiling(ncol(train_DS_BI)/2),splitrule="gini",min.node.size=100)
set.seed(1)
rf_BI <- train(x=train_BI, 
               y=make.names(train_labels_BI), 
               method="ranger", metric="ROC",num.trees=500,
               trControl=fitControl,tuneGrid=rf_grid_BI,importance = 'impurity')
rf_pred_BI <- predict(rf_BI,test_BI,type="prob")
cm_rf_BI <-table(rf_pred_BI$X1>0.5,test_labels_BI)
library(pROC)
roc_rf_BI <- roc(test_labels_BI, rf_pred_BI$X1)
auc_rf_BI <-auc(roc_rf_BI)

#GBM BI
gbm_grid_BI <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                           n.minobsinnode=10)
set.seed(2)
gbm_BI <- train(x=train_BI, 
                y=make.names(train_labels_BI), 
                method="gbm", metric="ROC",
                trControl=fitControl,tuneGrid=gbm_grid_BI)
gbm_pred_BI <- predict(gbm_BI,test_BI,type="prob")
cm_gbm_BI <- table(gbm_pred_BI$X1>0.5,test_labels_BI)
roc_gbm_BI <- roc(test_labels_BI, gbm_pred_BI$X1)
auc_gbm_BI <- auc(roc_gbm_BI)

#SVM BI
svm_grid_BI <- expand.grid(C=1,sigma=0.001)
set.seed(3)
svm_BI <- train(x=train_BI, 
                y=make.names(train_labels_BI),
                method = "svmRadial",
                metric="ROC",
                preProcess = c("center","scale"), #necessary for svm
                trControl=fitControl,
                tuneGrid=svm_grid_BI)
svm_pred_BI <- predict(svm_BI,test_BI,type="prob")
cm_svm_BI <- table(svm_pred_BI$X1>0.5,test_labels_BI)
roc_svm_BI <- roc(test_labels_BI, svm_pred_BI$X1)
auc_svm_BI <- auc(roc_svm_BI)

#NN BI
nn_grid_BI <- data.frame(size=6)
set.seed(4)
nn_BI <- train(x=train_BI, 
               y=make.names(train_labels_BI), 
               method="mlp", metric="ROC",
               trControl=fitControl,tuneGrid=nn_grid_BI,
               preProcess = c("center","scale"))
nn_pred_BI <- predict(nn_BI,test_BI,type="prob")
cm_nn_BI <- table(nn_pred_BI$X1>0.5,test_labels_BI)
roc_nn_BI <- roc(test_labels_BI, nn_pred_BI$X1)
auc_nn_BI <- auc(roc_nn_BI)

#LR BI
set.seed(5)
lr_BI <- train(x=train_BI, 
               y=make.names(train_labels_BI), 
               method="glm", metric="ROC",
               trControl=fitControl#,tuneGrid=lr_grid
               #preProcess = c("center","scale")
)
lr_pred_BI <- predict(lr_BI,test_BI,type="prob")
cm_lr_BI <- table(lr_pred_BI$X1>0.5,test_labels_BI)
roc_lr_BI <- roc(test_labels_BI, lr_pred_BI$X1)
auc_lr_BI <- auc(roc_lr_BI)


# MICE Imputation (MICE)-----------------------------------------------------------
library(mice)
train_DS_MICE <- train_DS_orig
MICE_Imp <-parlmice(train_DS_MICE, method = 'rf' ,seed = 123, n.core = 3, n.imp.core = 1) 
train_DS_MICE<- complete(MICE_Imp) 

#Test Train Split MICE
#################

set.seed(13)
ind_MICE <- sample(1:nrow(train_DS_MICE),0.8*(nrow(train_DS_MICE)))
train_MICE <- train_DS_MICE[ind_MICE,-ncol(train_DS_MICE)]
test_MICE <- train_DS_MICE[-ind_MICE,-ncol(train_DS_MICE)]

train_DS_MICE$Class <- factor(train_DS_MICE$Class, levels = c(0, 1))
train_labels_MICE <- train_DS_MICE$Class[ind_MICE]
test_labels_MICE <- train_DS_MICE$Class[-ind_MICE]

#Train RF MICE
rf_grid_MICE <- expand.grid(mtry=ceiling(ncol(train_DS_MICE)/2),splitrule="gini",min.node.size=100)
set.seed(1)
rf_MICE <- train(x=train_MICE, 
                 y=make.names(train_labels_MICE), 
                 method="ranger", metric="ROC",num.trees=500,
                 trControl=fitControl,tuneGrid=rf_grid_MICE,importance = 'impurity')
rf_pred_MICE <- predict(rf_MICE,test_MICE,type="prob")
cm_rf_MICE <-table(rf_pred_MICE$X1>0.5,test_labels_MICE)
library(pROC)
roc_rf_MICE <- roc(test_labels_MICE, rf_pred_MICE$X1)
auc_rf_MICE <-auc(roc_rf_MICE)

#GBM MICE
gbm_grid_MICE <- expand.grid(n.trees=seq(100,600), interaction.depth=6, shrinkage=0.01, 
                             n.minobsinnode=10)
set.seed(2)
gbm_MICE <- train(x=train_MICE, 
                  y=make.names(train_labels_MICE), 
                  method="gbm", metric="ROC",
                  trControl=fitControl,tuneGrid=gbm_grid_MICE)
gbm_pred_MICE <- predict(gbm_MICE,test_MICE,type="prob")
cm_gbm_MICE <- table(gbm_pred_MICE$X1>0.5,test_labels_MICE)
roc_gbm_MICE <- roc(test_labels_MICE, gbm_pred_MICE$X1)
auc_gbm_MICE <- auc(roc_gbm_MICE)

#SVM MICE
svm_grid_MICE <- expand.grid(C=1,sigma=0.001)
set.seed(3)
svm_MICE <- train(x=train_MICE, 
                  y=make.names(train_labels_MICE),
                  method = "svmRadial",
                  metric="ROC",
                  preProcess = c("center","scale"), #necessary for svm
                  trControl=fitControl,
                  tuneGrid=svm_grid_MICE)
svm_pred_MICE <- predict(svm_MICE,test_MICE,type="prob")
cm_svm_MICE <- table(svm_pred_MICE$X1>0.5,test_labels_MICE)
roc_svm_MICE <- roc(test_labels_MICE, svm_pred_MICE$X1)
auc_svm_MICE <- auc(roc_svm_MICE)

#NN MICE
nn_grid_MICE <- data.frame(size=6)
set.seed(4)
nn_MICE <- train(x=train_MICE, 
                 y=make.names(train_labels_MICE), 
                 method="mlp", metric="ROC",
                 trControl=fitControl,tuneGrid=nn_grid_MICE,
                 preProcess = c("center","scale"))
nn_pred_MICE <- predict(nn_MICE,test_MICE,type="prob")
cm_nn_MICE <- table(nn_pred_MICE$X1>0.5,test_labels_MICE)
roc_nn_MICE <- roc(test_labels_MICE, nn_pred_MICE$X1)
auc_nn_MICE <- auc(roc_nn_MICE)

#LR MICE
set.seed(5)
lr_MICE <- train(x=train_MICE, 
                 y=make.names(train_labels_MICE), 
                 method="glm", metric="ROC",
                 trControl=fitControl#,tuneGrid=lr_grid
                 #preProcess = c("center","scale")
)
lr_pred_MICE <- predict(lr_MICE,test_MICE,type="prob")
cm_lr_MICE <- table(lr_pred_MICE$X1>0.5,test_labels_MICE)
roc_lr_MICE <- roc(test_labels_MICE, lr_pred_MICE$X1)
auc_lr_MICE <- auc(roc_lr_MICE)

#only for Random Forest without Binary Encoding
#data2 <- data2 %>% select_if(function(x) is.numeric(x) | nlevels(x)<50)


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

library(pROC)
library(ggplot2)
g <- ggroc(roc_rf_BI)
g

# with additional aesthetics:
g <-ggroc(roc_rf_BI, alpha = 0.5, colour = "red", linetype = 2, size = 2)

# You can then your own theme, etc.
g + theme_minimal() + ggtitle("My ROC curve")

# Multiple curves:
g_rf <- ggroc(list(Basic_Imputation=roc_rf_BI, MICE=roc_rf_MICE)) + theme_minimal() 
g_gbm <- ggroc(list(Basic_Imputation=roc_gbm_BI, MICE=roc_gbm_MICE)) + theme_minimal() 
g_nn <- ggroc(list(Basic_Imputation=roc_nn_BI, MICE=roc_nn_MICE)) + theme_minimal() 
g_lr <- ggroc(list(Basic_Imputation=roc_lr_BI, MICE=roc_lr_MICE)) + theme_minimal() 
g_svm <- ggroc(list(Basic_Imputation=roc_svm_BI, MICE=roc_svm_MICE)) + theme_minimal() 

library(ggpubr)
ggarrange(  
  g_rf  + ggtitle("Random Forest")
  ,g_gbm + ggtitle("Gradient Boosted Machine")
  ,g_nn + ggtitle("Neural Networks")
  ,g_lr + ggtitle("Logistic Regression")
  ,g_svm + ggtitle("Suppport Vector Machines")
  ,nrow=2 ,ncol=3
)