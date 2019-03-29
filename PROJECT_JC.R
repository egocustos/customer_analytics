#Importing the dataset
library(readr)
data<-read.csv("query.csv")
anyNA(data)
#Types of variables

#Factors
# Encoding the target feature as factor (In this case, Churn)
data$Ins <- factor(data$Ins, levels = c(1, 0)) #Objective: If has purchased the Insurance (1=yes 0=no)
objective <- data$Ins
#Encoding factor columns as factors (based on details)
data$ATM <- factor(data$ATM, levels = c(1, 0)) #If use ATM Sevice (1=yes 0=no)
data$CC <- factor(data$CC, levels = c(1, 0)) #If has credit card account (1=yes 0=no)
data$CD <- factor(data$CD, levels = c(1, 0)) #If has certificate of deposit (1=yes 0=no)
data$DDA <- factor(data$DDA, levels = c(1, 0)) #If has a checking acount (1=yes 0=no)
data$DirDep <- factor(data$DirDep, levels = c(1, 0)) #direct deposit (1=yes 0=no)
data$HMOwn <- factor(data$HMOwn, levels = c(1, 0)) #If owns Home (1=yes 0=no)
data$ILS <- factor(data$ILS, levels = c(1, 0)) #has installment loan (1=yes 0=no)
data$IRA <- factor(data$IRA, levels = c(1, 0)) #has retirement account (1=yes 0=no)
data$InArea <- factor(data$InArea, levels = c(1, 0)) #local address (1=yes 0=no)
data$Inv <- factor(data$Inv, levels = c(1, 0)) #Has Investment account (1=yes 0=no)
data$LOC <- factor(data$LOC, levels = c(1, 0)) #has line of credit (1=yes 0=no)
data$MM <-  factor(data$MM, levels = c(1, 0)) #has money market account (1=yes 0=no)
data$MTG <- factor(data$MTG, levels = c(1, 0)) #has mortgage account (1=yes 0=no)
data$Moved <- factor(data$Moved, levels = c(1, 0)) #recent address change (1=yes 0=no)
data$NSF <- factor(data$NSF, levels = c(1, 0)) #occurrence of insufficient funds (1=yes 0=no)
data$SDB <- factor(data$SDB, levels = c(1, 0)) #has a safety deposit box (1=yes 0=no)
data$Sav <- factor(data$Sav, levels = c(1,0)) #savings account (1=yes 0=no)

factors <- subset(data, select=c(ATM,CC,CD,DDA,DirDep,HMOwn,ILS,IRA,InArea,Inv,LOC,MM,MTG,Moved,NSF,SDB,Sav))

#Numerical
str(data$AcctAge)

data$ATMAmt #ATM Withdrawal Amount
data$AcctAge #age of oldest account in years
data$Age #age of customer in years
data$CCBal #Credit Card Balance
data$CCPurc #number of credit card purchases
data$CDBal #certificate of deposit balance
data$CRScore #Credit Score
data$CashBk #number of times customer received cash back
data$Checks #Number of Checks
data$DDABal #Checking Balance
data$Dep #number of checking deposits
data$DepAmt #Amount Deposited
data$HMVal #home value in thousands of dollars
data$ILSBal #installment loan balance
data$IRABal #retirement account balance
data$Income #income in thousands of dollars
data$InvBal #Investment Balance
data$LOCBal #Line of credit balance
data$LORes #Length of Residency in years
data$MMBal #Money Market Balance
data$MMCred #number of money market credits
data$MTGBal #Mortgage Balance
data$NSFAmt #amount of insufficient funds
data$POS #number of point of sale transactions 
data$POSAmt #amount in point of sale transactions
data$Phone #number of times customer used telephone banking
data$SavBal #Savings Balance
data$Teller #Number of teller visits

numerical <- subset(data, select=c(ATMAmt,AcctAge,Age,CCBal,CCPurc,CDBal,CRScore,CashBk,Checks,DDABal,Dep,DepAmt,HMVal,ILSBal,IRABal,Income,InvBal,LOCBal,LORes,MMBal,MMCred,MTGBal,NSFAmt,POS,POSAmt,Phone,SavBal,Teller))

#Categorical
nlevels(data$Branch) #Branch of Bank
nlevels(data$Res) #Area Classification: (R=rural, S=suburb, U=urban)

categorical <- subset(data, select= c(Branch,Res))

summary(numerical)


#Preprocessing with functions from Data review.R
library(magrittr)
library(dplyr)
data2 <- data %>% 
                  NULLremover(0.9) %>% 
                                      SDremover(0)
#DownSampling
library(caret)
train_DS_orig <- downSample(x = data2[ , names(data2) != "Ins"], y = data2$Ins)
num <- colnames(numerical)
numerical_DS <- train_DS_orig[num]

cat <- colnames(categorical)
categorical_DS <- train_DS_orig[cat]

fac <- colnames(factors)
factors_DS <- train_DS_orig[fac]




numerical_with_NAs <- numerical_DS
corrmatrix_with_NAs <-cor(numerical_with_NAs,use="pairwise.complete.obs")
CorrMatrix_with_NAs <- round(corrmatrix_with_NAs, 2)

# for(i in 1:ncol(corr_matrix_with_NAs)){
#   corr_matrix_with_NAs[is.na(corr_matrix_with_NAs[,i]), i] <- 0
# }

?findCorrelation
highly_correlated <- findCorrelation(corrmatrix_with_NAs, cutoff=0.9, names = TRUE)

library(corrplot)
corrplot(CorrMatrix_with_NAs, order = "hclust", tl.cex = 0.8)


train_DS_orig <- train_DS_orig[,!(colnames(train_DS_orig) %in% highly_correlated)] #CCBal out

train_DS_orig_BE <- train_DS_orig %>% lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()


# Basic Imputation (BI)-----------------------------------------------------------

train_DS_BI <- train_DS_orig %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .))))

#Split for BI
#################

set.seed(31)
ind_BI <- sample(1:nrow(train_DS_BI),0.8*(nrow(train_DS_BI)))
train_BI <- train_DS_BI[ind_BI,-ncol(train_DS_BI)]
test_BI <- train_DS_BI[-ind_BI,-ncol(train_DS_BI)]

#train_DS_BI$Class <- factor(train_DS_BI$Class, levels = c(0, 1))
train_labels_BI <- train_DS_BI$Class[ind_BI]
test_labels_BI <- train_DS_BI$Class[-ind_BI]

#Control for ML algorithms
##########################
fitControl <- trainControl(method = "CV", #Cross-validation
                           number = 3, #3-fold
                           verboseIter = TRUE, #Output while running
                           classProbs= TRUE, #needed for ROC
                           summaryFunction = twoClassSummary ) #needed for ROC


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
auc_rf_BI <-auc(roc_rf_BI) #It's 1

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


#Split for BI
#################

library(mice)
train_MI <- data
MI <-parlmice(train_MI, method = 'rf' ,seed = 123, n.core = 2, n.imp.core = 1) 
train_DS_MICE<- complete(MI) 




data_BI <- data %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .))))

data_lr <- data_BI
data_lr <- subset(data_BI, select = -Branch)

data_f_1 <- subset(data_lr, select = c("AcctAge", "DDA", "DDABal","CashBk", "Checks"
                                       ,"Phone","Teller", "Sav", "SavBal","ATM" 
                                       ,"ATMAmt", "POS", "POSAmt", "CD", "IRA"
                                       ,"LOC" ,"MM", "MMBal", "MTG", "MTGBal"
                                       ,"CC","CCBal","SDB", "LORes", "HMVal"
                                       ,"Branch", "Dep","Inv", "Ins" ))

Index <- createDataPartition(data_lr$Ins, p = 0.8, list = FALSE)
train <- data_lr[Index,-42]
test <- data_lr[-Index,-42]

#f_1
train <- data_lr[Index,-42]
test <- data_lr[-Index,-42]

#data_lr$Ins <- factor(data_lr$Ins, levels = c(0, 1))
train_labels <- data_lr$Ins[Index]
test_labels <- data_lr$Ins[-Index]

set.seed(31)
ind <- sample(1:nrow(data_lr),0.8*(nrow(data_lr)))
train <- data_lr[ind,-42]
test <- data_lr[-ind,-42]

data_lr$Ins <- factor(data_lr$Ins, levels = c(0, 1))
train_labels <- data_lr$Ins[ind]
test_labels <- data_lr$Ins[-ind]

#Control for ML algorithms
##########################
library(caret)
fitControl <- trainControl(method = "CV", #Cross-validation
                           number = 10, #10-fold
                           verboseIter = TRUE, #Output while running
                           classProbs= TRUE, #needed for ROC
                           summaryFunction = twoClassSummary ) #needed for ROC

#LR BI
set.seed(5)
lr <- train(x=train, 
               y=make.names(train_labels), 
               method="glm", metric="ROC",
               trControl=fitControl, na.action =na.omit, #,tuneGrid=lr_grid
               preProcess = c("center","scale")
)
lr_pred <- predict(lr,test,type="prob")
cm_lr <- table(lr_pred$X1>0.5,make.names(test_labels))
library(pROC)
roc_lr <- roc(test_labels, lr_pred$X1)
auc_lr <- auc(roc_lr)





