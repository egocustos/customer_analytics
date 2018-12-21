library(dplyr)
library(caret)
library(pROC)

setwd("E:/Users/Richard.Vogg/Documents/Customer Analytics/PoC_ML") #path to the documents
data <- read.table("orange_small_train.data.csv",sep=",",header=T)
labels <- read.table("labels.csv",sep=",",header=T)
data_orig <- data
data[data==""] <- NA

#############
#Functions
#############

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
#for applying this to a dataframe df call
#df <- df %>% lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()


#Example for preprocessing with the previous defined functions
data2 <- data %>% 
  select_if(function(x) is.numeric(x) | nlevels(x)<5100) %>% #removes Var217,Var214,Var202,Var200
  select(-Var198,-Var220) %>% #as they are identical to Var222
  NULLremover(1) %>% 
  SDremover(0) %>%  
  mutate_if(is.numeric, funs(ifelse(is.na(.), mean(.,na.rm = T),.))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character,funs(factor(ifelse(is.na(.), "missing", .)))) %>%
  lapply(function(x) categoryToOther(x,100)) %>% data.frame() %>%
  lapply(function(x) x <- binaryEncoding(x)) %>% data.frame()

data2 <- data2 %>% SDremover(0) %>% select_if(function(x) is.numeric(x) | nlevels(x)<50)


#Train-Test split
set.seed(64)
ind <- sample(1:nrow(data2),0.8*(nrow(data2)))
train <- data2[ind,]
test <- data2[-ind,]

train_labels <- labels[ind,]
test_labels <- labels[-ind,]

train_upsample <- downSample(x = train, y = factor(train_labels$Churn))

fitControl <- trainControl(method = "CV",
                           number = 3,
                           verboseIter = TRUE,
                           classProbs= TRUE,
                           summaryFunction = twoClassSummary)

#Random forest

#rf_grid <- data.frame(mtry=seq(50,100,10),splitrule="gini",min.node.size=1
rf_grid <- expand.grid()

rf <- train(x=train_upsample[,-ncol(train_upsample)], 
            y=make.names(train_upsample$Class), 
            method="ranger", metric="ROC",num.trees=500,
            trControl=fitControl,tuneGrid=rf_grid)
rf_pred <- predict(rf,test,type="prob")
table(rf_pred$X1>0.5,test_labels$Churn)

#Calculating the AUC
roc_obj <- roc(test_labels$Churn, rf_pred$X1)
auc(roc_obj)
