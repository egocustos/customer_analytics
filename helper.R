library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)
library(tibble)


NULLcounter <- function(data,perc) {
  a <- lapply(data, function(x) sum(is.na(x))/length(x)) %>% unlist()
  return(sum(a<perc))
}

SDcounter <- function(perc) {
  a <- lapply(data, function(x) sd(as.numeric(x),na.rm=T)/
                (max(as.numeric(x),na.rm=T)-min(as.numeric(x),na.rm = T))) %>%
    unlist()
  a[is.na(a)] <- 0
  return(sum(a<perc))
}

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


pcafunction <- function(data,labels) {
  #assuming all variables are numeric
  
  #4. Sampling to create training and test
  
  set.seed(64)
  samp <- sample(nrow(data), nrow(data)*0.8)
  data.train <- data[samp,]
  data.test <- data[-samp,]
  train.churn<-data.frame(labels$Churn[samp])
  test.churn<-data.frame(labels$Churn[-samp])
  #5. Applying PCA
  pca <- prcomp(data.train, retx=TRUE, center=TRUE, scale=TRUE)
  test.data <- data.frame(predict(pca, newdata=data.test))
  train.data <- data.frame(pca$x)
  #train.data<-train.data[,1:npc]
  #test.data<-test.data[,1:npc]
  return(list(test.data,train.data,test.churn,train.churn))  
}  


num_cat <- function(x) {
  return(nlevels(x))
}


min_freq <- function(x) {
  return(min(table(x)))
}

mean_freq <- function(x) {
  return(round(mean(table(x)),2))
}

max_freq <- function(x) {
  return(max(table(x)))
}

accuracy <- function(table) {
  (table[1,1]+table[2,2])/sum(table)
}
