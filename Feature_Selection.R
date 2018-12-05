# Feature Selection -----------------------------------------------------

# Remove Redundant Features -----------------------------------------------------

which(names(train_numeric)=="Var118") #Last Numeric Column
#length(unique(train_numeric$Var118))

NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}
train <- subset(orange_train, select = -NoVar(orange_train))
train_numeric <- train[,1:174]

for(i in 1:ncol(train_numeric)){
  train_numeric[is.na(train_numeric[,i]), i] <- mean(train_numeric[,i], na.rm = TRUE)
}

train_numeric <- subset(train_numeric, select = -NoVar(train_numeric))

# ensure the results are repeatable
set.seed(7)

# load the library
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix <- cor(train_numeric)
anyNA(correlationMatrix)

CorrMatrix <- round(correlationMatrix, 2)
#View(CorrMatrix)

# find attributes that are highly corrected (ideally >0.75, but requested 0.9)
highlyCorrelated <- findCorrelation(CorrMatrix, cutoff=0.9)

library(corrplot)
corrplot(CorrMatrix, order = "hclust", tl.cex = 0.2) #tl.cex for the variable text size

train_numeric <- train_numeric[,-c(highlyCorrelated)]

#Categorical Variables
train_categoric <- train[,175:212]
View(train_categoric)

library(mice)
md.pattern(train_categoric)

miceMod <- mice(train_categoric, method="rf")  # perform mice imputation, based on random forests.
train_categoric <- complete(miceMod)  # generate the completed data.
anyNA(train_categoric)


# Remove Redundant Features -----------------------------------------------------

cor2 = function(df){
  
  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  
  cor_fun <- function(pos_1, pos_2){
    
    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- stats::cor(df[[pos_1]]
                      , df[[pos_2]]
                      , use = "pairwise.complete.obs"
      )
    }
    
    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
    }
    
    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
    }
    
    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
    }
    
    return(r)
  } 
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )
  
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  
  return(corrmat)
}

start_time <- Sys.time()
df <-cor2(train_categoric)
end_time <- Sys.time()
time_elapsed <- end_time - start_time
