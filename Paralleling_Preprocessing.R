train_num_DS_replace_mean <- train_numeric_DS

for(i in 1:ncol(train_num_DS_replace_mean)){
  train_num_DS_replace_mean[is.na(train_num_DS_replace_mean[,i]), i] <- mean(train_num_DS_replace_mean[,i], na.rm = TRUE)
}

NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}
train_num_DS_replace_mean <- subset(train_num_DS_replace_mean, select = -NoVar(train_num_DS_replace_mean))

# ensure the results are repeatable
set.seed(31)

# load the library
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix_nas_rep_mean <- cor(train_num_DS_replace_mean)
anyNA(correlationMatrix_nas_rep_mean)

CorrMatrix_nas_rep_mean <- round(correlationMatrix_nas_rep_mean, 2)
#View(CorrMatrix)

# find attributes that are highly corrected (ideally >0.75, but requested 0.9)
highlyCorrelated_nas_rep_mean <- findCorrelation(CorrMatrix_nas_rep_mean, cutoff=0.9)

library(corrplot)
corrplot(CorrMatrix_nas_rep_mean, order = "hclust", tl.cex = 0.2) #tl.cex for the variable text size

train_num_DS <- train_numeric_DS[,-c(highlyCorrelated_nas_rep_mean)]

library(mice)
library(parallel)

#install.packages("devtools")
#devtools::install_github(repo = "stefvanbuuren/mice")


tic('Using parlMICE:')

imp <- parlmice(train_num_DS, method = 'rf' ,seed = 31)

train_numeric_DS_parl <- complete(imp)  # generate the completed data.
anyNA(train_numeric_DS_parl)

toc()

for(i in 1:ncol(train_numeric_DS_parl)){
  train_numeric_DS_parl[is.na(train_numeric_DS_parl[,i]), i] <- 99999
}

#Categorical Variables
train_cat_DS <- train_DS[,175:212]
#View(train_categoric)

tic('parlmice on categorical data:')

library(mice)

imp_cat <- parlmice(train_cat_DS, method = 'rf' ,seed = 31)  # perform mice imputation, based on random forests.
train_cat_DS <- complete(imp_cat)  # generate the completed data.
anyNA(train_cat_DS)

toc()

clean_train_DS <- cbind(train_numeric_DS_parl,train_cat_DS,train_DS$Class)
anyNA(clean_train_DS)

library(fastDummies)

tic('Making dummies for Categorical variables:')

dummy_cat_DS <- dummy_cols(train_cat_DS, select_columns = NULL, remove_first_dummy = FALSE,
           remove_most_frequent_dummy = FALSE)
toc()

dummy_cat_DS_num <- dummy_cat_DS[,39:21588]

# calculate correlation matrix
correlationMatrix_dummies <- cor(dummy_cat_DS_num)
anyNA(correlationMatrix_dummies)

CorrMatrix_dummies <- round(correlationMatrix_dummies, 2)
#View(CorrMatrix)

# find attributes that are highly corrected (ideally >0.75, but requested 0.9)
highlyCorrelated_dummies <- findCorrelation(CorrMatrix_dummies, cutoff=0.9)

dummy_cat_DS_num <- dummy_cat_DS_num[,-c(highlyCorrelated_dummies)]

View(CorrMatrix_dummies[,1:100])

library(doParallel)
cl <- makePSOCKcluster(7)
registerDoParallel(cl)

## All subsequent models are then run in parallel
model_rf <- train(`train_DS$Class` ~ ., data = clean_train_DS, method = "rf")

## When you are done:
stopCluster(cl)




