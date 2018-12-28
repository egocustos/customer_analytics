#Importing the dataset
library(readr)
orange_train<-read.csv("ORANGE_TRAIN_DATA.csv")
# orange_test <-read.csv("ORANGE_TEST_DATA.csv")
objectives <- read.csv("KDD_CUP_2009_OBJECTIVE_COLUMNS_LABELS.csv")

# Encoding the target feature as factor (In this case, Churn)
objectives$Churn[objectives$Churn==-1]<-0
Churn <- factor(objectives$Churn, levels = c(0, 1))

library(caret)
Orange_DS<-downSample(orange_train,Churn)

NoVar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

Orange_DS <- subset(Orange_DS, select = -NoVar(Orange_DS))

library("dplyr")
Orange_numeric_DS <- select_if(Orange_DS, is.numeric)

for(i in 1:ncol(Orange_numeric_DS)){
  Orange_numeric_DS[is.na(Orange_numeric_DS[,i]), i] <- mean(Orange_numeric_DS[,i], na.rm = TRUE)
}

Orange_numeric_DS <- subset(Orange_numeric_DS, select = -NoVar(Orange_numeric_DS))

# ensure the results are repeatable
set.seed(7)

# load the library
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix <- cor(Orange_numeric_DS)
anyNA(correlationMatrix)

CorrMatrix <- round(correlationMatrix, 2)
#View(CorrMatrix)

# find attributes that are highly corrected (ideally >0.75, but requested 0.9)
highlyCorrelated <- findCorrelation(CorrMatrix, cutoff=0.99, names = F)

Orange_numeric_DS <- Orange_numeric_DS[,-c(highlyCorrelated)]

Orange_numeric_DS <- subset(Orange_DS, select=names(Orange_numeric_DS))
#anyNA(Orange_numeric_DS) #shall be TRUE

library(mice)
library(parallel)
library(tictoc)

tic('Using parlMICE:')

imp <- parlmice(Orange_numeric_DS, method = 'rf' ,seed = 31, n.core = 7, n.imp.core = 2) #Error in serialize(data, node$con) : error writing to connection

Orange_numeric_DS_parl <- complete(imp)  # generate the completed data.
anyNA(Orange_numeric_DS_parl)

toc()


names(head(sort(colSums(is.na(Orange_numeric_DS_parl)> 0), decreasing = T)))

which(names(Orange_DS)=="Var90")
# Var118  Var29  Var66  Var90 Var156 Var105  Var71  Var91  Var88 Var128 
# 7321    7249   7249   7249   7249   7243   7222   7222   7218   7218 
#Var2 Var122  Var27 Var138   
#7201   7201   7175   7154 

Orange_DS[,c(107,213)]

#NoVar (just 1 value) 
boxplot(Orange_DS$Var118) # 0 and 3
boxplot(Orange_DS$Var29) #Var29: 0
boxplot(Orange_DS$Var90) # 0
boxplot(Orange_DS$Var2) # 0
boxplot(Orange_DS[,127])

sum(is.na(Orange_DS$col))

#Just 1 value (no churn)
boxplot(Orange_DS[,111]) 
boxplot(Orange_DS[,24])

#Cases of stduy (all have multiples values)
boxplot(Orange_DS$Var66) #
boxplot(Orange_DS$Var156)#
boxplot(Orange_DS[,94]) #
boxplot(Orange_DS[,61]) #
boxplot(Orange_DS[,80]) #
boxplot(Orange_DS[,77]) #
boxplot(Orange_DS[,117]) #



Orange_DS_NA_cols <- sapply(Orange_numeric_DS_parl, function(x) sum(is.na(x))) 
df <- subset(Orange_numeric_DS_parl, select = -names(Orange_DS_NA_cols[Orange_DS_NA_cols>0]))



