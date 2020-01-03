# Importing the dataset
dataset = read.csv('Employee_Churn.csv')

# Encoding the target feature as factor
dataset$left = factor(dataset$left, levels = c(0, 1))
dataset$salary = factor(dataset$salary, levels = c('low', 'medium', 'high'))

library(MASS)       # load the MASS package 
tbl = table(dataset$left, dataset$salary) 
tbl                 # the contingency table 

chisq.test(tbl) 