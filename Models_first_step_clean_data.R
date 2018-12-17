library(caTools)
set.seed(123)
split <- sample.split(clean_train$Churn, SplitRatio = 0.75)
clean_training_set <- subset(clean_train, split == TRUE)
clean_test_set <- subset(clean_train, split == FALSE)


# Naive Bayes -------------------------------------------------------------


# Fitting NB to the Training set
#install.packages('e1071')
library(e1071)
clean_classifier_nb = naiveBayes(x = clean_training_set[-212],
                           y = clean_training_set$Churn)

# Predicting the Test set results
y_pred_nb_clean = predict(clean_classifier_nb, newdata = clean_test_set[-212])

# Making the Confusion Matrix
cm_nb <- table(clean_test_set[, 212], y_pred_nb_clean)
#Results: 
# True Negatives Raw data: 2304/11582 (19.89%)
# True Positives Raw data: 822/918 (89.54%)

# True Negatives clean data: 1144/11582 (9.88%)
# True Positives clean data: 822/918 (93.9%)

# Random Forest -----------------------------------------------------------

#It is required to omit the columns which have 0 variance
NoManyCat <- function(dat) {
  out <- sapply(dat, function(x) nlevels(x))
  want <- which(!out <= 53)
  unlist(want)
}

train_categoric_many_levels <- subset(train_categoric, select = NoManyCat(train_categoric))
summary(train_categoric_many_levels$Var197)
hist(train_categoric_many_levels$Var192)

library(plyr)
freq_df <- count(train_categoric_many_levels, 'Var197')
muestra <- train_categoric_many_levels$Var197
muestra <- ddply(as.data.frame(muestra), .(muestra), nrow)
muestra$muestra <- with(muestra, reorder(muestra, -V1))

p <- ggplot(muestra, aes(x=muestra, y=V1)) + geom_bar(stat="identity")
print(p)
df<-colnames(freq_df$Var197)
ggplot(data=freq_df, aes(x=reorder(new,-freq), y=freq)) + 
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x=element_text(size=1))
  #theme_grey(base_size = 4)
muestra$`train_categoric_many_levels$Var197` <- with(muestra, reorder(train_categoric_many_levels$Var197, -V1))
orden <- names(sort(train_categoric_many_levels$Var197), decreasing=TRUE)

library(dplyr)
add_column(freq_df, new = 1:226)
freq_df<-mutate(freq_df, new = 1:226)
library(ggplot2)
# counts
ggplot(train_categoric_many_levels, aes(x=reorder(Var197,y=..count..))) +
  geom_bar()

library(dplyr)
library(magrittr)
train_categoric_many_levels$Var197 %>% 
  #group_by(Var197) %>% 
  filter(n()>500)


train_categoric_rf <- subset(train_categoric, select = -NoManyCat(train_categoric))
train_rf <- cbind(train_numeric, train_categoric_rf, Churn)

library(caTools)
set.seed(321)
split_rf <- sample.split(train_rf$Churn, SplitRatio = 0.75)
training_set_rf <- subset(train_rf, split_rf == TRUE)
test_set_rf <- subset(train_rf, split_rf == FALSE)


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier_rf = randomForest(x = training_set_rf[-187],
                             y = training_set_rf$Churn,
                             ntree = 200)

classif_rf<-train(Churn~., data = training_set_rf, method = "ranger", importance = TRUE)

a<-sapply(train_rf, function(x) nlevels(x))

# Predicting the Test set results
y_pred_rf = predict(classifier_rf, newdata = test_set_rf[,-187])

# Making the Confusion Matrix
cm_rf = table(test_set_rf[, 187], y_pred_rf)
# True Negatives clean data: 11,306/12,216 (92.55%)
# True Positives clean data: 8/284 (2.82%)

library(plyr)
count(y_pred_rf)

summary(train_categoric_many_levels$Var197)


library(caret) 
confusionMatrix(cm_rf)
