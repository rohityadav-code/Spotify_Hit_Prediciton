# Importing required Libraries

library(caTools)
library(caret)
library(e1071)
library(class)
library(rpart)
library(rpart.plot)
library(ROCR)
library(forecast)
library(Metrics)
library(data.table)
library(tidyverse)
library(R.utils)
library(ggplot2)
library(ggthemes)
library(ggmap)

dataset <- read.csv("FinalDataSet.csv", sep=",", header=TRUE)

str(dataset)

### Data Preprocessing ###

# Shuffle dataset
set.seed(1)

spotify_dataset <- dataset[sample(nrow(dataset)),]

# Remove fields that won't be used for building model

spotify_dataset$mode <- NULL
spotify_dataset$track <- NULL
spotify_dataset$artist <- NULL
spotify_dataset$key <- NULL
spotify_dataset$uri <- NULL
spotify_dataset$sections <- NULL
spotify_dataset$genre<-NULL
spotify_dataset$time_signature <- NULL
spotify_dataset$duration_ms <- as.numeric(spotify_dataset$duration_ms/60000)

# Encode categorial data

spotify_dataset$top100  <- factor(spotify_dataset$top100,
                                  levels=c(0,1),
                                  labels=c(0,1))


## Split dataset
spotify_dataset[, 1:10] <- scale(spotify_dataset[, 1:10])

split        <- sample.split(spotify_dataset$top100, SplitRatio = 0.70)
training_set <- subset(spotify_dataset, split == TRUE)
validation_set     <- subset(spotify_dataset, split == FALSE)

## Feature Scaling 
training_set[, 1:10] <- scale(training_set[, 1:10])
validation_set[, 1:10] <- scale(validation_set[, 1:10])



#########   1. Logistic Regression   ############

# Create classifier and fit logistic regression to training_set
classifier <- glm(formula = top100 ~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)

# predicting the training_set results
prob_pred_training <- predict(classifier, type = 'response', newdata = training_set[,-11])
y_pred_training <- ifelse(prob_pred_training > 0.5, 1, 0) # change probabilities to binary

# predicting the validation_set results
prob_pred_validation <- predict(classifier, type = 'response', newdata = validation_set[,-11])
y_pred_validation <- ifelse(prob_pred_validation > 0.5, 1, 0) # change probabilities to binary

# Build the confusion matrix & determine accuracy of training data
lr_cm_training <- table(training_set[,11], y_pred_training)
lr_accuracy_training <- (sum(diag(lr_cm_training))/sum(lr_cm_training))
lr_cm_training
paste("Training Data Accuracy:",round(lr_accuracy_training*100,digits=2),"%") # ~79% for 0.7 split

# Build the confusion matrix & determine accuracy of validation data
lr_cm_validation <- table(validation_set[,11], y_pred_validation)
lr_accuracy_validation <- (sum(diag(lr_cm_validation))/sum(lr_cm_validation))
lr_cm_validation
paste("Validation Data Accuracy:",round(lr_accuracy_validation*100,digits=2),"%") # ~81% for 0.3 split

# Performance Evaluation of validation data
pred_lr <- prediction(y_pred_validation, validation_set[,11])
lr_roc <- performance(pred_lr, "tpr","fpr")


#########   2. K-Nearest Neighbours   ############


# train test split 
set.seed(1)

split        <- sample.split(spotify_dataset$top100, SplitRatio = 0.7)
training_set <- subset(spotify_dataset, split == TRUE)
validation_set     <- subset(spotify_dataset, split == FALSE)

# feature scaling
training_set[, 1:10] <- scale(training_set[, 1:10])
validation_set[, 1:10] <- scale(validation_set[, 1:10])


# fit K-NN to training set and predict training set results
# Rule of thumb: k = sqrt(nrow(training_set)) in this case, 66
y_pred_training_knn <- knn(train = training_set[, -11],
                          test  = training_set[, -11],
                          cl    = training_set[, 11],
                          k     = 7)


# fit K-NN to training set and predict validation set results
# Rule of thumb: k = sqrt(nrow(training_set)) in this case, 66
y_pred_validation_knn <- knn(train = training_set[,-11],
                            test  = validation_set[,-11],
                            cl    = training_set[, 11],
                            k     = 7)

# Build the confusion matrix & determine accuracy of training data
knn_cm_training <- table(training_set[, 11], y_pred_training_knn)
knn_accuracy_training <- (sum(diag(knn_cm_training))/sum(knn_cm_training))
confusionMatrix(y_pred_training_knn, as.factor(training_set$top100)) # ~77% accuracy with k=66
paste("Training Data Accuracy:",round(knn_accuracy_training*100,digits=2),"%")

# Build the confusion matrix & determine accuracy of validation data
knn_cm_validation <- table(validation_set[, 11], y_pred_validation_knn)
knn_accuracy_validation <- (sum(diag(knn_cm_validation))/sum(knn_cm_validation))
confusionMatrix(y_pred_validation_knn, as.factor(validation_set$top100)) # ~78% accuracy with k=66
apaste("Validation Data Accuracy:",round(knn_accuracy_validation*100,digits=2),"%")

# Performance Evaluation of validation data

y_pred_validation_knn <- as.numeric(y_pred_validation_knn)-1 # converting from factor type
pred_knn <- prediction(y_pred_validation_knn, validation_set[,11])
knn_roc <- performance(pred_knn, "tpr","fpr")


#########   3. Support Vector Machine - Linear Kernel  #########

set.seed(1)
split        <- sample.split(spotify_dataset$top100, SplitRatio = 0.7)
training_set <- subset(spotify_dataset, split == TRUE)
validation_set     <- subset(spotify_dataset, split == FALSE)

## Feature Scaling 
training_set[, 1:10] <- scale(training_set[, 1:10])
validation_set[, 1:10] <- scale(validation_set[, 1:10])

# SVM Classifier 
classifier <- svm(formula = top100 ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
?svm

# Test on training data
y_pred_training_svm <- predict(classifier, newdata = training_set[-11])

# Test on validation data
y_pred_validation_svm <- predict(classifier, newdata = validation_set[-11])

# Build the confusion matrix & determine accuracy of training data
svm_cm_training <- table(training_set[, 11], y_pred_training_svm)
svm_accuracy_training <- (sum(diag(svm_cm_training))/sum(svm_cm_training))
confusionMatrix(y_pred_training_svm, as.factor(training_set$top100))
paste("Training Data Accuracy:",round(svm_accuracy_training*100,digits=2),"%") # ~79% accuracy with 0.7 split

# Build the confusion matrix & determine accuracy of validation data
svm_cm_validation <- table(validation_set[, 11], y_pred_validation_svm)
svm_accuracy_validation <- (sum(diag(svm_cm_validation))/sum(svm_cm_validation))
confusionMatrix(y_pred_validation_svm, as.factor(validation_set$top100))
paste("Validation Data Accuracy:",round(svm_accuracy_validation*100,digits=2),"%") # ~80% accuracy with 0.3 split

# Performance Evaluation of validation data
y_pred_validation_svm <- as.numeric(y_pred_validation_svm)-1 # converting from factor type
pred_svm <- prediction(y_pred_validation_svm, validation_set[,11])
svm_roc <- performance(pred_svm, "tpr","fpr")

#########   4. Decision Tree  #########

set.seed(1)

split          <- sample.split(spotify_dataset$top100, SplitRatio = 0.70)
training_set      <- subset(spotify_dataset, split == TRUE)
validation_set     <- subset(spotify_dataset, split == FALSE)

# Fit classifier to Training Set
classifier <- rpart(formula = top100 ~ .,
                   data = training_set,
                   control = rpart.control(minbucket = 50, maxdepth = 5, cp = 0.0001))
?glm
# Test on training data 
y_pred_training_dt <- predict(classifier, newdata = training_set[-11], type='class')

# Test on validation data 
y_pred_validation_dt <- predict(classifier, newdata = validation_set[-11], type='class')

# Build the confusion matrix & determine accuracy of training data
dt_cm_training <- table(training_set[, 11], y_pred_training_dt)
dt_accuracy_training <- (sum(diag(dt_cm_training))/sum(dt_cm_training))
confusionMatrix(y_pred_training_dt, as.factor(training_set$top100))
paste("Training Data Accuracy:",round(dt_accuracy_training*100,digits=2),"%") # ~80% accuracy with 0.7 split

# Build the confusion matrix & determine accuracy of validation data
dt_cm_validation <- table(validation_set[, 11], y_pred_validation_dt)
dt_accuracy_validation <- (sum(diag(dt_cm_validation))/sum(dt_cm_validation))
confusionMatrix(y_pred_validation_dt, as.factor(validation_set$top100))
paste("Validation Data Accuracy:",round(dt_accuracy_validation*100,digits=2),"%") # ~80% accuracy with 0.3 split

#plot(tree classifier)
prp(classifier, type = 1, extra = 1, split.font = 1, varlen = -10)

#Pruned tree
printcp(classifier)
pruned.ct <- prune(classifier, 
                   cp = classifier$cptable[which.min(classifier$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10) 

# Performance Evaluation of validation data
y_pred_validation_dt <- as.numeric(y_pred_validation_dt)-1 # converting from factor type
pred_dt <- prediction(y_pred_validation_dt, validation_set[,11])
dt_roc <- performance(pred_dt, "tpr","fpr")

######### Performance Evaluation by plotting ROC curve ########

plot(lr_roc,
     main = "ROC Curve - Performance Evaluation",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     col=rgb(1,0,0,0.8),
     lwd=2,
     lty=5)
plot(knn_roc,
     add=T,
     col=rgb(0,1,0,0.8),
     lwd=2,
     lty=5)
plot(svm_roc,
     add=T,
     col=rgb(0,0,1,0.8),
     lwd=2,
     lty=5)
plot(dt_roc,
     add=T,
     col=rgb(0.2,0.2,0.2,0.8),
     lwd=2,
     lty=5)
abline(a=0, b=1)
legend(0.4,0.3,
       legend=c("Logistical Regression",
                "k-NN",
                "SVM",
                "Decision Tree"),
       col=c("red", "blue", "green","purple","grey"), 
       pch=15, 
       cex=0.7)

