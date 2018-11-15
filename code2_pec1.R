############## PEC 1 MACHINE LEARNING ################### Rubén Sánchez Fernández

################################################################################
#importing data
setwd("C:/Users/ruben/OneDrive/Escritorio/MASTER/Semestre 2/MACHINE LEARNING/PEC 1")
data <- read.csv("promoterDataSet.csv", header=F)

#naming features in data (for convinience)
colnames(data) <- c("promoter","name","sequence")

#data overview
head(data)

str(data)

##################################################################################

#loading one-hot encoding function from script
source("one_hot_function.R")

#encoding dna sequences
enc_seq <- one_hot(data, "sequence")

####################################################################################

#We will work with the encoded sequences saved as [106,228]: 1 full sequence per row
enc_seq_df <- enc_seq$`Dataframe encoded sequences` 

#randomly separating data into training and test sets
# 67% training
size <- floor(0.67 * nrow(data))

#set seed to assure reproducibility
set.seed(123)

#training indices
trn <- sample(seq_len(nrow(data)), size = size)

#knn function requires data to be in matrix or data frame form,
#therefore we will work with the encoded sequences saved as [106,228]: 1 full sequence per row
train <- enc_seq$`Dataframe encoded sequences`[trn,]

train_labels <- data[trn,1] #training labels

#test sequences
test <- enc_seq$`Dataframe encoded sequences`[-trn,]

test_labels <- data[-trn,1] #test labels

######################################################################################

library(class)
#k = 3
prediction<-knn(train=train, test=test, cl=train_labels, k=3) 

#confusion matrix
library(caret)
confusionMatrix(prediction, test_labels, positive = "+")

#k = 5
prediction<-knn(train=train, test=test, cl=train_labels, k=5) 
#confusion matrix
confusionMatrix(prediction, test_labels, positive = "+")

#k = 7
prediction<-knn(train=train, test=test, cl=train_labels, k=7) 
#confusion matrix
confusionMatrix(prediction, test_labels, positive = "+")

#k = 11
prediction<-knn(train=train, test=test, cl=train_labels, k=11) 
#confusion matrix
confusionMatrix(prediction, test_labels, positive = "+")

