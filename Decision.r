#Statlog German loan report
#Data set is of size 1000*21 entries V1,V2,V3...V21
#Each entry's  target class if located at the end.  V21 is the target class
#https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/


#Load the libraries 
library("C50")
library("gmodels")

#Change the working directory
setwd("C:/Users/Temp/Desktop/Data")
getwd()

#Read the data file
credit <- read.csv("german.data",sep=" ",stringsAsFactors = FALSE,header = FALSE) 

#Summary of Good and Bad loans 70-30
table(credit$V21)

#Random sample of given 1000 observations
credit_ran <- credit[sample(1:1000,1000),]

#Splitting the data int 90-10 ration for Training and Test classes
credit_ran_train <- credit_ran[1:900,1:20]
train_class <- credit_ran[1:900,21]

#Factor the target class
train_class <- as.factor(train_clas)

test_data <- credit_ran[901:1000,1:20]
test_class <- credit_ran[901:1000,21]

#Model C5.0 with boosting of 5 Trees
credit_model <- C5.0(credit_ran_train,train_class,trials = 5)
predict_class <- predict(credit_model,test_data)

#Will give you the summary of decision tree model . like num of Trees , each decision tree
summary(credit_model)

#Confusion matrix 
CrossTable(train_class,test_class,prop.chisq = FALSE,prop.r = FALSE,prop.t = FALSE)

