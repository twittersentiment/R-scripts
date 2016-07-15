#Wisconsin Diagnostic breast cancer
#Data set is of size 560*32 entries
#Each entry is of type Id,Label and 30 other Features related to pyhsical properties of masses
# http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29

library('gmodels')
library('e1071')

#Reading Data from File
data <- read.csv("wdbc.data",header = FALSE,stringsAsFactors = FALSE)

#Prune the id column
data <- data[-1]

#Factor the label class.
data$V2 <- factor(data$V2,levels = c("B","M"))

#Normalisze the features for KNN
normalize <- function(x){ return ((x-min(x))/max(x)-min(x))) }

#Using list apply to normalize the data of every column.
#We are removing target class.
data_norm <- as.data.frame(lapply(data[2:31],normalize))

#Consists of first 450 Elements
train_set <- data_norm[1:450,]

#Last 120 elements for test set
test_set <- data_norm[450:569,]

#Extract the train and test set labels
train_label <- data[1:450,1]
test_label <- data[450:569,1]

#For Naive Bayes
data.train <- data[1:450,]
data.test <- data[451:569,]
train.label <- data.train[,1]
test.label <- data.test[,1]



#Use the knn to predict the data
knn.pred <- knn(train = train_set,test = test_set,cl = test_label,k=2)
#For Naive Bayes
nb <- naiveBayes(data.train,train.label)
nb.pred <- predict(nb,data.test)


#Building CROSS Table
CrossTable(test_label,knn.pred,prop.r = FALSE,prop.t = FALSE,prop.chisq = FALSE,prop.c = FALSE)
#NB
CrossTable(test.label,nb.pred,prop.r = FALSE,prop.t = FALSE,prop.chisq = FALSE,prop.c = FALSE)