#Wisconsin Diagnostic breast cancer
#Data set is of size 560*32 entries
#Each entry is of type Id,Label and 30 other Features related to pyhsical properties of masses
# http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29


#Reading Data from File
data <- read.csv("wdbc.data",headers=FALSE,stringsAsFactors = FALSE)

#Prune the id column
data <- data[-1]

#Factor the label class.
data$V2 <- factor(data$V2,levels = c("B","M"))

#Normalisze the features
normalize <- function(x){ return ((x-min(x))/max(x)-min(x))) }

#Using list apply to normalize the data of every column
data_norm <- as.data.frame(lapply(data[2:31],normalize))

#Consists of first 450 Elements
train_set <- data_norm[1:450,]

#Last 120 elements for test set
test_set <- data[450:569,1]

#Use the knn to predict the data
data_pred <- knn(train = train_set,test = test_set,cl = test_train_label,k=2)

#Building confusion matrix
CrossTable(x = test__label, y = data_pred,prop.chisq = FALSE)