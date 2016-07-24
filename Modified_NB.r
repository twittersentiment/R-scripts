#Improving the performance of Naive Bayes
#Performing the feature selection through Decision Tree C5.0
#Mushroom Data set from UCI Repository
#8124 * 23 is modified to 8124 * 21
#Column 7 and 17 are eliminated since they are degeneratively distributed
#Target class i column 1

NB   <- function(data,split_ratio,class.pos,features){
  total.length <- dim(data)[1]
  rand.rec <- c()
  tot.rec <- c()
  num.train.rec <- ceiling(total.length*split_ratio)
  #Sampling the data
  rand.rec <- sample(1:total.length,num.train.rec)
  tot.rec <- seq(from = 1,to = total.length,by = 1)
  rem.rec <- setdiff(tot.rec,rand.rec)
  x.class.pos <- class.pos*-1
  #Training the data
  if(missing(features)){
    train.data <- data[rand.rec, x.class.pos]
    test.data <- data[rem.rec,x.class.pos]
  }else{
    train.data <- data[rand.rec, features]
    test.data <- data[rem.rec,features]
  }
  train.class <- data[rand.rec, class.pos]
  nb.model <- naiveBayes(train.data,train.class,laplace = 1)
  test.class <- data[rem.rec,class.pos]
  nb.pred <- predict(nb.model,test.data)
  cm <- confusionMatrix(nb.pred,test.class)
  #Return accuracy of the model
  return(cm$overall['Accuracy']*100)
}

C5   <- function(data,ratio,class.pos){
  num.rec    <- dim(data)[1]
  rand.rec   <- c()
  tot.rec    <- c()
  num.train.rec <- ceiling(num.rec*ratio)
  #Sampling the data
  rand.rec   <- sample(1:num.rec,num.train.rec)
  tot.rec    <- seq(from = 1,to = num.rec,by = 1)
  rem.rec    <- setdiff(tot.rec,rand.rec)
  ex.class   <- class.pos*-1
  #Training set
  train.data <- data[rand.rec, ex.class]
  train.class<- data[rand.rec, class.pos]
  #Using C5 model
  C5.model   <- C5.0(train.data,train.class)
  C5Predc    <- predictors(C5.model)
  #test.data  <- data[rem.rec,ex.class]
  #test.class <- data[rem.rec,class.pos]
  #C5.predct  <- predict(C5.model,test.data)
  #cm         <- confusionMatrix(C5.predct,test.class)
  #Return  atmost 7 attributes
  return(C5Predc[0:7])
}

Modfd.NaiveBayes <- function(){
  #Load the required libraries
  library('C50')
  library('e1071')
  library('gmodels')
  #Initialize
  Trainset <- 10
  Iterate <- 5
  # On the 10% training set for 5 times.Select atmost 7 attributes used by DT-C5.0
  train.split <- rep.int(Trainset,Iterate)
  target.class <- 1
  features <- c()
  NB.accur <- c()
  NNB.accur <- c()
  #Load the data from UCI repository
  mushroom.f <-file("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", open="r") 
  data <- read.csv(mushroom.f, header=F)
  #Exclude columns 7 and 17. It is degenerate data
  data <- data[,c(-7,-17)]
  #Gathering the features for decision tree
  for(split in train.split){
    feature <- C5(data,split/100,target.class)
    feature <- feature[!is.na(feature)]
    features <- union(features,feature)
  }
  #Splitting the data into train and test. Starting from 10
  train.split <- seq(from = 10, to = 90,by = 10)
  for(split in train.split){
    #Naive bayes Accuracy
    NB.accur  <- c(NB.accur,NB(data,split/100,target.class))
    #New Naive bayes with feature selection through Decision Tree
    NNB.accur <- c(NNB.accur,NB(data,split/100,target.class,features = features))
  }
  #Plotting
  xrange = range(10:100)
  yrange = range(90:100)
  plot(xrange,yrange,type="n",xlab="Training (%)",ylab = "Accuracy (%)",main = "Mushroom")
  lines(train.split,NB.accur,lty=2,lwd=3,type="o",col="red")
  lines(train.split,NNB.accur,lty=1,lwd=3,type="o",col="blue")
  legend(min(xrange),max(yrange),legend = c("NB","NNB"),lty = c(2,1),col=c("red","blue"))
}

#Main Method.
Modfd.NaiveBayes()
