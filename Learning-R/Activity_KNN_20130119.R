rm(list=ls(all=TRUE))
setwd("~/work/Insofe/Jan-19/KNN_20140119")
lfdata <- read.csv("leap-frog.csv")
head(lfdata)
str(lfdata)

#Remove NAs!!!
lfdata <- lfdata[!is.na(lfdata$Revenue180),]
#Remove Row.Id column
lfdata <- lfdata[!names(lfdata) %in% c("Row.id")]
rows=seq(1,nrow(lfdata),1)
set.seed(123)
train.rows <- sample(rows,0.7*nrow(lfdata))
test.rows <- rows[-(train.rows)]
data.train <- lfdata[train.rows,]
data.test <- lfdata[test.rows,]

#Some cleanup
rm(rows,test.rows,train.rows,lfdata)

#How many points in test data should be tested
#ntest <- nrow(data.test)
ntest <- 150

#We shall pick 1000 rows randomly from the training data
ntrain <- 1000
train.rows <- c(1:nrow(data.train))


class.var <- c("Revenue180")

result <- matrix(rep(0,ntest*2),ncol=2)
#For each record in test data
for (i.test in 1:ntest) {
  test.rec <- data.test[i.test,]
  dist <- matrix(rep(0,ntrain*2), ncol=2)
  #Pick records randomly from training records
  train.rows.sample <- sample(train.rows,ntrain)
  #Find the distance of test record from each records in training data
  for (i.train in 1:ntrain) {
    train.rec.index <- train.rows.sample[i.train]
    train.rec <- data.train[train.rec.index,]
    
    #Keep class variable out of distance measure
    train.rec <- train.rec[!names(train.rec) %in% class.var]
    test.rec <- test.rec[!names(test.rec) %in% class.var]

    #Store record index and corresponding distance
    dist[i.train,1] <- train.rec.index    
    dist[i.train,2] <- dist(rbind(data.frame(train.rec), test.rec))
  }
  #Find nearest neighbors 
  dist <- dist[order(dist[,2]),]
  #Pick top 100 nearest neighbors
  dist <- dist[1:k,]
  #Generate a Linear Regression Model using Trng data
  x <- data.frame(data.train[dist[,1],]) 
  lrmodel <- lm(Revenue180 ~ ., x)
  x <- data.frame(test.rec)
  pred.val <- predict(lrmodel,x)
  actual.val <- data.test[i.test,]$Revenue180
  cat("Outcome of test", i.test, "-> Predicted: ", pred.val, ", Actual: ", actual.val, "\n")
  result[i.test,] <- c(pred.val,actual.val)
}

#Print results
matrix(sprintf("%2.5f",result[1:ntest,]),ncol=2)
#Print mse
preds <- result[,1]
actuals <- result[,2]
mse <- mean((preds-actuals)^2)
mse
