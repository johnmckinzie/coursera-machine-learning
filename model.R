pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

set.seed(9991)
library(caret); library(rpart); library(randomForest)

# load data files
trainingData <- read.csv("pml-training.csv", header = TRUE, na.strings=c("","NA"))
testingData <- read.csv("pml-testing.csv", header = TRUE, na.strings=c("","NA"))

# remove metadata and empty/NA columns
trainingData <- trainingData[, -(1:7)]
trainingData <- trainingData[, colSums(is.na(trainingData)) == 0]
testingData <- testingData[, -(1:7)]
testingData <- testingData[, colSums(is.na(testingData)) == 0]

# create training and test sets from training data
inTrain <- createDataPartition(y=trainingData$classe, p=.60, list=FALSE)
trainingSet <- trainingData[inTrain,]
testingSet <- trainingData[-inTrain,]

# rpart
modRPart <- train(classe ~ ., method="rpart", data=trainingSet)
predRPart <- predict(modRPart, newdata=testingSet)
confRPart <- confusionMatrix(predRPart, testingSet$classe)
print(confRPart)

predTestRPart <- predict(modRPart, newdata=testingData)
print(predTestRF)

# random forest
modRF <- randomForest(classe ~ ., data=trainingData)
predRF <- predict(modRF, newdata = testingSet)
confRF <- confusionMatrix(predRF, testingSet$classe)
print(confRF)

predTestRF <- predict(modRF, newdata=testingData)
print(predTestRF)

# random forest w/ pca
modRFPCA <- randomForest(classe ~ ., data=trainingData, preProcess="pca")
predRFPCA <- predict(modRFPCA, newdata = testingSet)
confRFPCA <- confusionMatrix(predRFPCA, testingSet$classe)
print(confRFPCA)

predTestRFPCA <- predict(modRFPCA, newdata=testingData)
print(predTestRF)

# write random forest predictions to file
pml_write_files(predTestRF)
