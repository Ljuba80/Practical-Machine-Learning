# HAR Analysis Using R
ljuba80  
###Syllabus
This report is part of course project in Practical Machine Learning.Data was divided in two parts - Training(70%) and test data (30%). Because of performance
issues, random forest training function applied. No crossvalidation is needed in 
this case because it is performed internally. We are expecting overall accuracy above 95%. Please note that first seven variables can (and will) be ommited. Also additional variables can be ommited (NA in test data for the second part of assignment) - resulting in ~50 features in total

###Data Processing
First we load data

```r
urlTrainData<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv?accessType=DOWNLOAD"
trainData=read.csv(urlTrainData,header=TRUE,na.strings = "NA")
urlTestData<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv?accessType=DOWNLOAD"
testData=read.csv(urlTestData,header=TRUE,na.strings = "NA")
```


###Imputting missing values

As there is number of missing values, we must impute them. Most obvious choice is
to use mean value of appropriate data. Also, factor variables with float levels are transformed to numeric variables

```r
for(i in names(trainData))
{
  missingIdx<-is.na(trainData[,i]) 
  if(class(trainData[,i]) == "integer")
  {
    valToImput<-mean(trainData[,i],na.rm=TRUE)
    trainData[missingIdx,i]<-round(valToImput)
  }
  else if(class(trainData[,i]) == "numeric")
  {
    valToImput<-mean(trainData[,i],na.rm=TRUE)
    trainData[missingIdx,i]<-valToImput
  }
  else if (class(trainData[,i]) == "factor")
  {
    if(length(levels(trainData[,i]))>32)
    {
      trainData[,i]<-as.numeric(trainData[,i])
    }  
  }
}

for(i in names(testData))
{
  missingIdx<-is.na(testData[,i]) 
  if(class(testData[,i]) == "integer")
  {
    valToImput<-mean(testData[,i],na.rm=TRUE)
    testData[missingIdx,i]<-round(valToImput)
  }
  else if(class(testData[,i]) == "numeric")
  {
    valToImput<-mean(testData[,i],na.rm=TRUE)
    testData[missingIdx,i]<-valToImput
  }
  else if (class(testData[,i]) == "factor")
  {
    if(length(levels(testData[,i]))>32)
    {
      testData[,i]<-as.numeric(testData[,i])
    } 
  }  
}
```

Now we partition our (train) dataset in two parts (trained and test part).Also, first seven variables, as well as variables stated as NA's in 20 test data are removed from training set .

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.2
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.2
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
set.seed(32323)
inTrain <- createDataPartition(y = trainData$classe, p = 0.7, list = FALSE)
training <- trainData[inTrain, ]
testing <- trainData[-inTrain, ]
excludedData=c(1:7,12:36,50:59, 69:83,87:101,103:112,125:139,141:150)
p <- training[, -excludedData]
model.treebag <- randomForest(classe ~ ., data = p,ntree=500)
result2 <- predict(model.treebag, newdata = testing)
confusionMatrix(result2, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1671    4    0    0    0
##          B    3 1130    6    0    0
##          C    0    5 1019    8    0
##          D    0    0    1  956    1
##          E    0    0    0    0 1081
## 
## Overall Statistics
##                                         
##                Accuracy : 0.995         
##                  95% CI : (0.993, 0.997)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.994         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.998    0.992    0.993    0.992    0.999
## Specificity             0.999    0.998    0.997    1.000    1.000
## Pos Pred Value          0.998    0.992    0.987    0.998    1.000
## Neg Pred Value          0.999    0.998    0.999    0.998    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.192    0.173    0.162    0.184
## Detection Prevalence    0.285    0.194    0.175    0.163    0.184
## Balanced Accuracy       0.999    0.995    0.995    0.996    1.000
```
