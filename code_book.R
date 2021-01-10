library(caret)
library(lattice)
library(ggplot2)
library(randomForest)

## Reading the datasets

training<-read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!"))
testing<-read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!"))

dim(training)
table(training$classe)


## Removing all the variables with NA values in the training and testing dataset

NA_Count = sapply(1:dim(training)[2],function(x)sum(is.na(training[,x])))
NA_list = which(NA_Count>0)
colnames(training[,c(1:7)])
training = training[,-NA_list]
training = training[,-c(1:7)]
training$classe = factor(training$classe)

testing = testing[,-NA_list]
testing = testing[,-c(1:7)]

## Modeling with Cross Validation

set.seed(1234)
cv3 = trainControl(method="cv",number=3,allowParallel=TRUE,verboseIter=TRUE)
modrf = train(classe~., data=training, method="rf",trControl=cv3)

library(rpart)

modtree = train(classe~.,data=training,method="rpart",trControl=cv3)
prf=predict(modrf,training)
ptree=predict(modtree,training)
table(prf,training$classe)
table(ptree,training$classe)

prf=predict(modrf,testing)
ptree=predict(modtree,testing)
table(prf,ptree)
## Considering the obtained results, random forest models has the best accuracy for the testing dataset.


## Conclusion

answers=predict(modrf,testing)
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
answers
pml_write_files(answers)
