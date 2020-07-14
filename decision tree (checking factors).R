
mysmalldata_train <- read.csv("3factors_train.csv",header=T) # 89.52%, C.50
mysmalldata_test <- read.csv("3factors_test.csv",header=T)

mysmalldata_train <- read.csv("4factors_train.csv",header=T) # 89.52%
mysmalldata_test <- read.csv("4factors_test.csv",header=T)

mysmalldata_train <- read.csv("5factors_train.csv",header=T) # 90.75%
mysmalldata_test <- read.csv("5factors_test.csv",header=T)

mysmalldata_train <- read.csv("6factors_train.csv",header=T) # 89.52%
mysmalldata_test <- read.csv("6factors_test.csv",header=T)

mysmalldata_train <- read.csv("7factors_train.csv",header=T) # 89.52%
mysmalldata_test <- read.csv("7factors_test.csv",header=T)

mysmalldata_train <- read.csv("8factors_train.csv",header=T) # 89.52%
mysmalldata_test <- read.csv("8factors_test.csv",header=T)

mysmalldata_train <- read.csv("9factors_train.csv",header=T) # 89.52%
mysmalldata_test <- read.csv("9factors_test.csv",header=T)

# Decision Tree
tree.model <- rpart(Result~., data=mysmalldata_train, method="class")
print(tree.model) #shows the data partition percentages and the split attributes
#run the model on the data, print a confusion matrix, and show the accuracy
tree.prediction <- predict(tree.model, newdata=mysmalldata_test, type="class")
confusion.matrix <- table(mysmalldata_test$Result, tree.prediction)
print(confusion.matrix)
#generate the tree accuracy from the confusion matrix
accuracy.percent <- 100*sum(diag(confusion.matrix))/sum(confusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))
print(paste("error rate:",100-accuracy.percent,"%"))

#plot the tree 
plot(tree.model)
text(tree.model, pretty=1)
prettyTree(tree.model)
rpart.plot(tree.model,box.palette="RdBu", shadow.col="gray", nn=TRUE)

library(C50)
mysmalldata_train$Result<-as.factor(mysmalldata_train$Result) ###FIXED THIS LINE
c5tree.model <- C5.0(as.formula(Result~.),data=mysmalldata_train, rules=T) 
str(mysmalldata_train$Result)
print(c5tree.model)
summary(c5tree.model)
#run the model on the data, print a confusion matrix, and show the accuracy
c5tree.prediction <- predict(c5tree.model, newdata=mysmalldata_test)
c5confusion.matrix <- table(mysmalldata_test$Result, c5tree.prediction)
print(c5confusion.matrix)
accuracy.percent <- 100*sum(diag(c5confusion.matrix))/sum(c5confusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))

#plot the tree (have to rerun the model with rules=F)
c5tree.model <- C5.0(as.formula(Result~.), data=mysmalldata_train, rules=F)
plot(c5tree.model)

