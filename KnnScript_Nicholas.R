setwd("C:/Users/copel/Documents/GitHub/Phishing_URL_Identification")
library(caret)
library(lsa)
set.seed(700)
## Cosine Distance function
cosine_dist <- function(test_set, train_set){
  
  row_num<-nrow(test_set)
  col_num<-nrow(train_set)
  
  result = matrix(0,row_num,col_num)
  
  for (i in 1:row_num)
  {
    for (j in 1:col_num)
    {
      result[i,j] = 1 - cosine(as.numeric(train_set[j,]),as.numeric(test_set[i,]))
      # result[i,j] = 
    }
  }
  return(result)
} 
# Regular KNN test
mysmalldata_train <- read.delim(file.choose(), header = T, , stringsAsFactor = FALSE, sep = ",")
mysmalldata_test <-  read.delim(file.choose(), header = T, , stringsAsFactor = FALSE, sep = ",")



# KNN Model
mysmalldata_train$Result<-as.factor(mysmalldata_train$Result)
mysmalldata_test$Result<-as.factor(mysmalldata_test$Result)

#x_train<-train_set[,c(2:(cols - 1))]
cols <- ncol(mysmalldata_train)
cols1 <- ncol(mysmalldata_test)
mysmalldata_train <- mysmalldata_train[,c(2:cols)]
#mysmalldata_train
mysmalldata_test < mysmalldata_test[,c(2:cols1)]


grid <- expand.grid(k = c(1:10))
# choose values for K in K-NN
trctl <- trainControl("repeatedcv", number = 10, repeats = 3)
knn_fit_small <- train(Result ~., data = mysmalldata_train, method = "knn",trControl=trctl, tuneGrid=grid)
plot(knn_fit_small)
test_pred <- predict(knn_fit_small, newdata = mysmalldata_test)

confusionMatrix(factor(test_pred), factor(mysmalldata_test$Result))


#cosine distance test


train_set =mysmalldata_train
test_set = mysmalldata_test
  
train_set
test_set

cols <- ncol(train_set)

# Obtain predictors and target variable

x_train<-train_set[,c(2:(cols - 1))] # Predicting features in training set
y_train<-train_set[,cols] # Predicted target in training set

x_test<-test_set[,c(2:cols - 1)] # Predicting features in testing set
y_test<-test_set[,cols]# Predicted target in testing set

x_train

n<-nrow(x_train)
k<-nrow(x_test)


dist_matrix = matrix(0,k,n)

#install.packages(FastKNN)
library(FastKNN)


1-(sum(x_train[1,]*x_test[1,]))/sqrt((sum(x_test[1,]*x_test[1,]))*(sum(x_train[1,]*x_train[1,])))
# dist_matrix<-read.csv("Cosine_distance_70_30_Mode.csv")
# dist_matrix<-as.matrix(dist_matrix)
dist_matrix<-cosine_dist(x_test,x_train) # calculate the cosine distance from the each testing record to each training record
# write.csv(dist_matrix,"Cosine_distance_70_30_Mode.csv",row.names = F)# writing the distance calculation to csv file due to long duration of calculation

# knn model with training, testing feature, distance matrix, target variable, and number of neighbors to be considered
pred<-knn_test_function(x_train, x_test, dist_matrix,y_train, k = 2)

confusionMatrix(factor(pred),factor(y_test))






