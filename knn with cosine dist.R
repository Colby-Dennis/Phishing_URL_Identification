library(caret)

## Cosine Distance function
cosine_dist <- function(test_set, train_set){
  install.packages(lsa)
  library(lsa)
  
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

cleanDataset <- read.csv("cleanDataSet.csv")
my_data <-cleanDataset[c(1:100),c(2:32)]

my_data$X.1.16<- as.factor(my_data$X.1.16)
# Splitting data
trainIndex = createDataPartition(my_data[,31], p = 0.7, list = F, times = 1) #p = .7

train_set = my_data[trainIndex,]
test_set = my_data[-trainIndex,]

# Obtain predictors and target variable

x_train<-train_set[,c(1:30)] # Predicting features in training set
y_train<-train_set[,31] # Predicted target in training set

x_test<-test_set[,c(1:30)] # Predicting features in testing set
y_test<-test_set[,31]# Predicted target in testing set

n<-nrow(x_train)
k<-nrow(x_test)

dist_matrix = matrix(0,k,n)

install.packages(FastKNN)
library(FastKNN)

1-(sum(x_train[1,]*x_test[1,]))/sqrt((sum(x_test[1,]*x_test[1,]))*(sum(x_train[1,]*x_train[1,])))
# dist_matrix<-read.csv("Cosine_distance_70_30_Mode.csv")
# dist_matrix<-as.matrix(dist_matrix)
dist_matrix<-cosine_dist(x_test,x_train) # calculate the cosine distance from the each testing record to each training record
# write.csv(dist_matrix,"Cosine_distance_70_30_Mode.csv",row.names = F)# writing the distance calculation to csv file due to long duration of calculation

# knn model with training, testing feature, distance matrix, target variable, and number of neighbors to be considered
pred<-knn_test_function(x_train, x_test, dist_matrix,y_train, k = 2)

confusionMatrix(factor(pred),factor(y_test))

detectCores()
