library(caret)
library(FastKNN)
library(parallel)
library(lsa)
library(ggplot2)

## Parallel Cosine Distance function
cosine.par <- function(cl, vecA, matB) {
  r <- parApply(cl, matB, 2, cosine, vecA)
  dim(r) <- c(length(r), 1)
  r
}
## Grid search function oof KNN
grid_search_knn<-function(min_iter,max_iter,train_feature,test_feature,train_label,dist_matrix)
{
  max_row<-max_iter-min_iter+1
  result=matrix(0,max_row,2)
  row_count<-1
  
  for (k in min_iter:max_iter)
  {
    pred<-knn_test_function(train_feature, test_feature, dist_matrix,train_label, k = k)
    conf_mat<-confusionMatrix(factor(pred),factor(y_test))
    result[row_count,1]<-k
    result[row_count,2]<-conf_mat$byClass[['Balanced Accuracy']]
    row_count<-row_count+1
  }
  return(result)
}


## Main Script
my_data <- read.csv("cleanDataSet.csv")[,c(2:32)]
my_data$X.1.16<- as.factor(my_data$X.1.16)
# Splitting data
set.seed(700)
trainIndex = createDataPartition(my_data[,31], p = 0.7, list = F, times = 1) #p = .7

train_set = my_data[trainIndex,]
test_set = my_data[-trainIndex,]

# Obtain predictors and target variable

x_train<-train_set[,c(1:30)] # Predicting features in training set
y_train<-train_set[,31] # Predicted target in training set

x_test<-test_set[,c(1:30)] # Predicting features in testing set
y_test<-test_set[,31]# Predicted target in testing set

# dist_matrix<-read.csv("Cosine_distance_70_30_Mode.csv")
# dist_matrix<-as.matrix(dist_matrix)

# calculate the cosine distance from the each testing record to each training record
system.time({
  nc <- detectCores()
  cl <- makeCluster(rep("localhost", nc))
  cosine_sim=matrix(0,nrow(x_train),nrow(x_test))
  unit_mat=c(rep(1,nrow(x_train)))
  
  for (i in 1:nrow(x_test))
  {
    col_result<-c()
    sample_vec<-unname(unlist(x_test[i,]))
    sample_mat<-t(as.matrix(x_train))
    col_result<-cosine.par(cl,sample_vec,sample_mat)
    cosine_sim[,i]<-unit_mat-col_result
  }
  stopCluster(cl)
  cosine_sim<-t(cosine_sim)
})

# write.csv(dist_matrix,"Cosine_distance_70_30_Mode.csv",row.names = F)# writing the distance calculation to csv file due to long duration of calculation

# knn model with training, testing feature, distance matrix, target variable, and number of neighbors to be considered
search_result<-data.frame(grid_search_knn(1,25,x_train,x_test,y_train,cosine_sim))
ggplot(data=search_result, aes(x=X1, y=X2, group=1)) +
  geom_line(color="red")+
  labs(x="Number of neighbors", y = "Balanced Accuracy")+
  geom_point()
