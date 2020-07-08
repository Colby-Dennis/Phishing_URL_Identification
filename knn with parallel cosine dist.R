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


cleanDataset <- read.csv("cleanDataSet.csv")
my_data <-cleanDataset[,c(2:32)]

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

n<-nrow(x_train)
k<-nrow(x_test)

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
max_row<-30
result=matrix(0,max_row,2)

for (k in 1:max_row)
{
  pred<-knn_test_function(x_train, x_test, cosine_sim,y_train, k = k)
  conf_mat<-confusionMatrix(factor(pred),factor(y_test))
  result[k,1]<-k
  result[k,2]<-conf_mat$byClass[['Balanced Accuracy']]
}

result<-data.frame(result)
ggplot(data=result, aes(x=X1, y=X2, group=1)) +
  geom_line(color="red")+
  labs(x="Number of neighbors", y = "Balanced Accuracy")+
  geom_point()