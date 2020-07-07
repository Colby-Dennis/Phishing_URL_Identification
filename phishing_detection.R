# setwd('/home/colby/Documents/GitHub/Phishing_URL_Identification/')
# setwd("C:/Users/jhern/Desktop/PROJECT/Phishing_URL_Identification")
# to comment out a chunk ctrl+shift+c

# Required Packages
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("neuralnet")
# install.packages("caret")
# install.packages("corrplot")
# install.packages("microbenchmark")
library(ggplot2)
library(gridExtra)
library(neuralnet)
library(caret)
library(rpart); library(rpart.plot)
library(DMwR)
library(corrplot)
library(microbenchmark)

#Loading in the data
mysmalldata <- read.csv("https://raw.githubusercontent.com/PhysikerWT/Phishing_URL_Identification/master/rawDataSetSmall.csv")
myrawdata <- read.csv("https://raw.githubusercontent.com/PhysikerWT/Phishing_URL_Identification/master/rawDataSet.csv", stringsAsFactors = F)

# Renaming data according to provided paper.
data_names <- c("id","having_IP_address","URL_Length","Shortining_Service","having_At_Symbol",
                "double_slash_redirecting","Prefix_Suffix","having_Sub_Domain","SSLfinal_State",
                "Domain_registration_length","Favicon","Port","HTTPS_token","Request_URL",
                "URL_of_Anchor","Links_in_tags","SFH","Submitting_to_email","Abnormal_URL",
                "Redirect","on_mouseover","RightClick","popUpWindow","Iframe","age_of_domain",
                "DNSRecord","web_traffic","Page_Rank","Google_Index","Links_pointing_to_page",
                "Statistical_report","Result")

names(mysmalldata) <- data_names
names(myrawdata) <- data_names

# Creating functions to clean the data.

# Function that removes all non-existance result rows
clean_results <- function(df) {
  does_exist <- c() # Initialize with information
  i <- 1
  while (i <= (nrow(df))) {
    if (is.na(df[i,ncol(df)])) {
      does_exist[i] <- F
    } else {
      does_exist[i] <- T
    }
    i <- i + 1
  }
  return(df[does_exist,])
}

# Clean w/ df$school[is.na(df$school)] <- "houston"

# Function that replaces all NA's with zeros
replace_zero <- function(df) {
  i <- 2
  while (i <= (ncol(df)-1)) {
    j <- 1
    while (j<= nrow(df[i])) {
      if (is.na(df[[i]][j])) {
        df[[i]][j] <- 0
      }
      j <- j + 1
    }
    i <- i + 1
  }
  return(df)
}

# Parallelized replace NA w/ zero
replace_zero_new <- function(df) {
  i <- 2
  while (i <= (ncol(df)-1)) {
    df[[i]][is.na(df[[i]])] <- 0
    i <- i + 1
  }
  return(df)
}

# Function that replaces the unknown with an average.
replace_average <- function(df) {
  i <- 2
  while (i <= (ncol(df)-1)) {
    j <- 1
    average = mean(df[,i], na.rm=T)
    while (j<= nrow(df[i])) {
      if (is.na(df[[i]][j])) {
        df[[i]][j] <- average
      }
      j <- j + 1
    }
    i <- i + 1
  }
  return(df)
}

#Function that replaces all NAs with a proportional (yet random) distribution of -1 or 1 based on known values

replace_distribution <- function(df) {
  for (i in 2:(ncol(df)-1)) {
    uniq1 <- c(0,-1, 1)
    cnt <- tabulate(match(df[,i], uniq1))
    prop <- cnt[2] / cnt[3]
    for (j in 1:nrow(df)){
      if(is.na(df[j,i])){
        df[j,i] <-ifelse(runif(1)<=prop,-1,1)
      }
    }
  }
  return(df)
}

# Function that replaces the unknown with the most often
replace_mode <- function(df) {
  i <- 2
  while (i <= (ncol(df)-1)) {
    j <- 1
    value_matrix <- matrix(table(df[,i]))
    value <- -1
    if (nrow(value_matrix) == 3) {
      if (value_matrix[1] > value_matrix[2] && value_matrix[1] > value_matrix[3]) {
        value <- -1
      } else if (value_matrix[2] > value_matrix[3]) {
        value <- 0
      } else {
        value <- 1
      }
    } else {
      if (value_matrix[1] > value_matrix[2]) {
        value <- -1
      } else {
        value <- 1
      }
    }
    while (j<= nrow(df[i])) {
      if (is.na(df[[i]][j])) {
        df[[i]][j] <- value
      }
      j <- j + 1
    }
    i <- i + 1
  }
  return(df)
}

#Randomly generates either a -1,0,1
set0<-c(-1,0,1)
replace_random_withzero <- function(df) {
  i <- 2
  while (i <= (ncol(df)-1)) {
    j <- 1
    while (j<= nrow(df[i])) {
      if (is.na(df[[i]][j])) {
        value<-sample(set0,1)
        df[[i]][j] <- value
      }
      j <- j + 1
    }
    i <- i + 1
  }
  return(df)
}

# A function to covert predictions to binary result
get_prediction <- function(mod) {
  result_vec <- integer(nrow(mod))
  i <- 1
  while (i <= nrow(mod)) {
    if (mod[i] >= 0) {
      result_vec[i] <- 1
    } else {
      result_vec[i] <- -1
    }
    i <- i + 1
  }
  return(result_vec)
}

# Using a low ranking value to determine what 1 and -1 represents.
# hist(mysmalldata$having_At_Symbol, main="Having @ symbol")
# 1 represents phishing, -1 represents ligitimant and 0 represents suspicous.

set.seed(700)

# Getting datasets
small_cleaned_results <- clean_results(mysmalldata)
raw_cleaned_results <- clean_results(myrawdata)
# small_cleaned_zeros <- replace_zero(small_cleaned_results)
# small_cleaned_average <- replace_average(small_cleaned_results)

# Checking code speed
# old code
start_time <- Sys.time()
test_1 <- replace_zero(small_cleaned_results)
end_time <- Sys.time()
test_1_time <-end_time-start_time
print(test_1_time)

# new code
start_time <- Sys.time()
test_2 <- replace_zero_new(small_cleaned_results)
end_time <- Sys.time()
test_2_time <- end_time-start_time
print(test_2_time)

# Partition Data
partitioned_small <- createDataPartition(y = small_cleaned_results$Result, p= 0.3, list = FALSE)

mysmalldata_train_b <- small_cleaned_results[partitioned_small,]
mysmalldata_test_b <- small_cleaned_results[-partitioned_small,]

partitioned_raw <- createDataPartition(y = raw_cleaned_results$Result, p= 0.3, list = FALSE)

myrawdata_train_b <- raw_cleaned_results[partitioned_raw,]
myrawdata_test_b <- raw_cleaned_results[-partitioned_raw,]

# Check Partition
#two-sample z-test on small data (mysmalldata_test , mysmalldata_train)
p1 <- sum(mysmalldata_train_b$Result=="-1")/nrow(mysmalldata_train_b)
p2 <- sum(mysmalldata_test_b$Result=="-1")/nrow(mysmalldata_test_b)
p_pooled <- (sum(mysmalldata_train_b$Result=="-1") +
               sum(mysmalldata_test_b$Result=="-1"))/
  (nrow(mysmalldata_train_b) + nrow(mysmalldata_test_b))
z <- (p1 - p2)/sqrt(p_pooled*(1-p_pooled) *
                      (1/nrow(mysmalldata_train_b) + 1/nrow(mysmalldata_test_b)))
z.p <- 2*pnorm(-abs(z))

#two-sample z-test on raw data (myrawdata_test, myrawdata_train)
p1 <- sum(myrawdata_train_b$Result=="-1")/nrow(myrawdata_train_b)
p2 <- sum(myrawdata_test_b$Result=="-1")/nrow(myrawdata_test_b)
p_pooled <- (sum(myrawdata_train_b$Result=="-1") +
               sum(myrawdata_test_b$Result=="-1"))/
  (nrow(myrawdata_train_b) + nrow(myrawdata_test_b))
z <- (p1 - p2)/sqrt(p_pooled*(1-p_pooled) *
                      (1/nrow(myrawdata_train_b) + 1/nrow(myrawdata_test_b)))
z.p <- 2*pnorm(-abs(z))


# Cleaning functions 

#replace with Zeros
mysmalldata_train <- replace_zero(mysmalldata_train_b)
mysmalldata_test <- replace_zero(mysmalldata_test_b)
myrawdata_train <- replace_zero(myrawdata_train_b)
myrawdata_test <- replace_zero(myrawdata_test_b)
# #replace with random -1 or 1
# mysmalldata_train <- replace_random_withzero(mysmalldata_train_b)
# mysmalldata_test <- replace_random_withzero(mysmalldata_test_b)
# myrawdata_train <- replace_random_withzero(myrawdata_train_b)
# myrawdata_test <- replace_random_withzero(myrawdata_test_b)
# #replace with mode
# mysmalldata_train <- replace_mode(mysmalldata_train_b)
# mysmalldata_test <- replace_mode(mysmalldata_test_b)
# myrawdata_train <- replace_mode(myrawdata_train_b)
# myrawdata_test <- replace_mode(myrawdata_test_b)
# #replace with distributive -1 and 1
# mysmalldata_train <- replace_distribution(mysmalldata_train_b)
# mysmalldata_test <- replace_distribution(mysmalldata_test_b)
# myrawdata_train <- replace_distribution(myrawdata_train_b)
# myrawdata_test <- replace_distribution(myrawdata_test_b)

# linear base model
baseline_model<-lm(Result~.,mysmalldata_train) 
pred<-predict(baseline_model,mysmalldata_test) 
i <- 1
lin_pred <- integer(length(pred))
while (i <= length(pred)) {
  if (pred[[i]] >= 0) {
    lin_pred[i] <- 1
  } else {
    lin_pred[i] <- -1
  }
  i <- i + 1
}

lin_pred_conf <- confusionMatrix(factor(lin_pred), factor(mysmalldata_test$Result), mode="everything")
print(lin_pred_conf)

# Decision Tree for small data 
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

# Decision Tree for raw data 
tree.model <- rpart(Result~., data=myrawdata_train, method="class")
print(tree.model) #shows the data partition percentages and the split attributes
#run the model on the data, print a confusion matrix, and show the accuracy
tree.prediction <- predict(tree.model, newdata=myrawdata_test, type="class")
confusion.matrix <- table(myrawdata_test$Result, tree.prediction)
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
  
# KNN Model
mysmalldata_train$Result<-as.factor(mysmalldata_train$Result)
mysmalldata_test$Result<-as.factor(mysmalldata_test$Result)


grid <- expand.grid(k = c(1:10))
# choose values for K in K-NN
trctl <- trainControl("repeatedcv", number = 10, repeats = 3)
knn_fit_small <- train(Result ~., data = mysmalldata_train, method = "knn",trControl=trctl, tuneGrid=grid)
plot(knn_fit_small)
test_pred <- predict(knn_fit_small, newdata = mysmalldata_test)
test_pred <- ifelse(test_pred>0, 1, -1)
confusionMatrix(factor(test_pred), factor(mysmalldata_test$Result))

# Simple Perception
simp_perc_small<-neuralnet(Result~having_IP_address+URL_Length+Shortining_Service+having_At_Symbol+
                           double_slash_redirecting+Prefix_Suffix+having_Sub_Domain+SSLfinal_State+
                           Domain_registration_length+Favicon+Port+HTTPS_token+Request_URL+
                           URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+
                           Redirect+on_mouseover+RightClick+popUpWindow+Iframe+age_of_domain+
                           DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+
                           Statistical_report, mysmalldata_train,hidden=c(1))

plot(simp_perc_small, main="Simple Perceptron w/ NA's replaced with zeros")

simp_perc_small_model<-predict(simp_perc_small,newdata = mysmalldata_test) 
simp_perc_small_result <- get_prediction(simp_perc_small_model)
simp_perc_small_conf <- confusionMatrix(factor(simp_perc_small_result), factor(mysmalldata_test$Result), mode="everything")
print(simp_perc_small_conf)
  
# Some activation functions if needed
softplus <- function(x) log(1+exp(x))
sigmoid <- function(x) 1/(1+exp(-x))
gauss <- function(x) exp(-(x*x)/2)
  
# 1 hidden layer, 2 nodes neural network

nn_1_2_small<-neuralnet(Result~having_IP_address+URL_Length+Shortining_Service+having_At_Symbol+
                             double_slash_redirecting+Prefix_Suffix+having_Sub_Domain+SSLfinal_State+
                             Domain_registration_length+Favicon+Port+HTTPS_token+Request_URL+
                             URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+
                             Redirect+on_mouseover+RightClick+popUpWindow+Iframe+age_of_domain+
                             DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+
                             Statistical_report, mysmalldata_train,hidden=c(2))

plot(nn_1_2_small)

nn_1_2_small_model<-predict(nn_1_2_small,newdata = mysmalldata_test) 
nn_1_2_small_result <- get_prediction(nn_1_2_small_model)
nn_1_2_small_conf <- confusionMatrix(factor(nn_1_2_small_result), factor(mysmalldata_test$Result), mode="everything")
print(nn_1_2_small_conf)

# 1 hidden layer, 3 nodes neural network

nn_1_3_small<-neuralnet(Result~having_IP_address+URL_Length+Shortining_Service+having_At_Symbol+
                          double_slash_redirecting+Prefix_Suffix+having_Sub_Domain+SSLfinal_State+
                          Domain_registration_length+Favicon+Port+HTTPS_token+Request_URL+
                          URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+
                          Redirect+on_mouseover+RightClick+popUpWindow+Iframe+age_of_domain+
                          DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+
                          Statistical_report, mysmalldata_train,hidden=c(3))

plot(nn_1_3_small)

nn_1_3_small_model<-predict(nn_1_3_small,newdata = mysmalldata_test) 
nn_1_3_small_result <- get_prediction(nn_1_3_small_model)
nn_1_3_small_conf <- confusionMatrix(factor(nn_1_3_small_result), factor(mysmalldata_test$Result), mode="everything")
print(nn_1_3_small_conf)

# 1 hidden layer, 4 nodes neural network

nn_1_4_small<-neuralnet(Result~having_IP_address+URL_Length+Shortining_Service+having_At_Symbol+
                          double_slash_redirecting+Prefix_Suffix+having_Sub_Domain+SSLfinal_State+
                          Domain_registration_length+Favicon+Port+HTTPS_token+Request_URL+
                          URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+
                          Redirect+on_mouseover+RightClick+popUpWindow+Iframe+age_of_domain+
                          DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+
                          Statistical_report, mysmalldata_train,hidden=c(4))

plot(nn_1_4_small)

nn_1_4_small_model<-predict(nn_1_4_small,newdata = mysmalldata_test) 
nn_1_4_small_result <- get_prediction(nn_1_4_small_model)
nn_1_4_small_conf <- confusionMatrix(factor(nn_1_4_small_result), factor(mysmalldata_test$Result), mode="everything")
print(nn_1_4_small_conf)

# Plotting information
# p1 <- ggplot(small_cleaned_zeros, aes(x=Request_URL, color=factor(Result), fill=factor(Result))) +
#   geom_histogram(bins=3) +
#   ggtitle("Are Objects like images loaded from the same domain?")
# 
# p2 <- ggplot(small_cleaned_zeros, aes(x=age_of_domain, color=factor(Result), fill=factor(Result))) +
#   geom_histogram(bins=3) +
#   ggtitle("Is the age of the domain greater than 2 years?")
# 
# p3 <- ggplot(small_cleaned_zeros, aes(x=SSLfinal_State, color=factor(Result), fill=factor(Result))) +
#   geom_histogram(bins=3) +
#   ggtitle("Is the final state HTTPS?")
# 
# p4 <-ggplot(small_cleaned_zeros, aes(x=Statistical_report, color=factor(Result), fill=factor(Result))) +
#   geom_histogram(bins=3) +
#   ggtitle("Is the website rank less than 100,000")
# 
# grid.arrange(p1,p2,p3,p4, nrow=2)

#Correlation Matrix (comparing 4 characteristics from above "Request URL","age-of-domain","SSL","Statistical Report")
X <- mysmalldata_train[,c(9,14,25,31)]
#normalize the predictor variables
X_z <- as.data.frame(scale(X))
#find the correlation of the predictor variables
a<-round(cor(X_z),3)
plot(a)
plot(as.data.frame(a))
heatmap(a)

#Correlation Matrix (Comparing all characteristics)
Y <- mysmalldata_train[,c(1:32)]
#normalize the predictor variables
Y_z <- as.data.frame(scale(Y))
#find the correlation of the predictor variables
b<-round(cor(Y_z),3)
plot(b)
plot(as.data.frame(b)) #MUST ZOOM IN => HARD TO SEE
heatmap(b)

#Correlation Matrix (Comparing first half)
G <- mysmalldata_train[,c(2:15, 32)]
#normalize the predictor variables
G_z <- as.data.frame(scale(G))
#find the correlation of the predictor variables
c<-round(cor(G_z),3)
plot(c)
plot(as.data.frame(c)) #MUST ZOOM IN => HARD TO SEE
heatmap(c)

#Correlation Matrix (Comparing second half)
H <- mysmalldata_train[,c(16:32)]
#normalize the predictor variables
H_z <- as.data.frame(scale(H))
#find the correlation of the predictor variables
d<-round(cor(H_z),3)
plot(d)
plot(as.data.frame(d)) #MUST ZOOM IN => HARD TO SEE
heatmap(d)


#Correlation Matrix - Raw data (Comparing all characteristics)

df <- replace_distribution(raw_cleaned_results)
rawSet <- df[,c(1:32)]
summary(rawSet)
#normalize the predictor variables
rawSet_z <- as.data.frame(scale(rawSet))
#find the correlation of the predictor variables
k<-round(cor(rawSet_z),3)
plot(k)
plot(as.data.frame(k)) #MUST ZOOM IN => HARD TO SEE
heatmap(k)


corrplot(k, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Creating a reduced model for the browser extension
fox_labels <- c("URL_Length","having_At_Symbol","double_slash_redirecting",
                "Prefix_Suffix","having_Sub_Domain","Result")

fox_train <- myrawdata_train[,fox_labels]
fox_test <- myrawdata_test[,fox_labels]

fox_baseline_model<-lm(Result~.,fox_train) 
fox_pred<-predict(fox_baseline_model,fox_test) 
i <- 1
fox_lin_pred <- integer(length(fox_pred))
while (i <= length(fox_pred)) {
  if (fox_pred[[i]] >= 0) {
    fox_lin_pred[i] <- 1
  } else {
    fox_lin_pred[i] <- -1
  }
  i <- i + 1
}

fox_lin_pred_conf <- confusionMatrix(factor(fox_lin_pred), factor(fox_test$Result), mode="everything")
print(fox_lin_pred_conf)

# Accuracy was 69.94%

# Decision Tree for small data 
fox_tree.model <- rpart(Result~., data=fox_train, method="class")
print(fox_tree.model) #shows the data partition percentages and the split attributes
#run the model on the data, print a confusion matrix, and show the accuracy
fox_tree.prediction <- predict(fox_tree.model, newdata=fox_test, type="class")
fox_confusion.matrix <- table(fox_test$Result, fox_tree.prediction)
print(fox_confusion.matrix)
#generate the tree accuracy from the confusion matrix
fox_accuracy.percent <- 100*sum(diag(fox_confusion.matrix))/sum(fox_confusion.matrix)
print(paste("accuracy:",fox_accuracy.percent,"%"))
print(paste("error rate:",100-fox_accuracy.percent,"%"))

#plot the tree 
plot(fox_tree.model)
text(fox_tree.model, pretty=1)
prettyTree(fox_tree.model)
rpart.plot(fox_tree.model,box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Accuracy was 71.97%

# Simple Perception
fox_simp_perc_small<-neuralnet(Result~URL_Length+having_At_Symbol+
                                 double_slash_redirecting+Prefix_Suffix+
                                 having_Sub_Domain, fox_train,hidden=c(1))

plot(fox_simp_perc_small, main="Simple Perceptron w/ NA's replaced with zeros")

fox_simp_perc_small_model<-predict(fox_simp_perc_small,newdata = fox_test) 
fox_simp_perc_small_result <- get_prediction(fox_simp_perc_small_model)
fox_simp_perc_small_conf <- confusionMatrix(factor(fox_simp_perc_small_result), factor(fox_test$Result), mode="everything")
print(fox_simp_perc_small_conf)

# Accuracy was 71.85%

# nn 1 layer 2 nodes
fox_nn_2nodes<-neuralnet(Result~URL_Length+having_At_Symbol+
                           double_slash_redirecting+Prefix_Suffix+
                           having_Sub_Domain, fox_train,hidden=c(2))

plot(fox_nn_2nodes, main="Simple Perceptron w/ NA's replaced with zeros")

fox_nn_2nodes_model<-predict(fox_nn_2nodes,newdata = fox_test) 
fox_nn_2nodes_result <- get_prediction(fox_nn_2nodes_model)
fox_nn_2nodes_conf <- confusionMatrix(factor(fox_nn_2nodes_result), factor(fox_test$Result), mode="everything")
print(fox_nn_2nodes_conf)

# Accuracy was 71.7%

# nn 1 layer 3 nodes
fox_nn_3nodes<-neuralnet(Result~URL_Length+having_At_Symbol+
                           double_slash_redirecting+Prefix_Suffix+
                           having_Sub_Domain, fox_train,hidden=c(3))

plot(fox_nn_3nodes, main="Simple Perceptron w/ NA's replaced with zeros")

fox_nn_3nodes_model<-predict(fox_nn_3nodes,newdata = fox_test) 
fox_nn_3nodes_result <- get_prediction(fox_nn_3nodes_model)
fox_nn_3nodes_conf <- confusionMatrix(factor(fox_nn_3nodes_result), factor(fox_test$Result), mode="everything")
print(fox_nn_3nodes_conf)

# Accuracy was 71.94%

# nn 1 layer 4 nodes
fox_nn_4nodes<-neuralnet(Result~URL_Length+having_At_Symbol+
                           double_slash_redirecting+Prefix_Suffix+
                           having_Sub_Domain, fox_train,hidden=c(4))

plot(fox_nn_4nodes, main="Simple Perceptron w/ NA's replaced with zeros")

fox_nn_4nodes_model<-predict(fox_nn_4nodes,newdata = fox_test) 
fox_nn_4nodes_result <- get_prediction(fox_nn_4nodes_model)
fox_nn_4nodes_conf <- confusionMatrix(factor(fox_nn_4nodes_result), factor(fox_test$Result), mode="everything")
print(fox_nn_4nodes_conf)

# DID NOT CONVERGE WITHING STEPMAX
