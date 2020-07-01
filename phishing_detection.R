# setwd('/home/colby/Documents/GitHub/Phishing_URL_Identification/')
# setwd("C:/Users/jhern/Desktop/PROJECT/Phishing_URL_Identification")
# to comment out a chunk ctrl+shift+c

# Required Packages
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("neuralnet")
# install.packages("caret")
library(ggplot2)
library(gridExtra)
library(neuralnet)
library(caret)
library(rpart); library(rpart.plot)
library(DMwR)

#Loading in the data
mysmalldata <- read.csv("rawDataSetSmall.csv")
myrawdata <- read.csv("rawDataSet.csv", stringsAsFactors = F)

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

# Partition Data
partitioned_small <- createDataPartition(y = small_cleaned_results$Result, p= 0.7, list = FALSE)

mysmalldata_train_b <- small_cleaned_results[partitioned_small,]
mysmalldata_test_b <- small_cleaned_results[-partitioned_small,]

partitioned_raw <- createDataPartition(y = raw_cleaned_results$Result, p= 0.7, list = FALSE)

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
grid <- expand.grid(k = c(1, 3,4, 5, 6,  7,8,  9))
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

