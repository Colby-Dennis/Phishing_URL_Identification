# setwd('/home/colby/Documents/GitHub/Phishing_URL_Identification/')

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
  i <- 2
  while (i <= (ncol(df)-1)) {
    j <- 1
    uniq1 <- c(0,-1, 1)
    cnt <- tabulate(df[,i], uniq1)
    prop <- cnt[2] / cnt[3]
    nmbr<-ifelse(runif(1)<=prop,-1,1)

    while (j<= nrow(df[i])) {
      if (is.na(df[[i]][j])) {
        df[[i]][j] <- nmbr
      }
      j <- j + 1
    }
    i <- i + 1
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

mysmalldata_train <- small_cleaned_results[partitioned_small,]
mysmalldata_test <- small_cleaned_results[-partitioned_small,]

partitioned_raw <- createDataPartition(y = raw_cleaned_results$Result, p= 0.7, list = FALSE)

myrawdata_train <- raw_cleaned_results[partitioned_raw,]
myrawdata_test <- raw_cleaned_results[-partitioned_raw,]

# Check Partition
#two-sample z-test on small data (mysmalldata_test , mysmalldata_train)
p1 <- sum(mysmalldata_train$Result=="-1")/nrow(mysmalldata_train)
p2 <- sum(mysmalldata_test$Result=="-1")/nrow(mysmalldata_test)
p_pooled <- (sum(mysmalldata_train$Result=="-1") +
               sum(mysmalldata_test$Result=="-1"))/
  (nrow(mysmalldata_train) + nrow(mysmalldata_test))
z <- (p1 - p2)/sqrt(p_pooled*(1-p_pooled) *
                      (1/nrow(mysmalldata_train) + 1/nrow(mysmalldata_test)))
z.p <- 2*pnorm(-abs(z))

#two-sample z-test on raw data (myrawdata_test, myrawdata_train)
p1 <- sum(myrawdata_train$Result=="-1")/nrow(myrawdata_train)
p2 <- sum(myrawdata_test$Result=="-1")/nrow(myrawdata_test)
p_pooled <- (sum(myrawdata_train$Result=="-1") +
               sum(myrawdata_test$Result=="-1"))/
  (nrow(myrawdata_train) + nrow(myrawdata_test))
z <- (p1 - p2)/sqrt(p_pooled*(1-p_pooled) *
                      (1/nrow(myrawdata_train) + 1/nrow(myrawdata_test)))
z.p <- 2*pnorm(-abs(z))


# Cleaning functions 

#replace with Zeros
mysmalldata_train <- replace_zero(mysmalldata_train)
mysmalldata_test <- replace_zero(mysmalldata_test)
myrawdata_train <- replace_zero(myrawdata_train)
myrawdata_test <- replace_zero(myrawdata_test)
#replace with random -1 or 1
mysmalldata_train <- replace_random_withzero(mysmalldata_train)
mysmalldata_test <- replace_random_withzero(mysmalldata_test)
myrawdata_train <- replace_random_withzero(myrawdata_train)
myrawdata_test <- replace_random_withzero(myrawdata_test)
#replace with mode
mysmalldata_train <- replace_mode(mysmalldata_train)
mysmalldata_test <- replace_mode(mysmalldata_test)
myrawdata_train <- replace_mode(myrawdata_train)
myrawdata_test <- replace_mode(myrawdata_test)
#replace with distributive -1 and 1
mysmalldata_train <- replace_distribution(mysmalldata_train)
mysmalldata_test <- replace_distribution(mysmalldata_test)
myrawdata_train <- replace_distribution(myrawdata_train)
myrawdata_test <- replace_distribution(myrawdata_test)

# Simple Perception
mysmalldata_train_zeros <- replace_zero(mysmalldata_train)
mysmalldata_test_zeros <- replace_zero(mysmalldata_test)

simp_perc_small_zeros<-neuralnet(Result~having_IP_address+URL_Length+Shortining_Service+having_At_Symbol+
                           double_slash_redirecting+Prefix_Suffix+having_Sub_Domain+SSLfinal_State+
                           Domain_registration_length+Favicon+Port+HTTPS_token+Request_URL+
                           URL_of_Anchor+Links_in_tags+SFH+Submitting_to_email+Abnormal_URL+
                           Redirect+on_mouseover+RightClick+popUpWindow+Iframe+age_of_domain+
                           DNSRecord+web_traffic+Page_Rank+Google_Index+Links_pointing_to_page+
                           Statistical_report, mysmalldata_train_zeros,hidden=c(1,1))

plot(simp_perc_small_zeros)

simp_perc_small_zeros_model<-predict(simp_perc_small_zeros,newdata = mysmalldata_test_zeros) 
simp_perc_small_zeros_result <- get_prediction(simp_perc_small_zeros_model)
simp_perc_small_zeros_conf <- confusionMatrix(factor(simp_perc_small_zeros_result), factor(mysmalldata_test_zeros$Result))
print(simp_perc_small_zeros_conf)

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

