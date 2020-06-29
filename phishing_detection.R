setwd('/home/colby/Documents/RET/teamC/')

# Required Packages
# install.packages("ggplot2")
# install.packages("gridExtra")

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

result_exist <- c()
i <- 1
while (i <= nrow(mysmalldata)) {
  if (is.na(mysmalldata[i,32])) {
    result_exist[i] <- F
  } else {
    result_exist[i] <- T
  }
  i <- i + 1
}

df_cleaned_results <- mysmalldata[result_exist,]

zero_clean <- df_cleaned_results

# Stupid cleaning loop cause yeah...
i <- 2
while (i <= 31) {
  j <- 1
  while (j <= nrow(zero_clean[i])) {
    if (is.na(zero_clean[[i]][j])) {
      zero_clean[[i]][j] <- 0
    }
    j <- j + 1
  }
  i <- i + 1
}

# Using a low ranking value to determine what 1 and -1 represents.
hist(mysmalldata$having_At_Symbol, main="Having @ symbol")
# 1 represents phishing, -1 represents ligitimant and 0 represents suspicous.

# Creating a color vector
data_result <- c()
i <- 1
while (i <= nrow(mysmalldata)) {
  if (is.na(mysmalldata[i,32])) {
    data_result[i] <- "Unknown"
  } else {
    if (mysmalldata[i,32] == -1) {
    data_result[i] <- "Phishing"
  } else {
    data_result[i] <- "Legitimate"
  }
  }
  
  i <- i + 1
}

URL_Request <- c()
i <- 1
while (i <= nrow(mysmalldata)) {
  if (is.na(mysmalldata$Request_URL[i])) {
    URL_Request[i] <- 0
  } else {
    URL_Request[i] <- mysmalldata$Request_URL[i]
  }
  i <- i + 1
}

df <- data.frame(URL_Request, data_result)
p0 <- ggplot(df, aes(x=URL_Request, color=data_result, fill=data_result)) +
  geom_histogram(bins=3) +
    ggtitle("Are Objects like images loaded from the same domain?")


# Plotting information
library(ggplot2)
p1 <- ggplot(mysmalldata, aes(x=Request_URL, color=data_result, fill=data_result)) + 
  geom_histogram( bins=3) + 
  ggtitle("Are objects like images are loaded from the same domain?")
p2 <- ggplot(mysmalldata, aes(x=age_of_domain, color=data_result, fill=data_result)) +
  geom_histogram( bins=3) +
  ggtitle("Is the age of the domain greater than 2 years?")
p3 <- ggplot(mysmalldata, aes(x=SSLfinal_State, color=data_result, fill=data_result)) +
  geom_histogram( bins=3) +
  ggtitle("Is the final state HTTPS?")
p4 <-ggplot(mysmalldata, aes(x=Statistical_report, color=data_result, fill=data_result)) +
  geom_histogram( bins=3) +
  ggtitle("Is the website rank less than 100,000")


library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow=2)
  