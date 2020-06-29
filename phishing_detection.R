setwd('Documents/RET/teamC/')

#Loading in the data
mysmalldata <- read.csv("rawDataSetSmall.csv", stringsAsFactors = F)
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

# Using a low ranking value to determine what 1 and -1 represents.
hist(mysmalldata$having_At_Symbol, main="Having @ symbol")
# 1 represents phishing, -1 represents ligitimant and 0 represents suspicous.
