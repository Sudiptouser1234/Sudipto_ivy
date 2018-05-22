# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("doParallel")
# install.packages("zoo")
# install.packages("car")
# install.packages("caret")
# install.packages("caTools")
# install.packages("GGally")
# install.packages("viridis")
# install.packages("Hmisc")
# install.packages("data.table")
# install.packages("cowplot")
library(data.table)
library(stringr)
library(dplyr)
library(caTools)
library(MASS)
library(car)
library(doParallel)
library(caret)
library(caret)
library(GGally)
library(cowplot)
library(ggplot2)
library(lubridate)
library(Hmisc)#for creating lag variables
library(zoo)

setwd("~/MyData/Upgrad&IIITB/Ecommerce/Capstoneproject")

consumerdata=read.csv("ConsumerElectronics.csv")
#Refer to the attached excel files. These files are derived from Media data provided
investment=read.csv("Investment.csv")
nps=read.csv("Formatted_NPS.csv")
holidays=read.csv("Formatted_HolidayList.csv",stringsAsFactors = F)

########################################
# Data Understanding
########################################
head(consumerdata)
head(nps)
head(investment)
head(holidays)

str(consumerdata)
str(nps)
str(investment)
str(holidays)

summary(consumerdata)
summary(nps)
summary(investment)
summary(holidays)

########################################
#  Data Preparation 
########################################
############ Week of the year caluclation for consumer data
consumerdata$order_date=as.Date(consumerdata$order_date) #Formatting date
#Considering dates only from July 2015 to June 2016 and extracting week from the same. 
consumerdata=subset(consumerdata,order_date>="2015-07-1" & order_date<="2016-06-30")
consumerdata$week=lubridate::isoweek(ymd(consumerdata$order_date))
str(consumerdata$week)


# --------Create a new column with Year and week of the year which is used for plotting
consumerdata$YearWeek <- paste(consumerdata$Year,sprintf("%02d", as.numeric(consumerdata$week)), sep=" wk")
head(consumerdata$YearWeek)
sort(unique(consumerdata$YearWeek))
# Replace 2016 wk53 with 2015 wk53. Combination of year with starting of the year is causing this issue.
# Set the starting dates of Jan 2016 also to 2015 wk53 - to aggregate properly. 
consumerdata[which(consumerdata$YearWeek == "2016 wk53"),]$YearWeek <-  "2015 wk53"



# Duplicate/NA Check
#----------------------------------------
sum(duplicated(consumerdata))            # 106804 . Remove them. 
consumerdata=consumerdata[-which(duplicated(consumerdata)),]

sum(is.na(consumerdata))                  #12078 .
names(which(sapply(consumerdata, function(x) any(is.na(x)))))     # columns with NAs

# Replacing NAs in cust id and pin code with 0. T
consumerdata[which(is.na(consumerdata$cust_id)),]$cust_id <- 0
consumerdata[which(is.na(consumerdata$pincode)),]$pincode <- 0


# Handling NAs in GMV
sum(is.na(consumerdata$gmv))     # 4026
##Since number of rows is high. Lets not omit them.
# Replacing NA with mean of gmv for orders with same fsn_id
consumerdata$gmv=with(consumerdata, ave(gmv, fsn_id,
                                        FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
# Confirming no more NAs left
sum(is.na(consumerdata$gmv))
consumerdata[which(is.na(consumerdata$gmv)),]

# Since the remaining NA's dont hv any reference fsn_id to take the mean of, hence removing them
consumerdata = na.omit(consumerdata)

#  ------- Merging NPS data
consumerdata = merge(consumerdata,nps,by=c("Year","Month"))
sum(is.na(consumerdata))
#  - Confirming no NA's added from NPS
#  NPS will be same for all the weeks in a Month. 


# Delays
unique(consumerdata$deliverybdays)
consumerdata[which(consumerdata$deliverybdays=="\\N"),]$deliverybdays = 0
consumerdata$deliverybdays <- as.numeric(as.character(consumerdata$deliverybdays))
# considering delays greater than approximate months as noise and suppressing it to 0. (60 days).
# also -ve values are imputed to 0. 
consumerdata[which(consumerdata$deliverybdays < 0),]$deliverybdays <- 0
consumerdata[which(consumerdata$deliverybdays > 60),]$deliverybdays <- 0
which(is.na(consumerdata$deliverybdays))
##No NA's found

# Delays
unique(consumerdata$deliverycdays)
consumerdata[which(consumerdata$deliverycdays=="\\N"),]$deliverycdays = 0
consumerdata$deliverycdays <- as.numeric(as.character(consumerdata$deliverycdays))
# considering delays greater than approximate months as noise and suppressing it to 0. (60 days).
# also -ve values are imputed to 0. 
consumerdata[which(consumerdata$deliverycdays < 0),]$deliverycdays <- 0
consumerdata[which(consumerdata$deliverycdays > 60),]$deliverycdays <- 0
which(is.na(consumerdata$deliverycdays))
##No NA's found


# Derived KPIS
#----------------------------------------------------------
# Total SLA
consumerdata$total_sla<-consumerdata$sla+consumerdata$product_procurement_sla

# 2 - Discount %
consumerdata$List_price=consumerdata$gmv/consumerdata$units
#Creating a new variable promotion offered
consumerdata$promotion_offered=((consumerdata$product_mrp-consumerdata$List_price)/consumerdata$product_mrp) * 100   # in percentage
#Replacing INF values with NA
invisible(lapply(names(consumerdata),function(.name) set(consumerdata, which(is.infinite(consumerdata[[.name]])), j = .name,value =NA)))
#Replacing nas with 0
consumerdata[which(is.na(consumerdata$promotion_offered)),]$promotion_offered <-  0


# Sale dates
str(holidays)
# Sale KPI
holidays$Holiday.Dates=as.Date(holidays$Holiday.Dates, "%Y-%m-%d")
consumerdata$is_holiday = 0
consumerdata[which(consumerdata$order_date %in% holidays$Holiday.Dates),]$is_holiday =1
sum(is.na(consumerdata))




str(consumerdata)
# product_analytic_super_category is a factor with just 1 level. Can be removed. 
consumerdata <- subset(consumerdata, select= -c(product_analytic_super_category))
# FSN ID will be removed. Since modelling including it - will become too specific to an SKU
consumerdata <- subset(consumerdata, select = -c(fsn_id))
# Also removing below params since not much information from the same. 
# Cust id and pincode are masked. Not much info about geographical area possible
consumerdata <- subset(consumerdata, select = -c(cust_id, pincode, order_id))
str(consumerdata)



# ----------------------- Merge with investment. 

# creating data for the investment to get it by week. 
# Weeks range from wk27 2015 to wk26 2016. Hence the week numbers are unique and only
# Week numbers will be used to group / merge. 

# Create a date series from July2015 to June 2016 end. 
dates <- seq(as.Date("2015-07-01"), as.Date("2016-06-30"), by=1)
week <- lubridate::isoweek(ymd(dates))
Dateslist <- data.frame(dates, week)
Dateslist$Month <- as.numeric(format(as.Date(Dateslist$dates), "%m"))
Dateslist$Year <-  as.numeric(format(as.Date(Dateslist$dates), "%Y"))

DailyInvestment = transmute(investment,
                            yearmon = as.Date(as.yearmon(str_c(investment$Year,investment$Month,sep="-"))),
                            numdays=monthDays(yearmon),
                            DailyTotal_Investment=Total_Investment/numdays,
                            DailyTVInv=TV/numdays,
                            DailyDigitalInv=Digital/numdays,
                            DailySponsorshipInv=Sponsorship/numdays,
                            DailyContent_MarketingInv=Content_Marketing/numdays,
                            DailyOnline_marketingInv=Online_marketing/numdays,
                            DailyAffiliatesInv=Affiliates/numdays,
                            DailySEMInv=SEM/numdays,
                            DailyRadioInv=Radio/numdays,
                            DailyOtherInv=Other/numdays)
DailyInvestment=cbind(DailyInvestment,Year=investment$Year,Month=investment$Month)
DailyInvestment$yearmon = NULL
DailyInvestment$numdays = NULL
DailyInvestment<-merge(Dateslist,DailyInvestment,by=c("Year","Month"), sort = TRUE)
Weeklydata <- DailyInvestment %>% group_by(week) %>% summarise(total = sum(DailyTotal_Investment), 
                                                               TV = sum(DailyTVInv),
                                                               Digital = sum(DailyDigitalInv),
                                                               Sponsorship = sum(DailySponsorshipInv),
                                                               Content_Marketing = sum(DailyContent_MarketingInv),
                                                               Online_Marketing = sum(DailyOnline_marketingInv),
                                                               Affiliates = sum(DailyAffiliatesInv),
                                                               SEM = sum(DailySEMInv),
                                                               Radio = sum(DailyRadioInv),
                                                               Other = sum(DailyOtherInv))

sum(is.na(Weeklydata)) ## 84 NA's
names(which(sapply(Weeklydata, function(x) any(is.na(x)))))  
Weeklydata[which(is.na(Weeklydata$Radio)),]$Radio <-  0
Weeklydata[which(is.na(Weeklydata$Other)),]$Other <-  0


# Also get if sale week depending on the is_sale marked for the dates in consumerdata.
sale_weeks <- as.vector(unique(consumerdata$week[consumerdata$is_holiday > 0]))
Weeklydata$is_sale <- "No"
Weeklydata[which(Weeklydata$week %in% sale_weeks),]$is_sale ="yes"
head(Weeklydata)

# Merge the investment and sale data by week to the main data.
consumerdata<- merge(consumerdata, Weeklydata, by = c("week"))
# remove the is_holiday - since we have already derived it for the week.
consumerdata <- subset(consumerdata, select= -c(is_holiday))


# Creating Dummy Variables
payment_type <- data.frame(model.matrix( ~s1_fact.order_payment_type, data = consumerdata))
payment_type<- payment_type[,-1]
consumerdata<-cbind(subset(consumerdata, select = -c(s1_fact.order_payment_type)),payment_type)
str(consumerdata)


# -------------------------- EDA for all 3 categories together.
consumerdata3Category = subset(consumerdata,product_analytic_sub_category%in% c("GamingAccessory","HomeAudio","CameraAccessory"))
summary(consumerdata3Category$product_analytic_sub_category)
ggplot(data = consumerdata3Category, aes(x = YearWeek, y = sum(gmv), fill = factor(product_analytic_sub_category))) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))   # might need to change here. 

ggplot(data = consumerdata3Category,
       aes(consumerdata3Category$YearWeek, gmv,fill=factor(product_analytic_sub_category))) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "bar")


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

######################################## Subsetting data for 3 sub categories ######################################
consumerdataGA = subset(consumerdata,product_analytic_sub_category=='GamingAccessory')[,-which(colnames(consumerdata)=="product_analytic_sub_category" )]
consumerdataHA = subset(consumerdata,product_analytic_sub_category=='HomeAudio')[,-which(colnames(consumerdata)=="product_analytic_sub_category" )]
consumerdataCA = subset(consumerdata,product_analytic_sub_category=='CameraAccessory')[,-which(colnames(consumerdata)=="product_analytic_sub_category" )]
# To roll up to a weekly level Identify how the column values need to be calculated 
# - Either Average, Sum or Mode(repetition)

colWhichNeedMean = c("sla","product_mrp","product_procurement_sla",
                     "total_sla",
                     "List_price","promotion_offered", "deliverybdays", "deliverycdays")
colWhicNeedMode=c("Month","order_date","order_item_id","payment_type", "NPS", "YearWeek",
                  "NPS", "total", "TV", "Digital", "Sponsorship", "Content_Marketing",
                  "Online_Marketing", "Affiliates", "SEM", "Radio", "Other", "is_sale", "payment_type")
colWhichNeedSum=c("gmv","units")


############################################## Model Building for Gaming Accessory ##########################################

# Check all the columns.
colnames(consumerdataGA)

str(consumerdataGA$product_analytic_category)
unique(consumerdataGA$product_analytic_category)
# Only Gaming Hardware present. Remove this column. 
consumerdataGA$product_analytic_category <- NULL



#  - Aggregation by weeks 
consumerdataGAMeanCols=consumerdataGA%>% group_by(week) %>% 
  summarise_at(.vars=colWhichNeedMean,.funs = mean)

consumerdataGAModeCols=consumerdataGA%>% group_by(week) %>% 
  summarise_at(.vars=colWhicNeedMode,.funs=getmode)

consumerdataGASumCols=consumerdataGA%>% group_by(week) %>% 
  summarise_at(.vars=colWhichNeedSum,.funs = sum)

consumerdataGAWeekly=merge(consumerdataGAModeCols,consumerdataGAMeanCols,by=c("week"))
consumerdataGAWeekly=merge(consumerdataGAWeekly,consumerdataGASumCols,by=c("week"))

colnames(consumerdataGAWeekly)
str(consumerdataGAWeekly)

consumerdataGAWeekly <- consumerdataGAWeekly[order(consumerdataGAWeekly$YearWeek),] 
GAUnits = consumerdataGAWeekly$units


########################### EDA for GA

# Plotting the GMV trend.
ggplot(data = consumerdataGAWeekly, aes(x = YearWeek, y = gmv, group = factor(is_sale), colour = factor(is_sale))) + geom_line() +
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

# No particular overall trend observed. 
# Higher GMV is seen in weeks when there is a sale.

ggplot(data = consumerdataGAWeekly, aes(x = YearWeek, y = total, group = factor(is_sale), colour = factor(is_sale))) + geom_line() +
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

ggplot(data = consumerdataGAWeekly, aes(x = YearWeek, y = total, fill = factor(is_sale))) + geom_bar(stat = "identity") +
  theme_light() + theme(axis.text.x = element_text(angle = 90)) 
# more total investment is seen around sale weeks. 


# Checking by Investment type.
melted = melt(consumerdataGAWeekly[, 7:17], id.vars="YearWeek")
ggplot(data=melted, aes(x=YearWeek, y=value, group=variable, colour = variable)) + geom_line(size = 1) + 
  theme_light() + theme(axis.text.x = element_text(angle = 90))
# More investment is seen in sponsorship.



# Checking relationship between GMV and advertising - taking into consideration sale weeks.
ggplot(data = consumerdataGAWeekly, aes(x = total, y = gmv, group = factor(is_sale), colour = factor(is_sale))) + geom_line() + geom_smooth()+
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

# We see an increase in gmv when there is more total investment around sale.
# On non sale days increase in total investment doesnt seem to make much difference in GMV. 

# Check impact of NPS on total GMV
ggplot(data = consumerdataGAWeekly, aes(x = factor(NPS), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()

# Doesnt show any correlation/trend.

# Check relationship between promotion_offered and GMV
ggplot(data = consumerdataGAWeekly, aes(x = factor(round(promotion_offered,2)), y = gmv)) + geom_bar(stat = "identity") + theme_light() +
  theme(axis.text.x = element_text(angle = 90)) 
# In general GMV increases as promotions increase.

# Check relationship between number of units sold and promotions offered.
ggplot(data = consumerdataGAWeekly, aes(x = factor(round(promotion_offered,2)), y = units, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light() +
  theme(axis.text.x = element_text(angle = 90)) 
# Increasing trend. 
# Max units are sold when there is a promotion during a sale day

# Preferred payment type. 
ggplot(data = consumerdataGAWeekly, aes(x = payment_type, fill = factor(is_sale))) + geom_bar()
# One payment type is preferred over the other. 


# Check if the delivery/dispatch delays have any affect on GMV
ggplot(data = consumerdataGAWeekly, aes(x = factor(round(deliverybdays,1)), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()
# max GMV when there is no delay.  - But the data had a lot of noise - cant really conclude. 

ggplot(data = consumerdataGAWeekly, aes(x = factor(round(deliverycdays,1)), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()
# max GMV when there is no delay.  - But the data had a lot of noise - cant really conclude. 


#################################### End of EDA for GA
# YearWeek only created for plotting and ordering. Remove it. 
consumerdataGAWeekly$YearWeek = NULL
head(consumerdataGAWeekly)

# GMV is a combination of List Price and  Units. Removing them from the model. 
# removing Units in model building to avoid overfit. 
consumerdataGAWeekly$units <- NULL


#########################################
# GA Basic Model
#########################################
set.seed(100)

split= sample(1:nrow(consumerdataGAWeekly), 0.7*nrow(consumerdataGAWeekly))
trainBasicGA = consumerdataGAWeekly[split,]
testBasicGA = consumerdataGAWeekly[-split,]

GABasicModel1<-lm(gmv~.,data=trainBasicGA)
summary(GABasicModel1)
step <- stepAIC(GABasicModel1, direction="both")
step


GABasicModel2<-lm(formula = gmv ~ payment_type + NPS + total + TV + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    is_sale + product_mrp + List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)

summary(GABasicModel2)
sort(vif(GABasicModel2))

##Removing product_mrp - low significance
GABasicModel3<-lm(formula = gmv ~ payment_type + NPS + total + TV + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    is_sale + List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)
summary(GABasicModel3)
sort(vif(GABasicModel3))

##Removing payment_type  - low significance 
GABasicModel4<-lm(formula = gmv ~ NPS + total + TV + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    is_sale + List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)
summary(GABasicModel4)
sort(vif(GABasicModel4))


#Removing Online_Marketing - low significance anf high vif
GABasicModel5<-lm(formula = gmv ~ NPS + total + TV + Sponsorship + 
                    Content_Marketing + Affiliates + SEM + 
                    is_sale + List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)
summary(GABasicModel5)
sort(vif(GABasicModel5))

#Removing Affiliates - low significance
GABasicModel6<-lm(formula = gmv ~ NPS + total + TV + Sponsorship + 
                    Content_Marketing  + SEM + 
                    is_sale + List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)
summary(GABasicModel6)
sort(vif(GABasicModel6))

#Removing is_sale - low significance
GABasicModel7<-lm(formula = gmv ~ NPS + total + TV + Sponsorship + 
                    Content_Marketing  + SEM + 
                    List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)
summary(GABasicModel7)
sort(vif(GABasicModel7))


#Removing NPS - low significance
GABasicModel8<-lm(formula = gmv ~ total + TV + Sponsorship + 
                    Content_Marketing  + SEM + 
                    List_price + promotion_offered + 
                    deliverybdays + deliverycdays, data = trainBasicGA)
summary(GABasicModel8)
sort(vif(GABasicModel8))


#Removing deliverybdays - low significance
GABasicModel9<-lm(formula = gmv ~ total + TV + Sponsorship + 
                    Content_Marketing  + SEM + 
                    List_price + promotion_offered + 
                    deliverycdays, data = trainBasicGA)
summary(GABasicModel9)
sort(vif(GABasicModel9))

#Removing deliverycdays - low significance
GABasicModel10<-lm(formula = gmv ~ total + TV + Sponsorship + 
                     Content_Marketing  + SEM + 
                     List_price + promotion_offered , data = trainBasicGA)
summary(GABasicModel10)
sort(vif(GABasicModel10))

#Removing TV - low significance
GABasicModel11<-lm(formula = gmv ~ total +  Sponsorship + 
                     Content_Marketing  + SEM + 
                     List_price + promotion_offered , data = trainBasicGA)
summary(GABasicModel11)
sort(vif(GABasicModel11))

#Removing SEM - low significance
GABasicModel12<-lm(formula = gmv ~ total +  Sponsorship + 
                     Content_Marketing  + List_price + promotion_offered , data = trainBasicGA)
summary(GABasicModel12)
sort(vif(GABasicModel12))

#Removing Sponsorship - low significance
GABasicModel13<-lm(formula = gmv ~ total +  Content_Marketing  + List_price + promotion_offered , data = trainBasicGA)
summary(GABasicModel13)
sort(vif(GABasicModel13))

#Removing Content_Marketing - low significance
GABasicModel14<-lm(formula = gmv ~ total +  List_price + promotion_offered , data = trainBasicGA)
summary(GABasicModel14)
sort(vif(GABasicModel14))

GABasicFinalModel=GABasicModel14
##Since the remaining variables are significant. Keeping this as final model.
#Multiple R-squared:  0.5559,	Adjusted R-squared:  0.5155 

#Prediction part
predBasic =predict(GABasicFinalModel,testBasicGA)
testBasicGA$test_gmv=predBasic
r<-cor(testBasicGA$gmv,testBasicGA$test_gmv)
r
#Verifying r-squared value for test data
rsquaredBasic<-cor(testBasicGA$gmv,testBasicGA$test_gmv)^2
rsquaredBasic
#[1] .2666398

##The R-squared for the model built on test data set=.2666398
#This means the model is not generalizing well with the prediction.Hence,we cannot implement this model.


######################################
# GA Multiplicative Model
######################################
colnames(consumerdataGAWeekly)

GALogValues <- consumerdataGAWeekly
# Converting some columns to log values. 

names(which(sapply(GALogValues, function(x) any(x == 0))))  
head(GALogValues)

GALogValues[which(GALogValues$TV == 0),]$TV <-  0.001
GALogValues[which(GALogValues$Content_Marketing == 0),]$Content_Marketing <-0.001
GALogValues[which(GALogValues$Radio == 0),]$Radio <- 0.001
GALogValues[which(GALogValues$Other == 0),]$Other <- 0.001
GALogValues[which(GALogValues$deliverybdays == 0),]$deliverybdays <- 0.00001
GALogValues[which(GALogValues$deliverycdays == 0),]$deliverycdays <- 0.00001



colnames(GALogValues)
head(GALogValues)
GALogValues[,c(6:16, 18:26)] <- log(GALogValues[c(6:16, 18:26)])
summary(GALogValues)  
# Replace Infinite values with 0. 
invisible(lapply(names(GALogValues),function(.name) set(GALogValues, which(is.infinite(GALogValues[[.name]])), j = .name,value =0)))
sum(is.na(GALogValues))


# Split into Train and test data.
split= sample(1:nrow(GALogValues), 0.7*nrow(GALogValues))
trainlogGA = GALogValues[split,]
testlogGA = GALogValues[-split,]
head(trainlogGA)
GAMultiModel1<-lm(gmv~.,data=trainlogGA)
summary(GAMultiModel1)
step <- stepAIC(GAMultiModel1, direction="both")
step




GAMultiModel2 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + TV + Digital + Content_Marketing + Online_Marketing + 
                      Affiliates + SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel2)
sort(vif(GAMultiModel2))

# Removing Affiliates - low significance. and high VIF
GAMultiModel3 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + TV + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel3)
sort(vif(GAMultiModel3))


# Removing TV
GAMultiModel4 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel4)
sort(vif(GAMultiModel4))


# Removing NPS
GAMultiModel5 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      total + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel5)
sort(vif(GAMultiModel5))


# Removing total
GAMultiModel6 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel6)
sort(vif(GAMultiModel6))


# Removing product SLA 
GAMultiModel7 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel7)
sort(vif(GAMultiModel7))


#Removing payment type
GAMultiModel8 <- lm(formula = gmv ~ Month + order_date + order_item_id + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel8)
sort(vif(GAMultiModel8))


# Removing promotion offered.
GAMultiModel9 <- lm(formula = gmv ~ Month + order_date + order_item_id + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price  + deliverybdays, 
                    data = trainlogGA)
summary(GAMultiModel9)
sort(vif(GAMultiModel9))




######################################
# GA Kyock Model
######################################
colnames(consumerdataGAWeekly)

GALagValues <- consumerdataGAWeekly
GALagValues = cbind(GALagValues,GAUnits)
colnames(GALagValues)[27] = "units"
#Creating lag vaules for dependent variable i.e. gmv for kyock model
#lag gmv for 1 day,2 day,3 day,1 week before,1 month before
#LAGS of gmvs should be included only for Kyock,distributed and hybrid models;not to be included for basic and multiplicative models

GALagValues$lag_gmv_1day=Lag(GALagValues$gmv,-1)
GALagValues$lag_gmv_2day=Lag(GALagValues$gmv,-2)
GALagValues$lag_gmv_3day=Lag(GALagValues$gmv,-3)
GALagValues$lag_gmv_1week=Lag(GALagValues$gmv,-7)
GALagValues$lag_gmv_2week=Lag(GALagValues$gmv,-14)
GALagValues$lag_gmv_3week=Lag(GALagValues$gmv,-21)
GALagValues$lag_gmv_1month=Lag(GALagValues$gmv,-30)

GALagValues[which(is.na(GALagValues$lag_gmv_1day)),]$lag_gmv_1day <-  0
GALagValues[which(is.na(GALagValues$lag_gmv_2day)),]$lag_gmv_2day <-  0
GALagValues[which(is.na(GALagValues$lag_gmv_3day)),]$lag_gmv_3day <-  0
GALagValues[which(is.na(GALagValues$lag_gmv_1month)),]$lag_gmv_1month <-  0
GALagValues[which(is.na(GALagValues$lag_gmv_1week)),]$lag_gmv_1week <-  0
sum(is.na(GALagValues))



set.seed(100)

split= sample(1:nrow(GALagValues), 0.7*nrow(GALagValues))
trainlagGA = GALagValues[split,]
testlagGA = GALagValues[-split,]

str(trainlagGA)
GAKyockModel1<-lm(gmv~units+product_mrp+List_price+total_sla+promotion_offered+lag_gmv_1week,data=trainlagGA)
summary(GAKyockModel1)
vif(GAKyockModel1)
step <- stepAIC(GAKyockModel1, direction="both")
step

GAKyockModel2<-lm(gmv~List_price+product_mrp+units,data=trainlagGA)
summary(GAKyockModel2)
#Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9462
vif(GAKyockModel2)

#Prediction part:
predKyockGA<-predict(GAKyockModel2,testlagGA)
testlagGA$test_gmv<-predKyockGA
r<-cor(testlagGA$gmv,testlagGA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testlagGA$gmv,testlagGA$test_gmv)^2
rsquared
#[1] 0.9512582


#CV using bootstrap method
library(bootstrap)
#Using 10-fold cv
k_fold_rsq <- function(lmfit, ngroup=10) {
  
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

# fit and call function
lmfit <-GAKyockModel2
k_fold_rsq(lmfit, ngroup=10)
#raw_rsq    cv_rsq 
#0.9506970 0.9122742 
#rsquared from cv=0.9122742 

######################################
# GA Distributed lag``
######################################
#lag of product_mrp for 1,2,3 weeks


GALagValues$lag_product_mrp_1week=Lag(GALagValues$product_mrp,-7)
GALagValues$lag_product_mrp_2week=Lag(GALagValues$product_mrp,-14)
GALagValues$lag_product_mrp_3week=Lag(GALagValues$product_mrp,-21)

sum(is.na(GALagValues))

GALagValues[which(is.na(GALagValues$lag_product_mrp_1week)),]$lag_product_mrp_1week <-  0
GALagValues[which(is.na(GALagValues$lag_product_mrp_2week)),]$lag_product_mrp_2week <-  0
GALagValues[which(is.na(GALagValues$lag_product_mrp_3week)),]$lag_product_mrp_3week <-  0

sum(is.na(GALagValues))
#No NAs found


#Creating lags of list price


GALagValues$lag_List_price_1week=Lag(GALagValues$List_price,-7)
GALagValues$lag_List_price_2week=Lag(GALagValues$List_price,-14)
GALagValues$lag_List_price_3week=Lag(GALagValues$List_price,-21)

sum(is.na(GALagValues))

GALagValues[which(is.na(GALagValues$lag_List_price_1week)),]$lag_List_price_1week <-  0
GALagValues[which(is.na(GALagValues$lag_List_price_2week)),]$lag_List_price_2week <-  0
GALagValues[which(is.na(GALagValues$lag_List_price_3week)),]$lag_List_price_3week <-  0

sum(is.na(GALagValues))

#lag of promotion_offered for 1,2,3 weeks


GALagValues$lag_promotion_offered_1week=Lag(GALagValues$promotion_offered,-7)
GALagValues$lag_promotion_offered_2week=Lag(GALagValues$promotion_offered,-14)
GALagValues$lag_promotion_offered_3week=Lag(GALagValues$promotion_offered,-21)

sum(is.na(GALagValues))

GALagValues[which(is.na(GALagValues$lag_promotion_offered_1week)),]$lag_promotion_offered_1week <-  0
GALagValues[which(is.na(GALagValues$lag_promotion_offered_2week)),]$lag_promotion_offered_2week <-  0
GALagValues[which(is.na(GALagValues$lag_promotion_offered_3week)),]$lag_promotion_offered_3week <-  0

sum(is.na(GALagValues))
#No NAs found

#lag of total_sla for 1,2,3 weeks


GALagValues$lag_total_sla_1week=Lag(GALagValues$total_sla,-7)
GALagValues$lag_total_sla_2week=Lag(GALagValues$total_sla,-14)
GALagValues$lag_total_sla_3week=Lag(GALagValues$total_sla,-21)

sum(is.na(GALagValues))

GALagValues[which(is.na(GALagValues$lag_total_sla_1week)),]$lag_total_sla_1week <-  0
GALagValues[which(is.na(GALagValues$lag_total_sla_2week)),]$lag_total_sla_2week <-  0
GALagValues[which(is.na(GALagValues$lag_total_sla_3week)),]$lag_total_sla_3week <-  0

sum(is.na(GALagValues))

names(which(sapply(GALagValues, function(x) any(is.na(x))))) 
GALagValues[which(is.na(GALagValues$lag_gmv_2week)),]$lag_gmv_2week <-  0
GALagValues[which(is.na(GALagValues$lag_gmv_3week)),]$lag_gmv_3week <-  0
sum(is.na(GALagValues))
#No NAs found

set.seed(100)

split= sample(1:nrow(GALagValues), 0.7*nrow(GALagValues))
trainlagGA = GALagValues[split,]
testlagGA = GALagValues[-split,]


DisLagGAmodel1<-lm(gmv~total_sla +lag_total_sla_1week+lag_total_sla_2week+
                     lag_gmv_1week+lag_gmv_2week+lag_gmv_3week+product_mrp+ lag_product_mrp_1week+
                     List_price+lag_List_price_1week+promotion_offered+lag_promotion_offered_1week,data=trainlagGA)

summary(DisLagGAmodel1)
vif(DisLagGAmodel1)
step <- stepAIC(DisLagGAmodel1, direction="both")

DisLagGAmodel2<-lm(gmv~lag_total_sla_2week+lag_gmv_1week+lag_gmv_2week+product_mrp+ lag_product_mrp_1week+
                     promotion_offered,data=trainlagGA)

summary(DisLagGAmodel2)
vif(DisLagGAmodel2)

#Excluding lag_product_mrp_1week
DisLagGAmodel3<-lm(gmv~lag_total_sla_2week+lag_gmv_1week+lag_gmv_2week+product_mrp+ 
                     promotion_offered,data=trainlagGA)

summary(DisLagGAmodel3)
vif(DisLagGAmodel3)

#Excluding lag_total_sla_2week 
DisLagGAmodel4<-lm(gmv~lag_gmv_1week+lag_gmv_2week+product_mrp+ promotion_offered,data=trainlagGA)

summary(DisLagGAmodel4)
#Multiple R-squared:  0.842,     Adjusted R-squared:  0.8222
vif(DisLagGAmodel4)




#Prediction part:
pred<-predict(DisLagGAmodel4,testlagGA)
testlagGA$test_gmv<-pred
r<-cor(testlagGA$gmv,testlagGA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testlagGA$gmv,testlagGA$test_gmv)^2
rsquared
#[1] 0.1924762



###################################
# Hybrid model for GA
###################################

set.seed(100)

GAHrybridWeekly = merge(consumerdataGAWeekly,GALagValues)
GAHrybridWeekly$log_gmv = log(GAHrybridWeekly$gmv)
GAHrybridWeekly$log_promotion_offered = log(GAHrybridWeekly$promotion_offered)
GAHrybridWeekly$log_tv = log(GAHrybridWeekly$TV)
GAHrybridWeekly$log_digital = log(GAHrybridWeekly$Digital)
GAHrybridWeekly$log_sponsorship = log(GAHrybridWeekly$Sponsorship)
sum(is.na(GAHrybridWeekly))

split= sample(1:nrow(GAHrybridWeekly), 0.7*nrow(GAHrybridWeekly))
trainHybridGA = GAHrybridWeekly[split,]
testHybridGA = GAHrybridWeekly[-split,]

invisible(lapply(names(GAHrybridWeekly),function(.name) set(GAHrybridWeekly, which(is.infinite(GAHrybridWeekly[[.name]])), j = .name,value =0)))

HybridGAmodel1<-lm(gmv~total_sla +lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+lag_gmv_1week+
                     product_mrp+ lag_product_mrp_1week+List_price+lag_List_price_1week+promotion_offered+lag_promotion_offered_1week+log_gmv+
                     log_promotion_offered+log_tv+log_digital+log_sponsorship,data=trainHybridGA)


summary(HybridGAmodel1)
vif(HybridGAmodel1)
step <- stepAIC(HybridGAmodel1, direction="both")

HybridGAmodel2<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_digital+log_sponsorship,data=trainHybridGA)

summary(HybridGAmodel2)
vif(HybridGAmodel2)

#Excluding log_digital
HybridGAmodel3<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_sponsorship,data=trainHybridGA)

summary(HybridGAmodel3)
vif(HybridGAmodel3)

#Excluding log_sponsorship

HybridGAmodel4<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered,data=trainHybridGA)

summary(HybridGAmodel4)
vif(HybridGAmodel4)

#Excluding lag_total_sla_2week
HybridGAmodel5<-lm(gmv~lag_total_sla_1week+lag_total_sla_3week+
                     product_mrp+promotion_offered,data=trainHybridGA)

summary(HybridGAmodel5)
vif(HybridGAmodel5)

#Excluding lag_total_sla_1week
HybridGAmodel6<-lm(gmv~lag_total_sla_3week+product_mrp+promotion_offered,data=trainHybridGA)

summary(HybridGAmodel6)
#Multiple R-squared:  0.7631,    Adjusted R-squared:  0.7415 
vif(HybridGAmodel6)



#Prediction part:
pred<-predict(model6,test)
test$test_gmv<-pred
r<-cor(test$gmv,test$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(test$gmv,test$test_gmv)^2
rsquared
#[1] 0.03237991


#CV using bootstrap method

#Using 10-fold cv
k_fold_rsq <- function(lmfit, ngroup=10) {
  
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

# fit and call function
lmfit <-model6
k_fold_rsq(lmfit, ngroup=10)
#0.7630783 0.6614453

#rsquared from cv=0.6614453    

################################################### End of Modelling for Gaming Accessory ##################################################

####################################################### Modelling for Home Audio ##################################################
# Check all the columns.
colnames(consumerdataHA)

str(consumerdataHA$product_analytic_category)
unique(consumerdataHA$product_analytic_category)
# Only EntertainmentSmall present. Remove this column. 
consumerdataHA$product_analytic_category <- NULL

#  - Aggregation by weeks 
consumerdataHAMeanCols=consumerdataHA%>% group_by(week) %>% 
  summarise_at(.vars=colWhichNeedMean,.funs = mean)

consumerdataHAModeCols=consumerdataHA%>% group_by(week) %>% 
  summarise_at(.vars=colWhicNeedMode,.funs=getmode)

consumerdataHASumCols=consumerdataHA%>% group_by(week) %>% 
  summarise_at(.vars=colWhichNeedSum,.funs = sum)

consumerdataHAWeekly=merge(consumerdataHAModeCols,consumerdataHAMeanCols,by=c("week"))
consumerdataHAWeekly=merge(consumerdataHAWeekly,consumerdataHASumCols,by=c("week"))

colnames(consumerdataHAWeekly)
str(consumerdataHAWeekly)

consumerdataHAWeekly <- consumerdataHAWeekly[order(consumerdataHAWeekly$YearWeek),] 

HAUnits = consumerdataHAWeekly$units

########################### EDA for HA

# Plotting the GMV trend.
ggplot(data = consumerdataHAWeekly, aes(x = YearWeek, y = gmv, group = factor(is_sale), colour = factor(is_sale))) + geom_line() +
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

# No particular overall trend observed. 
# Higher GMV is seen in weeks when there is a sale.

ggplot(data = consumerdataHAWeekly, aes(x = YearWeek, y = total, group = factor(is_sale), colour = factor(is_sale))) + geom_line() +
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

ggplot(data = consumerdataHAWeekly, aes(x = YearWeek, y = total, fill = factor(is_sale))) + geom_bar(stat = "identity") +
  theme_light() + theme(axis.text.x = element_text(angle = 90)) 
# more total investment is seen around sale weeks. 


# Checking by Investment type.
melted = melt(consumerdataHAWeekly[, 7:17], id.vars="YearWeek")
ggplot(data=melted, aes(x=YearWeek, y=value, group=variable, colour = variable)) + geom_line(size = 1) + 
  theme_light() + theme(axis.text.x = element_text(angle = 90))
# More investment is seen in sponsorship.



# Checking relationship between GMV and advertising - taking into consideration sale weeks.
ggplot(data = consumerdataHAWeekly, aes(x = total, y = gmv, group = factor(is_sale), colour = factor(is_sale))) + geom_line() + geom_smooth()+
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

# We see an increase in gmv when there is more total investment around sale.
# On non sale days increase in total investment doesnt seem to make much difference in GMV. 

# Check impact of NPS on total GMV
ggplot(data = consumerdataHAWeekly, aes(x = factor(NPS), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()

# Doesnt show any correlation/trend.

# Check relationship between promotion_offered and GMV
ggplot(data = consumerdataHAWeekly, aes(x = factor(round(promotion_offered,2)), y = gmv)) + geom_bar(stat = "identity") + theme_light() +
  theme(axis.text.x = element_text(angle = 90)) 
# In general GMV increases as promotions increase.

# Check relationship between number of units sold and promotions offered.
ggplot(data = consumerdataHAWeekly, aes(x = factor(round(promotion_offered,2)), y = units, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light() +
  theme(axis.text.x = element_text(angle = 90)) 
# Increasing trend. 
# Max units are sold when there is a promotion during a sale day

# Preferred payment type. 
ggplot(data = consumerdataHAWeekly, aes(x = payment_type, fill = factor(is_sale))) + geom_bar()
# One payment type is preferred over the other. 


# Check if the delivery/dispatch delays have any affect on GMV
ggplot(data = consumerdataHAWeekly, aes(x = factor(round(deliverybdays,1)), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()
# max GMV when there is no delay.  - But the data had a lot of noise - cant really conclude. 

ggplot(data = consumerdataHAWeekly, aes(x = factor(round(deliverycdays,1)), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()
# max GMV when there is no delay.  - But the data had a lot of noise - cant really conclude. 


#################################### End of EDA for HA

# YearWeek only created for plotting and ordering. Remove it. 
consumerdataHAWeekly$YearWeek = NULL
head(consumerdataHAWeekly)

# GMV is a combination of List Price and  Units. Removing them from the model. 
# removing Units in model building to avoid overfit. 
consumerdataHAWeekly$units <- NULL


#########################################
# HA Basic Model
#########################################
set.seed(100)

split= sample(1:nrow(consumerdataHAWeekly), 0.7*nrow(consumerdataHAWeekly))
trainBasicHA = consumerdataHAWeekly[split,]
testBasicHA = consumerdataHAWeekly[-split,]

HABasicModel1<-lm(gmv~.,data=trainBasicHA)
summary(HABasicModel1)
step <- stepAIC(HABasicModel1, direction="both")
step

HABasicModel2 = lm(formula = gmv ~ week + Month + order_date + total + TV + Digital + 
                     Sponsorship + Content_Marketing + Online_Marketing + SEM + 
                     Other + is_sale + product_mrp + promotion_offered + deliverybdays + 
                     deliverycdays, data = trainBasicHA)
summary(HABasicModel2)
sort(vif(HABasicModel2))

##Removing deliverycdays
HABasicModel3 = lm(formula = gmv ~ week + Month + order_date + total + TV + Digital + 
                     Sponsorship + Content_Marketing + Online_Marketing + SEM + 
                     Other + is_sale + product_mrp + promotion_offered + deliverybdays , data = trainBasicHA)
summary(HABasicModel3)
sort(vif(HABasicModel3))

##Removing Content_Marketing
HABasicModel4 = lm(formula = gmv ~ week + Month + order_date + total + TV + Digital + 
                     Sponsorship +  Online_Marketing + SEM + 
                     Other + is_sale + product_mrp + promotion_offered + deliverybdays , data = trainBasicHA)
summary(HABasicModel4)
sort(vif(HABasicModel4))

##Removing total
HABasicModel5 = lm(formula = gmv ~ week + Month + order_date + TV + Digital + 
                     Sponsorship +  Online_Marketing + SEM + 
                     Other + is_sale + product_mrp + promotion_offered + deliverybdays , data = trainBasicHA)
summary(HABasicModel5)
sort(vif(HABasicModel5))

##Removing SEM
HABasicModel6 = lm(formula = gmv ~ week + Month + order_date + TV + Digital + 
                     Sponsorship +  Online_Marketing + Other + is_sale + product_mrp + promotion_offered + deliverybdays , data = trainBasicHA)
summary(HABasicModel6)
sort(vif(HABasicModel6))

##Removing week
HABasicModel7 = lm(formula = gmv ~  Month + order_date + TV + Digital + 
                     Sponsorship +  Online_Marketing + Other + is_sale + product_mrp + promotion_offered + deliverybdays , data = trainBasicHA)
summary(HABasicModel7)
sort(vif(HABasicModel7))

##Removing promotion_offered
HABasicModel8 = lm(formula = gmv ~  Month + order_date + TV + Digital + 
                     Sponsorship +  Online_Marketing + Other + is_sale + product_mrp + deliverybdays , data = trainBasicHA)
summary(HABasicModel8)
sort(vif(HABasicModel8))

##Removing Online_Marketing
HABasicModel9 = lm(formula = gmv ~  Month + order_date + TV + Digital + 
                     Sponsorship +  Other + is_sale + product_mrp + deliverybdays , data = trainBasicHA)
summary(HABasicModel9)
sort(vif(HABasicModel9))

##Removing Sponsorship
HABasicModel10 = lm(formula = gmv ~  Month + order_date + TV + Digital + 
                      Other + is_sale + product_mrp + deliverybdays , data = trainBasicHA)
summary(HABasicModel10)
sort(vif(HABasicModel10))

##Removing Other
HABasicModel11 = lm(formula = gmv ~  Month + order_date + TV + Digital + 
                      is_sale + product_mrp + deliverybdays , data = trainBasicHA)
summary(HABasicModel11)
sort(vif(HABasicModel11))

##Removing order_date
HABasicModel12 = lm(formula = gmv ~  Month +  TV + Digital + 
                      is_sale + product_mrp + deliverybdays , data = trainBasicHA)
summary(HABasicModel12)
sort(vif(HABasicModel12))

##Removing Month
HABasicModel13 = lm(formula = gmv ~  TV + Digital + 
                      is_sale + product_mrp + deliverybdays , data = trainBasicHA)
summary(HABasicModel13)
sort(vif(HABasicModel13))

##Removing deliverybdays
HABasicModel14 = lm(formula = gmv ~  TV + Digital + 
                      is_sale + product_mrp  , data = trainBasicHA)
summary(HABasicModel14)
sort(vif(HABasicModel14))

##Removing TV
HABasicModel15 = lm(formula = gmv ~  Digital + is_sale + product_mrp  , data = trainBasicHA)
summary(HABasicModel15)
sort(vif(HABasicModel15))

##Removing is_sale
HABasicModel16 = lm(formula = gmv ~  Digital +  product_mrp  , data = trainBasicHA)
summary(HABasicModel16)
sort(vif(HABasicModel16))

HABasicFinalModel = HABasicModel16

##Since the remaining variables are significant. Keeping this as final model.
#Multiple R-squared:  0.6932,	Adjusted R-squared:  0.6741 

#Prediction part
predBasicHA =predict(HABasicFinalModel,testBasicHA)
testBasicHA$test_gmv=predBasicHA
r<-cor(testBasicHA$gmv,testBasicHA$test_gmv)
r
#Verifying r-squared value for test data
rsquaredBasic<-cor(testBasicHA$gmv,testBasicHA$test_gmv)^2
rsquaredBasic
#[1] 0.000402007

##The R-squared for the model built on test data set=0.0004
#This means the model is not generalizing well with the prediction.Hence,we cannot implement this model.


######################################
# HA Multiplicative Model
######################################
colnames(consumerdataHAWeekly)

HALogValues <- consumerdataHAWeekly
# Converting some columns to log values. 

names(which(sapply(HALogValues, function(x) any(x == 0))))  
head(HALogValues)

HALogValues[which(HALogValues$TV == 0),]$TV <-  0.001
HALogValues[which(HALogValues$Content_Marketing == 0),]$Content_Marketing <-0.001
HALogValues[which(HALogValues$Radio == 0),]$Radio <- 0.001
HALogValues[which(HALogValues$Other == 0),]$Other <- 0.001
HALogValues[which(HALogValues$deliverybdays == 0),]$deliverybdays <- 0.00001
HALogValues[which(HALogValues$deliverycdays == 0),]$deliverycdays <- 0.00001



colnames(HALogValues)
head(HALogValues)
HALogValues[,c(6:16, 18:26)] <- log(HALogValues[c(6:16, 18:26)])
summary(HALogValues)  
# Replace Infinite values with 0. 
invisible(lapply(names(HALogValues),function(.name) set(HALogValues, which(is.infinite(HALogValues[[.name]])), j = .name,value =0)))
sum(is.na(HALogValues))


# Split into Train and test data.
split= sample(1:nrow(HALogValues), 0.7*nrow(HALogValues))
trainlogHA = HALogValues[split,]
testlogHA = HALogValues[-split,]
head(trainlogHA)
HAMultiModel1<-lm(gmv~.,data=trainlogHA)
summary(HAMultiModel1)
step <- stepAIC(HAMultiModel1, direction="both")
step




HAMultiModel2 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + TV + Digital + Content_Marketing + Online_Marketing + 
                      Affiliates + SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel2)
sort(vif(HAMultiModel2))

# Removing Affiliates - low significance. and high VIF
HAMultiModel3 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + TV + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel3)
sort(vif(HAMultiModel3))


# Removing TV
HAMultiModel4 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel4)
sort(vif(HAMultiModel4))


# Removing NPS
HAMultiModel5 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      total + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel5)
sort(vif(HAMultiModel5))


# Removing total
HAMultiModel6 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel6)
sort(vif(HAMultiModel6))


# Removing product SLA 
HAMultiModel7 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel7)
sort(vif(HAMultiModel7))


#Removing payment type
HAMultiModel8 <- lm(formula = gmv ~ Month + order_date + order_item_id + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel8)
sort(vif(HAMultiModel8))


# Removing promotion offered.
HAMultiModel9 <- lm(formula = gmv ~ Month + order_date + order_item_id + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price  + deliverybdays, 
                    data = trainlogHA)
summary(HAMultiModel9)
sort(vif(HAMultiModel9))




######################################
# HA Kyock Model
######################################
colnames(consumerdataHAWeekly)

HALagValues <- consumerdataHAWeekly
HALagValues = cbind(HALagValues,HAUnits)
colnames(HALagValues)[27] = "units"
#Creating lag vaules for dependent variable i.e. gmv for kyock model
#lag gmv for 1 day,2 day,3 day,1 week before,1 month before
#LAGS of gmvs should be included only for Kyock,distributed and hybrid models;not to be included for basic and multiplicative models

HALagValues$lag_gmv_1day=Lag(HALagValues$gmv,-1)
HALagValues$lag_gmv_2day=Lag(HALagValues$gmv,-2)
HALagValues$lag_gmv_3day=Lag(HALagValues$gmv,-3)
HALagValues$lag_gmv_1week=Lag(HALagValues$gmv,-7)
HALagValues$lag_gmv_2week=Lag(HALagValues$gmv,-14)
HALagValues$lag_gmv_3week=Lag(HALagValues$gmv,-21)
HALagValues$lag_gmv_1month=Lag(HALagValues$gmv,-30)

HALagValues[which(is.na(HALagValues$lag_gmv_1day)),]$lag_gmv_1day <-  0
HALagValues[which(is.na(HALagValues$lag_gmv_2day)),]$lag_gmv_2day <-  0
HALagValues[which(is.na(HALagValues$lag_gmv_3day)),]$lag_gmv_3day <-  0
HALagValues[which(is.na(HALagValues$lag_gmv_1month)),]$lag_gmv_1month <-  0
HALagValues[which(is.na(HALagValues$lag_gmv_1week)),]$lag_gmv_1week <-  0
sum(is.na(HALagValues))


names(which(sapply(HALagValues, function(x) any(is.na(x))))) 
GALagValues[which(is.na(HALagValues$lag_gmv_2week)),]$lag_gmv_2week <-  0
GALagValues[which(is.na(HALagValues$lag_gmv_3week)),]$lag_gmv_3week <-  0 

set.seed(100)

split= sample(1:nrow(HALagValues), 0.7*nrow(HALagValues))
trainlagHA = HALagValues[split,]
testlagHA = HALagValues[-split,]

str(trainlagHA)
HAKyockmodel1<-lm(gmv~units+product_mrp+List_price+total_sla+promotion_offered+lag_gmv_1week,data=trainlagHA)
summary(HAKyockmodel1)
vif(HAKyockmodel1)
step <- stepAIC(HAKyockmodel1, direction="both")


HAKyockmodel2<-lm(gmv~units+product_mrp+List_price,data=trainlagHA)
summary(HAKyockmodel2)
#Multiple R-squared:  0.9709,    Adjusted R-squared:  0.9683 
vif(HAKyockmodel2)

#Prediction part:
pred<-predict(HAKyockmodel2,testlagHA)
testlagHA$test_gmv<-pred
r<-cor(testlagHA$gmv,testlagHA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testlagHA$gmv,testlagHA$test_gmv)^2
rsquared
#[1] 0.9929796

#CV using bootstrap method

#Using 10-fold cv
k_fold_rsq <- function(lmfit, ngroup=10) {
  
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

# fit and call function
lmfit <-HAKyockmodel2
k_fold_rsq(lmfit, ngroup=10)
#0.9709478 0.9492770
#rsquared from cv= 0.9492770



######################################
# HA Distributed lag``
######################################
#lag of product_mrp for 1,2,3 weeks


HALagValues$lag_product_mrp_1week=Lag(HALagValues$product_mrp,-7)
HALagValues$lag_product_mrp_2week=Lag(HALagValues$product_mrp,-14)
HALagValues$lag_product_mrp_3week=Lag(HALagValues$product_mrp,-21)

sum(is.na(HALagValues))

HALagValues[which(is.na(HALagValues$lag_product_mrp_1week)),]$lag_product_mrp_1week <-  0
HALagValues[which(is.na(HALagValues$lag_product_mrp_2week)),]$lag_product_mrp_2week <-  0
HALagValues[which(is.na(HALagValues$lag_product_mrp_3week)),]$lag_product_mrp_3week <-  0

sum(is.na(HALagValues))
#No NAs found


#Creating lags of list price


HALagValues$lag_List_price_1week=Lag(HALagValues$List_price,-7)
HALagValues$lag_List_price_2week=Lag(HALagValues$List_price,-14)
HALagValues$lag_List_price_3week=Lag(HALagValues$List_price,-21)

sum(is.na(HALagValues))

HALagValues[which(is.na(HALagValues$lag_List_price_1week)),]$lag_List_price_1week <-  0
HALagValues[which(is.na(HALagValues$lag_List_price_2week)),]$lag_List_price_2week <-  0
HALagValues[which(is.na(HALagValues$lag_List_price_3week)),]$lag_List_price_3week <-  0

sum(is.na(HALagValues))

#lag of promotion_offered for 1,2,3 weeks


HALagValues$lag_promotion_offered_1week=Lag(HALagValues$promotion_offered,-7)
HALagValues$lag_promotion_offered_2week=Lag(HALagValues$promotion_offered,-14)
HALagValues$lag_promotion_offered_3week=Lag(HALagValues$promotion_offered,-21)

sum(is.na(HALagValues))

HALagValues[which(is.na(HALagValues$lag_promotion_offered_1week)),]$lag_promotion_offered_1week <-  0
HALagValues[which(is.na(HALagValues$lag_promotion_offered_2week)),]$lag_promotion_offered_2week <-  0
HALagValues[which(is.na(HALagValues$lag_promotion_offered_3week)),]$lag_promotion_offered_3week <-  0

sum(is.na(HALagValues))
#No NAs found

#lag of total_sla for 1,2,3 weeks


HALagValues$lag_total_sla_1week=Lag(HALagValues$total_sla,-7)
HALagValues$lag_total_sla_2week=Lag(HALagValues$total_sla,-14)
HALagValues$lag_total_sla_3week=Lag(HALagValues$total_sla,-21)

sum(is.na(HALagValues))

HALagValues[which(is.na(HALagValues$lag_total_sla_1week)),]$lag_total_sla_1week <-  0
HALagValues[which(is.na(HALagValues$lag_total_sla_2week)),]$lag_total_sla_2week <-  0
HALagValues[which(is.na(HALagValues$lag_total_sla_3week)),]$lag_total_sla_3week <-  0

sum(is.na(HALagValues))
#No NAs found

set.seed(100)

split= sample(1:nrow(HALagValues), 0.7*nrow(HALagValues))
trainlagHA = HALagValues[split,]
testlagHA = HALagValues[-split,]


DisLagHAmodel1<-lm(gmv~total_sla +lag_total_sla_1week+lag_total_sla_2week+
                     lag_gmv_1week+lag_gmv_2week+lag_gmv_3week+product_mrp+ lag_product_mrp_1week+
                     List_price+lag_List_price_1week+promotion_offered+lag_promotion_offered_1week,data=trainlagHA)

summary(DisLagHAmodel1)
vif(DisLagHAmodel1)
step <- stepAIC(DisLagHAmodel1, direction="both")

DisLagHAmodel2<-lm(gmv~lag_total_sla_2week+lag_gmv_1week+lag_gmv_2week+product_mrp+ lag_product_mrp_1week+promotion_offered,data=trainlagHA)

summary(DisLagHAmodel2)
vif(DisLagHAmodel2)

#Excluding lag_product_mrp_1week
DisLagHAmodel3<-lm(gmv~lag_total_sla_2week+lag_gmv_1week+lag_gmv_2week+product_mrp+promotion_offered,data=trainlagHA)

summary(DisLagHAmodel3)
vif(DisLagHAmodel3)

#Excluding lag_total_sla_2week 
DisLagHAmodel4<-lm(gmv~lag_gmv_1week+lag_gmv_2week+product_mrp+promotion_offered,data=trainlagHA)

summary(DisLagHAmodel4)
#Multiple R-squared:  0.842,     Adjusted R-squared:  0.8222
vif(DisLagHAmodel4)




#Prediction part:
pred<-predict(DisLagHAmodel4,testlagHA)
testlagHA$test_gmv<-pred
r<-cor(testlagHA$gmv,testlagHA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testlagHA$gmv,testlagHA$test_gmv)^2
rsquared
#[1] 0.1924762



###################################
# Hybrid model for HA
###################################

set.seed(100)

HAHrybridWeekly = merge(consumerdataHAWeekly,HALagValues)
HAHrybridWeekly$log_gmv = log(HAHrybridWeekly$gmv)
HAHrybridWeekly$log_promotion_offered = log(HAHrybridWeekly$promotion_offered)
HAHrybridWeekly$log_tv = log(HAHrybridWeekly$TV)
HAHrybridWeekly$log_digital = log(HAHrybridWeekly$Digital)
HAHrybridWeekly$log_sponsorship = log(HAHrybridWeekly$Sponsorship)
sum(is.na(HAHrybridWeekly))

split= sample(1:nrow(HAHrybridWeekly), 0.7*nrow(HAHrybridWeekly))
trainHybridHA = HAHrybridWeekly[split,]
testHybridHA = HAHrybridWeekly[-split,]


HybridHAmodel1<-lm(gmv~total_sla +lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+lag_gmv_1week+
                     product_mrp+ lag_product_mrp_1week+List_price+lag_List_price_1week+promotion_offered+lag_promotion_offered_1week+log_gmv+
                     log_promotion_offered+log_tv+log_digital+log_sponsorship,data=trainHybridHA)


summary(HybridHAmodel1)
vif(HybridHAmodel1)
step <- stepAIC(HybridHAmodel1, direction="both")

HybridHAmodel2<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_digital+log_sponsorship,data=trainHybridHA)

summary(HybridHAmodel2)
vif(HybridHAmodel2)

#Excluding log_digital
HybridHAmodel3<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_sponsorship,data=trainHybridHA)

summary(HybridHAmodel3)
vif(HybridHAmodel3)

#Excluding log_sponsorship

HybridHAmodel4<-lm(gmv~lag_total_sla_1week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_sponsorship,data=trainHybridHA)

summary(HybridHAmodel4)
vif(HybridHAmodel4)

#Excluding lag_total_sla_2week
HybridHAmodel5<-lm(gmv~lag_total_sla_1week+lag_total_sla_3week+product_mrp+promotion_offered,data=trainHybridHA)

summary(HybridHAmodel5)
vif(HybridHAmodel5)

#Excluding lag_total_sla_1week
HybridHAmodel6<-lm(gmv~lag_total_sla_3week+product_mrp+promotion_offered,data=trainHybridHA)

summary(HybridHAmodel6)
#Multiple R-squared:  0.7631,    Adjusted R-squared:  0.7415 
vif(HybridHAmodel6)



#Prediction part:
pred<-predict(HybridHAmodel6,testHybridHA)
testHybridHA$test_gmv<-pred
r<-cor(testHybridHA$gmv,testHybridHA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testHybridHA$gmv,testHybridHA$test_gmv)^2
rsquared
#[1] 0.03237991


#CV using bootstrap method

#Using 10-fold cv
k_fold_rsq <- function(lmfit, ngroup=10) {
  
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

# fit and call function
lmfit <-HybridHAmodel6
k_fold_rsq(lmfit, ngroup=10)
#0.7630783 0.6614453

#rsquared from cv=0.6614453  

############################################### End of Model for HA ######################################################


####################################################### Modelling for Camera Accessory ##################################################
# Check all the columns.
colnames(consumerdataCA)

str(consumerdataCA$product_analytic_category)
unique(consumerdataCA$product_analytic_category)
# Only CameraAccessory present. Remove this column. 
consumerdataCA$product_analytic_category <- NULL

#  - Aggregation by weeks 
consumerdataCAMeanCols=consumerdataCA%>% group_by(week) %>% 
  summarise_at(.vars=colWhichNeedMean,.funs = mean)

consumerdataCAModeCols=consumerdataCA%>% group_by(week) %>% 
  summarise_at(.vars=colWhicNeedMode,.funs=getmode)

consumerdataCASumCols=consumerdataCA%>% group_by(week) %>% 
  summarise_at(.vars=colWhichNeedSum,.funs = sum)

consumerdataCAWeekly=merge(consumerdataCAModeCols,consumerdataCAMeanCols,by=c("week"))
consumerdataCAWeekly=merge(consumerdataCAWeekly,consumerdataCASumCols,by=c("week"))

colnames(consumerdataCAWeekly)
str(consumerdataCAWeekly)

consumerdataCAWeekly <- consumerdataCAWeekly[order(consumerdataCAWeekly$YearWeek),] 
CAUnits = consumerdataCAWeekly$units

########################### EDA for CA

# Plotting the GMV trend.
ggplot(data = consumerdataCAWeekly, aes(x = YearWeek, y = gmv, group = factor(is_sale), colour = factor(is_sale))) + geom_line() +
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

# No particular overall trend observed. 
# Higher GMV is seen in weeks when there is a sale.

ggplot(data = consumerdataCAWeekly, aes(x = YearWeek, y = total, group = factor(is_sale), colour = factor(is_sale))) + geom_line() +
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

ggplot(data = consumerdataCAWeekly, aes(x = YearWeek, y = total, fill = factor(is_sale))) + geom_bar(stat = "identity") +
  theme_light() + theme(axis.text.x = element_text(angle = 90)) 
# more total investment is seen around sale weeks. 


# Checking by Investment type.
melted = melt(consumerdataCAWeekly[, 7:17], id.vars="YearWeek")
ggplot(data=melted, aes(x=YearWeek, y=value, group=variable, colour = variable)) + geom_line(size = 1) + 
  theme_light() + theme(axis.text.x = element_text(angle = 90))
# More investment is seen in sponsorship.



# Checking relationship between GMV and advertising - taking into consideration sale weeks.
ggplot(data = consumerdataCAWeekly, aes(x = total, y = gmv, group = factor(is_sale), colour = factor(is_sale))) + geom_line() + geom_smooth()+
  theme_light() + theme(axis.text.x = element_text(angle = 90))  # might need to change here. 

# We see an increase in gmv when there is more total investment around sale.
# On non sale days increase in total investment doesnt seem to make much difference in GMV. 

# Check impact of NPS on total GMV
ggplot(data = consumerdataCAWeekly, aes(x = factor(NPS), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()

# Doesnt show any correlation/trend.

# Check relationship between promotion_offered and GMV
ggplot(data = consumerdataCAWeekly, aes(x = factor(round(promotion_offered,2)), y = gmv)) + geom_bar(stat = "identity") + theme_light() +
  theme(axis.text.x = element_text(angle = 90)) 
# In general GMV increases as promotions increase.

# Check relationship between number of units sold and promotions offered.
ggplot(data = consumerdataCAWeekly, aes(x = factor(round(promotion_offered,2)), y = units, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light() +
  theme(axis.text.x = element_text(angle = 90)) 
# Increasing trend. 
# Max units are sold when there is a promotion during a sale day

# Preferred payment type. 
ggplot(data = consumerdataCAWeekly, aes(x = payment_type, fill = factor(is_sale))) + geom_bar()
# One payment type is preferred over the other. 


# Check if the delivery/dispatch delays have any affect on GMV
ggplot(data = consumerdataCAWeekly, aes(x = factor(round(deliverybdays,1)), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()
# max GMV when there is no delay.  - But the data had a lot of noise - cant really conclude. 

ggplot(data = consumerdataCAWeekly, aes(x = factor(round(deliverycdays,1)), y = gmv, fill = factor(is_sale))) + geom_bar(stat = "identity") + theme_light()
# max GMV when there is no delay.  - But the data had a lot of noise - cant really conclude. 


#################################### End of EDA for CA
# YearWeek only created for plotting and ordering. Remove it. 
consumerdataCAWeekly$YearWeek = NULL
head(consumerdataCAWeekly)

# GMV is a combination of List Price and  Units. Removing them from the model. 
# removing Units in model building to avoid overfit. 
consumerdataCAWeekly$units <- NULL


#########################################
# CA Basic Model
#########################################
set.seed(100)

split= sample(1:nrow(consumerdataCAWeekly), 0.7*nrow(consumerdataCAWeekly))
trainBasicCA = consumerdataCAWeekly[split,]
testBasicCA = consumerdataCAWeekly[-split,]

CABasicModel1<-lm(gmv~.,data=trainBasicCA)
summary(CABasicModel1)
step <- stepAIC(CABasicModel1, direction="both")
step

CABasicModel2 = lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                     NPS + total + TV + Digital + Sponsorship + Content_Marketing + 
                     Online_Marketing + SEM + Radio + Other + is_sale + sla + 
                     product_mrp + product_procurement_sla + List_price + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel2)
sort(vif(CABasicModel2))

#Removing Digital
CABasicModel3 = lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                     NPS + total + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + SEM + Radio + Other + is_sale + sla + 
                     product_mrp + product_procurement_sla + List_price + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel3)
sort(vif(CABasicModel3))

#Removing SEM
CABasicModel4 = lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                     NPS + total + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + Radio + Other + is_sale + sla + 
                     product_mrp + product_procurement_sla + List_price + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel4)
sort(vif(CABasicModel4))


#Removing Other
CABasicModel5 = lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                     NPS + total + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + Radio + is_sale + sla + 
                     product_mrp + product_procurement_sla + List_price + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel5)
sort(vif(CABasicModel5))


#Removing total
CABasicModel6 = lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                     NPS + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + Radio + is_sale + sla + 
                     product_mrp + product_procurement_sla + List_price + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel6)
sort(vif(CABasicModel6))


#Removing payment_type
CABasicModel7 = lm(formula = gmv ~ Month + order_date + order_item_id + 
                     NPS + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + Radio + is_sale + sla + 
                     product_mrp + product_procurement_sla + List_price + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel7)
sort(vif(CABasicModel7))

#Removing List_price
CABasicModel8 = lm(formula = gmv ~ Month + order_date + order_item_id + 
                     NPS + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + Radio + is_sale + sla + 
                     product_mrp + product_procurement_sla + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel8)
sort(vif(CABasicModel8))

#Removing order_date
CABasicModel9 = lm(formula = gmv ~ Month + order_item_id + 
                     NPS + TV + Sponsorship + Content_Marketing + 
                     Online_Marketing + Radio + is_sale + sla + 
                     product_mrp + product_procurement_sla + deliverycdays, 
                   data = trainBasicCA)
summary(CABasicModel9)
sort(vif(CABasicModel9))


#Removing Radio
CABasicModel10 = lm(formula = gmv ~ Month + order_item_id + 
                      NPS + TV + Sponsorship + Content_Marketing + 
                      Online_Marketing + is_sale + sla + 
                      product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel10)
sort(vif(CABasicModel10))

#Removing Online_Marketing
CABasicModel11 = lm(formula = gmv ~ Month + order_item_id + 
                      NPS + TV + Sponsorship + Content_Marketing + 
                      is_sale + sla + 
                      product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel11)
sort(vif(CABasicModel11))

#Removing sla
CABasicModel12 = lm(formula = gmv ~ Month + order_item_id + 
                      NPS + TV + Sponsorship + Content_Marketing + 
                      is_sale + product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel12)
sort(vif(CABasicModel12))

#Removing Sponsorship
CABasicModel13 = lm(formula = gmv ~ Month + order_item_id + 
                      NPS + TV + Content_Marketing + 
                      is_sale + product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel13)
sort(vif(CABasicModel13))

#Removing Content_Marketing
CABasicModel14 = lm(formula = gmv ~ Month + order_item_id + NPS+TV + is_sale + product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel14)
sort(vif(CABasicModel14))

#Removing order_item_id
CABasicModel15 = lm(formula = gmv ~ Month +  NPS+TV + is_sale + product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel15)
sort(vif(CABasicModel15))

#Removing TV
CABasicModel16 = lm(formula = gmv ~ Month +  NPS + is_sale + product_mrp + product_procurement_sla + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel16)
sort(vif(CABasicModel16))


#Removing product_procurement_sla +
CABasicModel17 = lm(formula = gmv ~ Month +  NPS + is_sale + product_mrp + deliverycdays, 
                    data = trainBasicCA)
summary(CABasicModel17)
sort(vif(CABasicModel17))

CABasicFinalModel = CABasicModel17
#Multiple R-squared:  0.7429,	Adjusted R-squared:    0.7

#Prediction part
predBasicCA =predict(CABasicFinalModel,testBasicCA)
testBasicCA$test_gmv=predBasicCA
r<-cor(testBasicCA$gmv,testBasicCA$test_gmv)
r
#Verifying r-squared value for test data
rsquaredBasic<-cor(testBasicCA$gmv,testBasicCA$test_gmv)^2
rsquaredBasic
#[1] 0.3505082

##The R-squared for the model built on test data set=0.35
#This means the model is not generalizing well with the prediction.Hence,we cannot implement this model.
######################################
# CA Multiplicative Model
######################################
colnames(consumerdataCAWeekly)

CALogValues <- consumerdataCAWeekly
# Converting some columns to log values. 

names(which(sapply(CALogValues, function(x) any(x == 0))))  
head(CALogValues)

CALogValues[which(CALogValues$TV == 0),]$TV <-  0.001
CALogValues[which(CALogValues$Content_Marketing == 0),]$Content_Marketing <-0.001
CALogValues[which(CALogValues$Radio == 0),]$Radio <- 0.001
CALogValues[which(CALogValues$Other == 0),]$Other <- 0.001
CALogValues[which(CALogValues$deliverybdays == 0),]$deliverybdays <- 0.00001
CALogValues[which(CALogValues$deliverycdays == 0),]$deliverycdays <- 0.00001



colnames(CALogValues)
head(CALogValues)
CALogValues[,c(6:16, 18:26)] <- log(CALogValues[c(6:16, 18:26)])
summary(CALogValues)  
# Replace Infinite values with 0. 
invisible(lapply(names(CALogValues),function(.name) set(CALogValues, which(is.infinite(CALogValues[[.name]])), j = .name,value =0)))
sum(is.na(CALogValues))


# Split into Train and test data.
split= sample(1:nrow(CALogValues), 0.7*nrow(CALogValues))
trainlogCA = CALogValues[split,]
testlogCA = CALogValues[-split,]
head(trainlogCA)
CAMultiModel1<-lm(gmv~.,data=trainlogCA)
summary(CAMultiModel1)
step <- stepAIC(CAMultiModel1, direction="both")
step




CAMultiModel2 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + TV + Digital + Content_Marketing + Online_Marketing + 
                      Affiliates + SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel2)
sort(vif(CAMultiModel2))

# Removing Affiliates - low significance. and high VIF
CAMultiModel3 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + TV + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel3)
sort(vif(CAMultiModel3))


# Removing TV
CAMultiModel4 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      NPS + total + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel4)
sort(vif(CAMultiModel4))


# Removing NPS
CAMultiModel5 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      total + Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel5)
sort(vif(CAMultiModel5))


# Removing total
CAMultiModel6 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other + sla + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel6)
sort(vif(CAMultiModel6))


# Removing product SLA 
CAMultiModel7 <- lm(formula = gmv ~ Month + order_date + order_item_id + payment_type + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel7)
sort(vif(CAMultiModel7))


#Removing payment type
CAMultiModel8 <- lm(formula = gmv ~ Month + order_date + order_item_id + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price + promotion_offered + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel8)
sort(vif(CAMultiModel8))


# Removing promotion offered.
CAMultiModel9 <- lm(formula = gmv ~ Month + order_date + order_item_id + 
                      Digital + Content_Marketing + Online_Marketing + 
                      SEM + Radio + Other  + product_mrp + product_procurement_sla + 
                      total_sla + List_price  + deliverybdays, 
                    data = trainlogCA)
summary(CAMultiModel9)
sort(vif(CAMultiModel9))




######################################
# CA Kyock Model
######################################
colnames(consumerdataCAWeekly)

CALagValues <- consumerdataCAWeekly
CALagValues = cbind(CALagValues,CAUnits)
colnames(CALagValues)[27] = "units"
#Creating lag vaules for dependent variable i.e. gmv for kyock model
#lag gmv for 1 day,2 day,3 day,1 week before,1 month before
#LAGS of gmvs should be included only for Kyock,distributed and hybrid models;not to be included for basic and multiplicative models

CALagValues$lag_gmv_1day=Lag(CALagValues$gmv,-1)
CALagValues$lag_gmv_2day=Lag(CALagValues$gmv,-2)
CALagValues$lag_gmv_3day=Lag(CALagValues$gmv,-3)
CALagValues$lag_gmv_1week=Lag(CALagValues$gmv,-7)
CALagValues$lag_gmv_2week=Lag(CALagValues$gmv,-14)
CALagValues$lag_gmv_3week=Lag(CALagValues$gmv,-21)
CALagValues$lag_gmv_1month=Lag(CALagValues$gmv,-30)

CALagValues[which(is.na(CALagValues$lag_gmv_1day)),]$lag_gmv_1day <-  0
CALagValues[which(is.na(CALagValues$lag_gmv_2day)),]$lag_gmv_2day <-  0
CALagValues[which(is.na(CALagValues$lag_gmv_3day)),]$lag_gmv_3day <-  0
CALagValues[which(is.na(CALagValues$lag_gmv_1month)),]$lag_gmv_1month <-  0
CALagValues[which(is.na(CALagValues$lag_gmv_1week)),]$lag_gmv_1week <-  0
sum(is.na(CALagValues))



set.seed(100)

split= sample(1:nrow(CALagValues), 0.7*nrow(CALagValues))
trainlagCA = CALagValues[split,]
testlagCA = CALagValues[-split,]

str(trainlagCA)
CAKyockModel1<-lm(gmv~units+product_mrp+List_price+total_sla+promotion_offered+lag_gmv_1week,data=trainlagCA)
summary(CAKyockModel1)
vif(CAKyockModel1)
step <- stepAIC(CAKyockModel1, direction="both")
step

CAKyockModel2<-lm(gmv~List_price+product_mrp+units,data=train)
summary(CAKyockModel2)
#Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9462
vif(CAKyockModel2)

#Prediction part:
predKyockCA<-predict(CAKyockModel2,testlagCA)
testlagCA$test_gmv<-predKyockCA
r<-cor(testlagCA$gmv,testlagCA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testlagCA$gmv,testlagCA$test_gmv)^2
rsquared
#[1]  0.9929796


#CV using bootstrap method
library(bootstrap)
#Using 10-fold cv
k_fold_rsq <- function(lmfit, ngroup=10) {
  
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

# fit and call function
lmfit <-CAKyockModel2
k_fold_rsq(lmfit, ngroup=10)
#raw_rsq    cv_rsq 
#0.9709478 0.9492770 
#rsquared from cv=0.9492770 

######################################
# CA Distributed lag``
######################################
#lag of product_mrp for 1,2,3 weeks


CALagValues$lag_product_mrp_1week=Lag(CALagValues$product_mrp,-7)
CALagValues$lag_product_mrp_2week=Lag(CALagValues$product_mrp,-14)
CALagValues$lag_product_mrp_3week=Lag(CALagValues$product_mrp,-21)

sum(is.na(CALagValues))

CALagValues[which(is.na(CALagValues$lag_product_mrp_1week)),]$lag_product_mrp_1week <-  0
CALagValues[which(is.na(CALagValues$lag_product_mrp_2week)),]$lag_product_mrp_2week <-  0
CALagValues[which(is.na(CALagValues$lag_product_mrp_3week)),]$lag_product_mrp_3week <-  0

sum(is.na(CALagValues))
#No NAs found


#Creating lags of list price


CALagValues$lag_List_price_1week=Lag(CALagValues$List_price,-7)
CALagValues$lag_List_price_2week=Lag(CALagValues$List_price,-14)
CALagValues$lag_List_price_3week=Lag(CALagValues$List_price,-21)

sum(is.na(CALagValues))

CALagValues[which(is.na(CALagValues$lag_List_price_1week)),]$lag_List_price_1week <-  0
CALagValues[which(is.na(CALagValues$lag_List_price_2week)),]$lag_List_price_2week <-  0
CALagValues[which(is.na(CALagValues$lag_List_price_3week)),]$lag_List_price_3week <-  0

sum(is.na(CALagValues))

#lag of promotion_offered for 1,2,3 weeks


CALagValues$lag_promotion_offered_1week=Lag(CALagValues$promotion_offered,-7)
CALagValues$lag_promotion_offered_2week=Lag(CALagValues$promotion_offered,-14)
CALagValues$lag_promotion_offered_3week=Lag(CALagValues$promotion_offered,-21)

sum(is.na(CALagValues))

CALagValues[which(is.na(CALagValues$lag_promotion_offered_1week)),]$lag_promotion_offered_1week <-  0
CALagValues[which(is.na(CALagValues$lag_promotion_offered_2week)),]$lag_promotion_offered_2week <-  0
CALagValues[which(is.na(CALagValues$lag_promotion_offered_3week)),]$lag_promotion_offered_3week <-  0

sum(is.na(CALagValues))
#No NAs found

#lag of total_sla for 1,2,3 weeks


CALagValues$lag_total_sla_1week=Lag(CALagValues$total_sla,-7)
CALagValues$lag_total_sla_2week=Lag(CALagValues$total_sla,-14)
CALagValues$lag_total_sla_3week=Lag(CALagValues$total_sla,-21)

sum(is.na(CALagValues))

CALagValues[which(is.na(CALagValues$lag_total_sla_1week)),]$lag_total_sla_1week <-  0
CALagValues[which(is.na(CALagValues$lag_total_sla_2week)),]$lag_total_sla_2week <-  0
CALagValues[which(is.na(CALagValues$lag_total_sla_3week)),]$lag_total_sla_3week <-  0

sum(is.na(CALagValues))
#No NAs found

names(which(sapply(CALagValues, function(x) any(is.na(x))))) 
CALagValues[which(is.na(CALagValues$lag_gmv_2week)),]$lag_gmv_2week <-  0
CALagValues[which(is.na(CALagValues$lag_gmv_3week)),]$lag_gmv_3week <-  0 

set.seed(100)

split= sample(1:nrow(CALagValues), 0.7*nrow(CALagValues))
trainlagCA = CALagValues[split,]
testlagCA = CALagValues[-split,]


DisLagCAmodel1<-lm(gmv~total_sla +lag_total_sla_1week+lag_total_sla_2week+
                     lag_gmv_1week+lag_gmv_2week+lag_gmv_3week+product_mrp+ lag_product_mrp_1week+
                     List_price+lag_List_price_1week+promotion_offered+lag_promotion_offered_1week,data=trainlagCA)

summary(DisLagCAmodel1)
vif(DisLagCAmodel1)
step <- stepAIC(DisLagCAmodel1, direction="both")

DisLagCAmodel2<-lm(gmv~lag_total_sla_2week+lag_gmv_1week+lag_gmv_2week+product_mrp+ lag_product_mrp_1week+
                     promotion_offered,data=trainlagCA)

summary(DisLagCAmodel2)
vif(DisLagCAmodel2)

#Excluding lag_product_mrp_1week
DisLagCAmodel3<-lm(gmv~lag_total_sla_2week+lag_gmv_1week+lag_gmv_2week+product_mrp+ 
                     promotion_offered,data=trainlagCA)

summary(DisLagCAmodel3)
vif(DisLagCAmodel3)

#Excluding lag_total_sla_2week 
DisLagCAmodel4<-lm(gmv~lag_gmv_1week+lag_gmv_2week+product_mrp+ promotion_offered,data=trainlagCA)

summary(DisLagCAmodel4)
#Multiple R-squared:  0.842,     Adjusted R-squared:  0.8222 
vif(DisLagCAmodel4)




#Prediction part:
pred<-predict(DisLagCAmodel4,testlagCA)
testlagCA$test_gmv<-pred
r<-cor(testlagCA$gmv,testlagCA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testlagCA$gmv,testlagCA$test_gmv)^2
rsquared
#[1] 0.1924762



###################################
# Hybrid model for CA
###################################

set.seed(100)

CAHrybridWeekly = merge(consumerdataCAWeekly,CALagValues)
CAHrybridWeekly$log_gmv = log(CAHrybridWeekly$gmv)
CAHrybridWeekly$log_promotion_offered = log(CAHrybridWeekly$promotion_offered)
CAHrybridWeekly$log_tv = log(CAHrybridWeekly$TV)
CAHrybridWeekly$log_digital = log(CAHrybridWeekly$Digital)
CAHrybridWeekly$log_sponsorship = log(CAHrybridWeekly$Sponsorship)
sum(is.na(CAHrybridWeekly))


split= sample(1:nrow(CAHrybridWeekly), 0.7*nrow(CAHrybridWeekly))
trainHybridCA = CAHrybridWeekly[split,]
testHybridCA = CAHrybridWeekly[-split,]


HybridCAmodel1<-lm(gmv~total_sla +lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+lag_gmv_1week+
                     product_mrp+ lag_product_mrp_1week+List_price+lag_List_price_1week+promotion_offered+lag_promotion_offered_1week+log_gmv+
                     log_promotion_offered+log_tv+log_digital+log_sponsorship,data=trainHybridCA)


summary(HybridCAmodel1)
vif(HybridCAmodel1)
step <- stepAIC(HybridCAmodel1, direction="both")

HybridCAmodel2<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_digital+log_sponsorship,data=trainHybridCA)

summary(HybridCAmodel2)
vif(HybridCAmodel2)

#Excluding log_digital
HybridCAmodel3<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered+log_gmv+log_sponsorship,data=trainHybridCA)

summary(HybridCAmodel3)
vif(HybridCAmodel3)

#Excluding log_sponsorship

HybridCAmodel4<-lm(gmv~lag_total_sla_1week+lag_total_sla_2week+lag_total_sla_3week+
                     product_mrp+promotion_offered,data=trainHybridCA)

summary(HybridCAmodel4)
vif(HybridCAmodel4)

#Excluding lag_total_sla_2week
HybridCAmodel5<-lm(gmv~lag_total_sla_1week+lag_total_sla_3week+
                     product_mrp+promotion_offered,data=trainHybridCA)

summary(HybridCAmodel5)
vif(HybridCAmodel5)

#Excluding lag_total_sla_1week
HybridCAmodel6<-lm(gmv~lag_total_sla_3week+product_mrp+promotion_offered,data=trainHybridCA)

summary(HybridCAmodel6)
#Multiple R-squared:  0.7631,    Adjusted R-squared:  0.7415 
vif(HybridCAmodel6)



#Prediction part:
pred<-predict(HybridCAmodel6,testHybridCA)
testHybridCA$test_gmv<-pred
r<-cor(testHybridCA$gmv,testHybridCA$test_gmv)
#Verifying r-squared value for test data
rsquared<-cor(testHybridCA$gmv,testHybridCA$test_gmv)^2
rsquared
#[1] 0.03237991


#CV using bootstrap method

#Using 10-fold cv
k_fold_rsq <- function(lmfit, ngroup=10) {
  
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

# fit and call function
lmfit <-model6
k_fold_rsq(lmfit, ngroup=10)
#0.7630783 0.6614453

#rsquared from cv=0.6614453  

############################################### End of Model for CA ######################################################

