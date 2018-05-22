#Loading libraries

library(recommenderlab)
library(data.table)
library(dplyr)


#Loading data set

data=fread("C:/Users/Sudipto Kumar/Desktop/UPGRAD/Recommender System Project/beer_data.csv")


**************************************************************************
#Data preparation:

#Filtering out the data set for both beer id and user id to reduce the no of NAs for similarity matrix.
#This will help us to get rid off the missing values,and decrease the computational time for building the model.
#I have used the following thresholds for filteting the data:
1.Considering a minimum no of ratings i.e. 50 for beers,filtering out rest of the beer ids,
2.A minimum no of rating i.e. 30 for users,and filtering out rest of the user ids.

#Aggregating beer ids having min rating counts 50

agg_items <- data %>%
            group_by(beer_beerid) %>%
            summarize(count=n(), avg_rating = mean(review_overall)) %>%
            filter(count >= 50) %>%
            arrange(avg_rating)

#Aggregating user ids having min rating counts 30


agg_users<- data %>%
            group_by(review_profilename) %>%
            summarize(count=n(), avg_rating = mean(review_overall)) %>%
            filter(count >= 30) %>%
            arrange(avg_rating)

new_data<-as.data.frame(agg_items)
new_data$count=NULL
new_data$avg_rating=NULL

#Merging with original data set
merged_data<-merge(data,new_data,by="beer_beerid")


new_user=as.data.frame(agg_users)

new_user$count=NULL
new_user$avg_rating=NULL

#Building the final data set

final_data<-merge(merged_data,new_user,by="review_profilename")


#Renaming the final data frame as master_file
master_file<-data.frame(user=final_data$review_profilename,item=final_data$beer_beerid,rating=final_data$review_overall)

#Converting the data frame to realratingMatrix

r <- as(master_file, "realRatingMatrix")


************************************************************************************
#Data Exploration
# get some informtaion
dim(r)
[1] 3178 2069

dimnames(r)
rowCounts(r)
colCounts(r)
rowMeans(r)


#How similar the first ten users are with each other and visualise it


similar_users <- similarity(r[1:10, ],
                               method = "cosine",
                               which = "users")


#Similarity matrix
as.matrix(similar_users)

#Few Nas observed

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Inference 
#Users 1, 3 and 8 are similar

#How similar are the first ten beers are with each other

similar_items <- similarity(r[,1:10 ],
                            method = "cosine",
                            which = "items")
#Similarity matrix
as.matrix(similar_items)

#No Nas found

#Visualise similarity matrix
image(as.matrix(similar_items), main = "Item similarity")

#We can see that items 1 and 4 are similar.

#Finding the unique values of rating
unique(master_file$rating)
[1] 4.5 3.5 5.0 4.0 3.0 2.5 1.5 2.0 1.0

#Total 9 unique values

************************************************************************************************************
#Visualizing the ratings for users and beers

library(ggplot2)

#Average beer ratings

qplot(colCounts(r), binwidth = 10, 
      main = "Average beer rating", 
      xlab = "# of users", 
      ylab = "# of beers rated")


#Most users has rated less number of beers.
#Very few users have rated more beers.


#Average user ratings

qplot(rowCounts(r), binwidth=10,
      main = "Average user rating", 
      xlab = "# of users", 
      ylab = "# of ratings")

#No of ratings per users is less.
#No of ratings decreases for more no of users.


#Average number of ratings given to the beers


qplot(colMeans(r), 
      main = "Average number of ratings given to the beers", 
      xlab = "ratings", 
      ylab = "avg no of ratings for beers")

#We can find from the graph that beers with rating 4 have got the maximum average no of ratings.

#Average number of ratings given by the users


qplot(rowMeans(r), 
      main = "Average number of ratings given by the users", 
      xlab = "ratings", 
      ylab = "avg no of ratings by users")

#Maximum average no of ratings given by users is for rating 4.



***************************************************************************************************
#Recommendation Models using UBCF and IBCF
         
#train and test
#Here we create an evaluation scheme which splits the users 
#into a training set (90%) and a test set (10%). 

#given 
#For the test set 2 items(given argument) will be given to the
#recommender algorithm and the other items will be held out for computing the error

#With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
#positives in the evaluation process

#Divide data into trian-test,building model with method="split"
scheme <- evaluationScheme(r, method = "split", train = .9,
                           k = 1, given = 2, goodRating = 4)

scheme




*********************************************************************************
#Building reco systems
r1<-Recommender(getData(scheme, "train"), "UBCF") 
r2 <- Recommender(getData(scheme, "train"), "IBCF")
 
#Predictions
p1 <- predict(r1, getData(scheme, "known"), type="ratings") 
p2<- predict(r2, getData(scheme, "known"), type="ratings")

#Now,we are calculating the error between the prediction and the unknown part of the test data.
#Thus,we are checking the performance of the 2 models


error_split <- rbind( UBCF = calcPredictionAccuracy(p1, getData(scheme, "unknown")),  IBCF = calcPredictionAccuracy(p2, getData(scheme, "unknown"))  ) 

error_split
          RMSE       MSE       MAE
UBCF 0.8186623 0.6702080 0.6036056
IBCF 0.9387974 0.8813406 0.6864278

#RMSE for ubcf is less compared to ibcf.Hence,we should implement ubcf here.

#ROC metric analysis

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)




# running algorithms, predicting next n items

results_split <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15))

class(results_split)

results_split

avg(results_split)


# Plotting ROC curve
plot(results_split, annotate = 1:4, legend="topleft")

#UBCF is higher than IBCF,hence,we should select ubcf for implementing our models.

**********************************************************************************************
#Building reco systems using Cross-validation evaluation scheme
#We are using k=4 here

scheme <- evaluationScheme(r, method = "cross", 
                           k = 4, given = 2, goodRating = 4)

scheme

#Building reco systems
r1<-Recommender(getData(scheme, "train"), "UBCF") 
r2 <- Recommender(getData(scheme, "train"), "IBCF")
 
#Predictions
p1 <- predict(r1, getData(scheme, "known"), type="ratings") 
p2<- predict(r2, getData(scheme, "known"), type="ratings")

#Checking the performance of the 2 models

error_kfold <- rbind( UBCF = calcPredictionAccuracy(p1, getData(scheme, "unknown")),  IBCF = calcPredictionAccuracy(p2, getData(scheme, "unknown"))  ) 


error_kfold

          RMSE       MSE       MAE
UBCF 0.8870771 0.7869058 0.6487591
IBCF 1.0600974 1.1238065 0.7486891

#RMSE for UBCF is less compared to IBCF.Hence,we should implement UBCF.


#ROC analysis


algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# running algorithms, predicting next n items

results_kfold <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15))

class(results_kfold)

results_kfold

avg(results_kfold)


# Plotting ROC curve
plot(results_kfold, annotate = 1:4, legend="topleft")

#Here,UBCF is higher than IBCF.Hence,we should implement UBCF for model building


*******************************************************************************************
#Recommending top 5 beers for users "cokes", "genog" & "giblet"

rownames(r)

#cokes=660;genog=1131;giblet=1148

#We should implement UBCF as per our analysis

#Recommendation model

r1=Recommender(r, method = "UBCF")

#Predicting the top 5 beers for the users

pred_cokes <- predict(r1, r[660,], n=5) 
 
pred_genog <- predict(r1, r[1131,], n=5) 

pred_giblet <- predict(r1, r[1148,], n=5) 

#Displaying the recommended beers
as(pred_cokes, "list")
$cokes
[1] "1153"  "131"   "311"   "857"   "12690"

as(pred_genog, "list")
$genog
[1] "991"   "104"   "20604" "22505" "36342"

as(pred_giblet, "list")
$giblet
[1] "3158"  "56973" "1256"  "412"   "34" 






 



