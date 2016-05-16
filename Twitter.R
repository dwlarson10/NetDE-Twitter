install.packages(c("devtools", "rjson", "bit64", "httr"))

library(devtools)
install_github("twitteR", username="geoffjentry")


#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
require(RCurl)

#initialize the twitter app authentications
consumer_key <- "wOj58vqE8onMQYAFBeAnUgHcC"
consumer_secret <- "8Hh8l7FWeZoXjHV4emQ4EwQ9yBFMZjIYhlAt15O5vP4rNfawMa"
access_token <- "32600877-SUEQzAuJDuZPEdAvorNzX3usuLAENHTPmGVk0eXuF"
access_secret <- "PA16VhIdjerpVprzcRD6bZl4YlXervAUlQSPeztvAyA70"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Pull Data -- I wasn't able to get a for loop to work that pulled the data for each day.
#I also had some trouble wragling the data as a dataframe. That is why I have done wrangling as individual lists. 
#If/when I do another twitter project this will be an area I hope to improve

list1 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-10', until= '2016-05-11')


DE1Tweet=sapply(list1, function(x) x$getText())
DE1date=lapply(list1, function(x) x$getCreated())
DE1date=sapply(DE1date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet1=sapply(list1, function(x) x$getIsRetweet())
retweeted1=sapply(list1, function(x) x$getRetweeted())
retweetcount1=sapply(list1, function(x) x$getRetweetCount())

favoritecount1=sapply(list1, function(x) x$getFavoriteCount())
favorited1=sapply(list1, function(x) x$getFavorited())

data=as.data.frame(cbind(tweet=DE1Tweet,date=DE1date,
                         isretweet=isretweet1,retweeted=retweeted1, retweetcount=retweetcount1,favoritecount=favoritecount1,favorited=favorited1))


list2 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-08', until= '2016-05-09')


DE2Tweet=sapply(list2, function(x) x$getText())
DE2date=lapply(list2, function(x) x$getCreated())
DE2date=sapply(DE2date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet2=sapply(list2, function(x) x$getIsRetweet())
retweeted2=sapply(list2, function(x) x$getRetweeted())
retweetcount2=sapply(list2, function(x) x$getRetweetCount())

favoritecount2=sapply(list2, function(x) x$getFavoriteCount())
favorited2=sapply(list2, function(x) x$getFavorited())

data2=as.data.frame(cbind(tweet=DE2Tweet,date=DE2date,
                         isretweet=isretweet2,retweeted=retweeted2, retweetcount=retweetcount2,favoritecount=favoritecount2,favorited=favorited2))


list3 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-09', until= '2016-05-10')


DE3Tweet=sapply(list3, function(x) x$getText())
DE3date=lapply(list3, function(x) x$getCreated())
DE3date=sapply(DE3date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet3=sapply(list3, function(x) x$getIsRetweet())
retweeted3=sapply(list3, function(x) x$getRetweeted())
retweetcount3=sapply(list3, function(x) x$getRetweetCount())

favoritecount3=sapply(list3, function(x) x$getFavoriteCount())
favorited3=sapply(list3, function(x) x$getFavorited())

data3=as.data.frame(cbind(tweet=DE3Tweet,date=DE3date,
                          isretweet=isretweet3,retweeted=retweeted3, retweetcount=retweetcount3,favoritecount=favoritecount3,favorited=favorited3))

list4 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-10', until= '2016-05-11')


DE4Tweet=sapply(list4, function(x) x$getText())
DE4date=lapply(list4, function(x) x$getCreated())
DE4date=sapply(DE4date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet4=sapply(list4, function(x) x$getIsRetweet())
retweeted4=sapply(list4, function(x) x$getRetweeted())
retweetcount4=sapply(list4, function(x) x$getRetweetCount())

favoritecount4=sapply(list4, function(x) x$getFavoriteCount())
favorited4=sapply(list4, function(x) x$getFavorited())

data4=as.data.frame(cbind(tweet=DE4Tweet,date=DE4date,
                          isretweet=isretweet4,retweeted=retweeted4, retweetcount=retweetcount4,favoritecount=favoritecount4,favorited=favorited4))

list5 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-11', until= '2016-05-12')

DE5Tweet=sapply(list5, function(x) x$getText())
DE5date=lapply(list5, function(x) x$getCreated())
DE5date=sapply(DE5date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet5=sapply(list5, function(x) x$getIsRetweet())
retweeted5=sapply(list5, function(x) x$getRetweeted())
retweetcount5=sapply(list5, function(x) x$getRetweetCount())

favoritecount5=sapply(list5, function(x) x$getFavoriteCount())
favorited5=sapply(list5, function(x) x$getFavorited())

data5=as.data.frame(cbind(tweet=DE5Tweet,date=DE5date,
                          isretweet=isretweet5,retweeted=retweeted5, retweetcount=retweetcount5,favoritecount=favoritecount5,favorited=favorited5))


list6 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-12', until= '2016-05-13')

DE6Tweet=sapply(list6, function(x) x$getText())
DE6date=lapply(list6, function(x) x$getCreated())
DE6date=sapply(DE6date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet6=sapply(list6, function(x) x$getIsRetweet())
retweeted6=sapply(list6, function(x) x$getRetweeted())
retweetcount6=sapply(list6, function(x) x$getRetweetCount())

favoritecount6=sapply(list6, function(x) x$getFavoriteCount())
favorited6=sapply(list6, function(x) x$getFavorited())

data6=as.data.frame(cbind(tweet=DE6Tweet,date=DE6date,
                          isretweet=isretweet6,retweeted=retweeted6, retweetcount=retweetcount6,favoritecount=favoritecount6,favorited=favorited6))

list7 <- searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-13', until= '2016-05-14')

DE7Tweet=sapply(list7, function(x) x$getText())
DE7date=lapply(list7, function(x) x$getCreated())
DE7date=sapply(DE7date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet7=sapply(list7, function(x) x$getIsRetweet())
retweeted7=sapply(list7, function(x) x$getRetweeted())
retweetcount7=sapply(list7, function(x) x$getRetweetCount())

favoritecount7=sapply(list7, function(x) x$getFavoriteCount())
favorited7=sapply(list7, function(x) x$getFavorited())

data7=as.data.frame(cbind(tweet=DE7Tweet,date=DE7date,
                          isretweet=isretweet7,retweeted=retweeted7, retweetcount=retweetcount7,favoritecount=favoritecount7,favorited=favorited7))

data = rbind(data,data2,data3,data4,data5,data6,data7)


#wrote the data into a CSV and did wrangling/plotting in another R script
write.csv(data,file='data.csv')


