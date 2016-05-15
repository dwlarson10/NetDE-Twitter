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

consumer_key <- "wOj58vqE8onMQYAFBeAnUgHcC"
consumer_secret <- "8Hh8l7FWeZoXjHV4emQ4EwQ9yBFMZjIYhlAt15O5vP4rNfawMa"
access_token <- "32600877-SUEQzAuJDuZPEdAvorNzX3usuLAENHTPmGVk0eXuF"
access_secret <- "PA16VhIdjerpVprzcRD6bZl4YlXervAUlQSPeztvAyA70"




setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

day <- data.frame(seq.Date(from = as.Date('2016-05-04'),to = as.Date('2016-05-11'),by = 'day'))
df <- data.frame()
for (i in day){
        df <- data.frame()
        j = as.character(as.Date(i,origin="1970-01-01")+1)
        list <- searchTwitter(searchString = "#netDE",n=1500, since = as.character(i), until= j)
        a<-twListToDF(list)
        df<-rbind(df,a)
}

df <- rbind(
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-04', until= '2016-05-05')),
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-05', until= '2016-05-06')),
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-06', until= '2016-05-07')),
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-07', until= '2016-05-08')),
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-08', until= '2016-05-09')),
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-09', until= '2016-05-10')),
        twListToDF(searchTwitter(searchString = "#netDE",n=1500, since = '2016-05-10', until= '2016-05-11'))
        )


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
# Create corpus
corpus=Corpus(VectorSource(data$tweet))
#Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)


tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet_list, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet_list)
data$tweet=tweet


positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")


sentiment_scores = function(tweets, positive_words, negative_words, .progress='none'){
        scores = laply(tweets,
                       function(tweet, positive_words, negative_words){
                               tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                               tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                               tweet = gsub('\\d+', '', tweet)          # remove digits
                               
                               # Let's have error handling function when trying tolower
                               tryTolower = function(x){
                                       # create missing value
                                       y = NA
                                       # tryCatch error
                                       try_error = tryCatch(tolower(x), error=function(e) e)
                                       # if not an error
                                       if (!inherits(try_error, "error"))
                                               y = tolower(x)
                                       # result
                                       return(y)
                               }
                               # use tryTolower with sapply
                               tweet = sapply(tweet, tryTolower)
                               # split sentence into words with str_split function from stringr package
                               word_list = str_split(tweet, "\\s+")
                               words = unlist(word_list)
                               # compare words to the dictionaries of positive & negative terms
                               positive_matches = match(words, positive_words)
                               negative_matches = match(words, negative_words)
                               # get the position of the matched term or NA
                               # we just want a TRUE/FALSE
                               positive_matches = !is.na(positive_matches)
                               negative_matches = !is.na(negative_matches)
                               # final score
                               score = sum(positive_matches) - sum(negative_matches)
                               return(score)
                       }, positive_matches, negative_matches, .progress=.progress )
        return(scores)
}

score = sentiment_scores(tweet, positives, negatives, .progress='text')
data$score=score


