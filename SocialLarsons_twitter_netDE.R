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

#loadData
data <- read.csv("data.csv",header=T)


#Removed all emojis from the tweets. R did not like to process them. 
data$tweet <- sapply(data$tweet,function(row) iconv(row, "latin1", "ASCII", sub=""))


retweet <- tbl_df(data)%>%group_by(isretweet)%>%summarise(tweets=n())

TorR<- ggplot(data=retweet,aes(x=isretweet,y=tweets)) +
  geom_bar(stat="identity")+
  theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.position="none") +
  # Set title and axis labels, and format these and tick marks
  ggtitle("#netDE Tweet or Retweet") +
  theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
  ylab("Frequency") +
  xlab("type") +
  scale_x_discrete(breaks=c("FALSE", "TRUE"),
                   labels=c("Tweet", "Retweet"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  # Big bold line at y=0
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(TorR,file="TorT.png",width = 10,height = 10)


data$day <- as.Date(data$date,format="%Y-%m-%d")





day <- tbl_df(data)%>%group_by(day)%>%summarise(tweets=n())



plotDays <- ggplot(data=day,aes(x=day)) +
  geom_line(aes(y=tweets),size=1,colour = "#7fbf7f") +
  geom_line(aes(y=mean(as.numeric(tweets))),size=1.5,colour="#faaca7")+
  theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.position="none") +
  # Set title and axis labels, and format these and tick marks
  ggtitle("#netDE Tweets per Day") +
  theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
  ylab("Frequency") +
  xlab("Date") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  # Big bold line at y=0
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(plotDays,file="days.png",width = 10,height = 10)


data$time <- strftime(data$date, format="%H",tz = "UTC")

time <- tbl_df(data)%>%group_by(time)%>%summarise(tweets=n(),fav = mean(as.numeric(favoritecount),na.rm=T),retweet=mean(as.numeric(retweetcount)))

RandF <- ggplot(data=time,aes(x=time)) +
  geom_line(aes(y=fav,group = 1,colour="#faaca7"),size=1)+
  geom_line(aes(y=retweet,group = 1,colour="#7fbf7f"),size=1)+
  scale_colour_manual(name="Engagement",
                      values=c("#faaca7", "#7fbf7f"),
                      labels=c("Favorites", "Retweets"))+
  theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  # Set title and axis labels, and format these and tick marks
  ggtitle("Average Number of Favorites and Retweets for #netDE \n by Hour of the Day") +
  theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
  ylab("Average") +
  xlab("Hour of Day (24 Hour)") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  # Big bold line at y=0
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x='22',y=6,label="Favorites",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(RandF,file="RandF.png",width = 10,height = 10)


# Create corpus
corpus=Corpus(VectorSource(data$tweet))

#Convert to lower-case
corpus=tm_map(corpus,tolower,mc.cores=1)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wc<-wordcloud(corpus, min.freq=15, scale=c(5,2),rot.per = 0.25,
              random.color=T, max.word=45, random.order=F,colors=col)

ggsave(wc,file="wc.png",width = 10,height = 10)





tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet_list, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet_list)
data$tweet=tweet





positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")


sentiment_scores = function(tweets, positive_words, negative_words){
  scores = lapply(tweets,
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
                  }, positive_words, negative_words )
  return(scores)
}

score = sentiment_scores(tweet, positives, negatives)
data$score=as.numeric(score)

tscore <- tbl_df(data)%>%group_by(score)%>%summarise(tweets=n())


sentiment<- ggplot(data=tscore,aes(x=score,y=tweets)) +
  geom_bar(stat="identity")+
  theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.position="none") +
  # Set title and axis labels, and format these and tick marks
  ggtitle("Sentiment Scores of #netDE Tweets") +
  theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
  ylab("Frequency") +
  xlab("Sentiment Score") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  # Big bold line at y=0
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")



ggsave(sentiment,file="sentiment.png",width = 10,height = 10)





mean(data$score)
