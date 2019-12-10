#  Install Requried Packages
install.packages("SnowballC")
install.packages("ROAuth")
install.packages("tm")
install.packages("stringr")
install.packages("wordcloud")
install.packages("twitteR")
install.packages("RColorBrewer")

# Load Requried Packages
library("SnowballC")
library("ROAuth")
library("stringr")
library("tm")
library("twitteR")
library("wordcloud")
library("RColorBrewer")

#call the created app from twitter by its four variables
consumer_key <- 'HoXSXueE7vDP6xGjxVvmDIbv5'
consumer_secret <- 'NxgnxU0IvYT8fcomHVxanGftaduhizCAPacy3FaC8d6mTeYTWz'
access_token <- '2257992877-1eBzBTeQNKz3aPsdwLf5PchuPVZDzS1vtf889TR'
access_secret <- 'v557kPKyBSIOV9rYB25qXTJ7BBs9fujR2wOoFXOdFen20'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- searchTwitter("#indianrailway",1000,lang="en")

#write tweets in new text file
getwd()
setwd(getwd())
getwd()

name <- file(description="namo.txt", open="w")
write.csv(twListToDF(tweets),file=name)
close (con=name)

#processing of tweets
tweets <- scan ("namo.txt", what=character(0), sep="\n")
tweets <- Corpus(VectorSource(tweets))
inspect(tweets)
tospace <- content_transformer(function(x,pattern)dsub(pattern,"",x))
tweets <- tm_map(tweets,toSpace,"/")
tweets <- tm_map(tweets,toSpace,"@")
tweets <- tm_map(tweets,toSpace,"\\|")
tweets <- tm_map(tweets,toSpace,"href")
tweets <- tm_map(tweets,content_transformer(tolower))
tweets <- tm_map(tweets, removeNumbers)
tweets <- tm_map(tweets, removeWords,stopwords("english"))
tweets <- tm_map(tweets, removeWords,c("blabla1","blabla2"))
tweets <- tm_map(tweets, removePunctuation)
tweets <- tm_map(tweets, stripWhitespace)
tweets <- tm_map(tweets, stemDocument)
removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
tweets <- tm_map(tweets, removeURL)
myStopwords <- c(stopwords('english'),"download","twittercom","android","falsenana", "href","relnofollowtwitt","falsenana","falsena","truenana","indianrailway")
tweets <- tm_map (tweets, removeWords, myStopwords)

#Create the term document matrix

dtm <- TermDocumentMatrix(tweets)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word=names(v), freq=v)
head (d,20)

#Create the word cloud
set.seed(1234)
wordcloud(word=d$word, freq=d$freq, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8,"Dark2"))

#Create the bar plot of frequent words
findFreqTerms(dtm,lowfreq=4)
findAssocs(dtm,terms="freedom", corlimit = 0.3)
head(d,10)
barplot(d[1:10,]$freq, las=2, names.arg=d[1:10,]$word, col="lightblue", main="Most frequent words",
ylab="Word frequencies")
