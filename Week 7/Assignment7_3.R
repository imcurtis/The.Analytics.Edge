#1.1
tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

ncol(allTweets)

#2.1
?wordcloud
install.packages("wordcloud")

#2.2
colSums(allTweets)
rowSums(allTweets)

#2.3
??wordcloud
install.packages(c("wordcloud","tm"),repos="http://cran.r-project.org")
library(wordcloud)
library(tm)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets))

#2.4
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets))

#3.1
??wordcloud
wordcloud(colnames(allTweets), colSums(allTweets))

#3.3
wordcloud(colnames(allTweets), colSums(allTweets), color=purple, random.color=TRUE)

#4.1
display.brewer.all()

#4.3
wordcloud(colnames(allTweets), colSums(allTweets), colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)], random.color=TRUE)
