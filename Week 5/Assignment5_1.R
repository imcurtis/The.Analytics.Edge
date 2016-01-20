#1.1

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
sum(wiki$Vandal == 1)

#1.2
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, c(stopwords("english")))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#1.3
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
wordsAdded

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#1.5
wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal = wiki$Vandal

library(caTools)

set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

wikiTrain = subset(wikiWords, split==TRUE)
wikiTest = subset(wikiWords, split==FALSE)

table(wikiTrain$Vandal)
2061/(2061+1815)

#1.6
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., wikiTrain, method="class")

prp(wikiCART)

predictions = predict(wikiCART, newdata=wikiTest, type="class")

table(wikiTest$Vandal, predictions)

(618+12)/(612+12+533)

#2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

sum(wikiWords2$HTTP == 1)

#2.2
split

wikiTrain2 = subset(wikiWords2, split==TRUE)

wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~., wikiTrain2, method="class")

predictions = predict(wikiCART2, newdata=wikiTest2, type="class")

table(wikiTest2$Vandal, predictions)

(609+57)/(609+57+488+9)

#2.3

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

#2.4

wikiTrain2 = subset(wikiWords2, split==TRUE)

wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~., wikiTrain2, method="class")

prp(wikiCART2)

predictions = predict(wikiCART2, newdata=wikiTest2, type="class")

table(wikiTest2$Vandal, predictions)
(514+248)/(514+248+104+297)

#3.1
wikiWords3 = wikiWords2

#Then add the two original variables Minor and Loggedin to this new data frame:
  
wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split==TRUE)

wikiTest3 = subset(wikiWords3, split==FALSE)

wikiCART3 = rpart(Vandal ~., wikiTrain3, method="class")

prp(wikiCART3)

predictions = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, predictions)
(595+241)/(595+241+304+23)

















