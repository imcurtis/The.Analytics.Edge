##Problem 1
Movies <- read.csv("Movies.csv")
str(Movies)

MoviesTrain <- subset(Movies, Year < 2010)
MoviesTest <- subset(Movies, Year >= 2010)
str(MoviesTrain)
str(MoviesTest)

##Problem 2

##We don't want to randomly split our data here, so the sample.split function is not appropriate.

##Problem 3
lmMoviesTrain = lm(Worldwide ~ Rated + Runtime + Action + Adventure + Crime + Drama + Thriller + Fantasy + Horror + Sci.Fi + Comedy + Family + Mystery + Romance + Animation + Music + History + Documentary + Wins + Nominations + Production.Budget, data=MoviesTrain)
summary(lmMoviesTrain)

##Problem 5
cor(MoviesTrain$Worldwide, MoviesTrain$Production.Budget)

##Problem 6
lmMoviesTrain2 = lm(Worldwide ~ Runtime + Crime + Horror + Animation + History + Nominations + Production.Budget, data=MoviesTrain)
summary(lmMoviesTrain2)

##Problem 7
0.465257

##Problem 8
SSE = sum((MoviesTrain$Worldwide)^2)
SSE
predTest = predict(lmMoviesTrain2, newdata=MoviesTest)
summary(predTest)
lmScore = lm(readingScore ~ .,data = pisaTest)

sum((predTest-MoviesTest$Worldwide)^2, na.rm=TRUE)

baseline = mean(MoviesTrain$Worldwide)
SST = sum((baseline - MoviesTest$Worldwide)^2)
SST

lmMoviesTest = lm(Worldwide ~ Runtime + Crime + Horror + Animation + History + Nominations + Production.Budget, data=MoviesTrain)
summary(lmMoviesTest)

R2 = 1 - (SSE/SST)
R2
25.0006/60.43398
1-(25.0006/60.43398)

##Problem 10
Movies$Performance = factor(ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .75), "Excellent", ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .25), "Average", "Poor")))
sum(Movies$Performance == "Average")
sum(Movies$Performance == "Excellent")
Movies$Worldwide = NULL

library(caTools)
set.seed(15071)
split = sample.split(Movies$Performance, SplitRatio = 0.7)

trainPerf = subset(Movies, split==TRUE)
testPerf = subset(Movies, split==FALSE)

##Problem 11
require(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
str(MoviesTrain)
CARTMoviesTrain3 = rpart(Performance ~ Rated + Runtime + Action + Adventure + Crime + Drama + Thriller + Fantasy + Horror + Sci.Fi + Comedy + Family + Mystery + Romance + Animation + Music + History + Documentary + Wins + Nominations + Production.Budget, data=Movies, method="class")
CARTMoviesTrain3
prp(CARTMoviesTrain3)

##Problem 12
predCART = predict(CARTMoviesTrain3, newdata = Movies, type= "class")
table(MoviesTrain$Performance, predCART)
