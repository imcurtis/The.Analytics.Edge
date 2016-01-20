pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

1.1
#How many students are there in the training set?

str(pisaTrain)

1.2
#Using tapply() on pisaTrain, what is the average reading test score of males?

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

1.3
#Which variables are missing data in at least one observation in the training set? 
#Select all that apply.

summary(pisaTrain)

#1.4

pisaTrain = na.omit(pisaTrain)
str(pisaTrain)

pisaTest = na.omit(pisaTest)
str(pisaTest)

#How many observations are now in the training set?

#How many observations are now in the testing set?

#2.1

#Which of the following variables is an unordered factor with at least 3 levels?
#raceeth

#Which of the following variables is an ordered factor with at least 3 levels?
#grade

#2.2
#Which binary variables will be included in the regression model?

#All except raceethWhite

#2.3
#For a student who is Asian, which binary variables would be set to 0?

#All except raceethAsian

#For a student who is white, which binary variables would be set to 0?

#All

#3.1

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ .,data = pisaTrain)

summary(lmScore)

#Multiple R-squared=0.3251

#3.2

RMSE <- sqrt(mean((y-y_pred)^2))

SSE = sum(lmScore$residuals^2)

RMSE = sqrt(SSE / nrow(pisaTrain))

RMSE = sqrt(12993365 / nrow(pisaTrain))


sqrt(mean(lmScore$residuals^2))

#4.1

What is the range between the maximum and minimum predicted reading score on the test set?

newdata = pisaTrain

predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

637.7-353.2
#Remember, it's the test set, not the training set

#4.2

##What is the sum of squared errors (SSE) of lmScore on the testing set?

lmScore = lm(readingScore ~ .,data = pisaTest)

sum((predTest-pisaTest$readingScore)^2, na.rm=TRUE)

#What is the root-mean squared error (RMSE) of lmScore on the testing set?

sqrt(mean((predTest-pisaTest$readingScore)^2, na.rm=TRUE))

#4.3

predTrain = predict(lmScore, newdata=pisaTrain)

mean(pisaTrain$readingScore)

summary(predTrain)

baseline = mean(pisaTrain$readingScore)

##What is the sum of squared errors of the baseline model on the testing set?

sum((baseline-pisaTest$readingScore)^2)

predTest = predict(lmScore, newdata=pisaTest)

mean(pisaTest$readingScore)

#4.4

#What is the test-set R-squared value of lmScore?

#Test-Set R-Squared is '1-SSE/SST'

1- 5762082/7802354






















