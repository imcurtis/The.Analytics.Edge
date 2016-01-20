#1.1
#Looking at the time period 2004-2011, which week corresponds to the highest percentage of 
#ILI-related physician visits?

FluTrain <- read.csv("FluTrain.csv")
summary(FluTrain)

Highest <- max(FluTrain$ILI)

?which

which.max(FluTrain$ILI)
(FluTrain$Week[which.max(FluTrain$ILI)])

#Which week corresponds to the highest percentage of ILI-related query fraction?
(FluTrain$Week[which.max(FluTrain$Queries)])

#1.2
#Let us now understand the data at an aggregate level. Plot the histogram of the dependent 
#variable, ILI. What best describes the distribution of values of ILI?

plot(hist(FluTrain$ILI))

#Skew Right

#1.3

#Plot the natural logarithm of ILI versus Queries. What does the plot suggest?

plot(log(FluTrain$ILI), FluTrain$Queries)

#Positive linear relationship

#2.1

##Based on our understanding of the data from the previous subproblem, which model best 
##describes our estimation problem?

#log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

#We are predicting log(ILI) using the Queries variable

#2.2

#Let's call the regression model from the previous problem (Problem 2.1) FluTrend1 
#and run it in R.

#What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
FluTrend1
summary(FluTrend1)

#2.3
#What is the relationship we infer from our [the previous] problem?

cor(FluTrain$ILI, FluTrain$Queries)
#0.8142115
#0.8142115^2 = 0.6629404 == R-squared = Cor^2

log(1/0.8142115)
exp((-0.5)*.8142115)

Correlation<-cor(FluTrain$ILI, FluTrain$Queries)

Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation^2

#3.1

FluTest <- read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

#What is our estimate for the percentage of ILI-related physician visits for the week 
#of March 11, 2012? 

which(FluTest$Week == "2012-03-11 - 2012-03-17")

PredTest1[11]

#What is the relative error betweeen the estimate (our prediction) and the observed 
#value for the week of March 11, 2012? Note that the relative error is calculated as

#(Observed ILI - Estimated ILI)/Observed ILI

FluTest$ILI[11] = 2.293422
RelativeError= (2.293422-2.187378)/2.293422
RelativeError

#3.3
#What is the Root Mean Square Error (RMSE) between our estimates and the actual 
#observations for the percentage of ILI-related physician visits, on the test set?

#First calculate SSE

SSE = sum((PredTest1-FluTest$ILI)^2)

#Divide by number of observations

RMSE = sqrt(SSE / nrow(FluTest))
RMSE

#4.1

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

#How many values are missing in the new ILILag2 variable?

(FluTrain$ILILag2)

#output of summary
summary(FluTrain$ILILag2)

#4.2

#Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best 
#describes the relationship between these two variables?

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

#4.3

#Train a linear regression model on the FluTrain dataset to predict the log of the ILI 
#variable using the Queries variable as well as the log of the ILILag2 variable. 
#Call this model FluTrend2.

#Which coefficients are significant at the p=0.05 level in this regression model?

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
FluTrend2
summary(FluTrend2)

#4.4

#FluTrend2 is a stronger model

#5.1

#Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest 
#data frame. How many missing values are there in this new variable?

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

summary(FluTest$ILILag2)
head(FluTest)
head(FluTrain)
tail(FluTrain)

#5.2

#Which value should be used to fill in the ILILag2 variable for the first observation 
#in FluTest?

#Which value should be used to fill in the ILILag2 variable for the second observation 
#in FluTest?

#5.3

#Fill in the missing values for ILILag2 in FluTest.

FluTest$ILILag2[x] = FluTrain$ILI[y]

FluTest$ILILag2[1] = FluTrain$ILI[416]

FluTest$ILILag2[2] = FluTrain$ILI[417]

#Read from ILI not ILILag2

#5.4

#Obtain test set predictions of the ILI variable from the FluTrend2 model, again 
#remembering to call the exp() function on the result of the predict() function to 
#obtain predictions for ILI instead of log(ILI).

#What is the test-set RMSE of the FluTrend2 model?

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

SSE = sum(na.omit(PredTest2-FluTest$ILI)^2)

RMSE = sqrt(SSE / nrow(FluTest))
RMSE
#0.2942029

#5.5
#Which model obtained the best test-set RMSE?

#FluTrend2

exp(-1)
1/(1+exp(1))
11/(11+187)
1069/(1069+6)


