#1.1

loans <- read.csv("loans.csv")
summary(loans)

#What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
#0.1601

#1.3

is.na(loans$pub.rec)

#We want to be able to predict risk for all borrowers, instead of just the ones 
#with all data reported.

#1.4

install.packages("mice")

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

======================================

loans_imputed <- read.csv("loans_imputed.csv")

summary(loans_imputed)

#We predicted missing variable values using the available independent variables for 
#each observation.

#2.1

set.seed(144)

library(caTools)

split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)

train = subset(loans_imputed, split == TRUE)

test = subset(loans_imputed, split == FALSE)

#build model using training set
mod = glm(not.fully.paid ~ ., data=train, family="binomial")

summary(mod)

#2.2

log(odds) = 9.187+(-.3368)*1+(-.6141)*1262+(-.3212)*3957+(-.4830)*437+.412*619+.001275*installment+(-.4337)*log.annual.inc+(-.009317)*fico+.000003085*revol.bal+.08437*inq.last.6mths+.3300*pub.rec

#What is the value of Logit(A) - Logit(B)?
-9.317e-03*(700-710)
#0.09317

#What is the value of O(A)/O(B)?
exp(-9.317e-03 * (700-710))

#2.3

#What is the accuracy of the logistic regression model?
predicted.risk <- predict(mod, type="response", newdata=test)
test$predicted.risk = predicted.risk
table(test$not.fully.paid, predicted.risk > 0.5)

(2400+3)/(2400+3+457+13)

#What is the accuracy of the baseline model?

table(test$not.fully.paid)

2413/(2413+460)

#2.4

library(ROCR)

pred = prediction(predicted.risk, test$not.fully.paid)

auc=as.numeric(performance(pred, "auc")@y.values)
auc

#3.1

mod1 <- glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(mod1)

#3.2

#What is the highest predicted probability of a loan not being paid 
#in full on the testing set?

pred.bivariate=predict(mod1, type="response", newdata=test)
summary(pred.bivariate)

#With a logistic regression cutoff of 0.5, how many loans would be predicted as not 
#being paid in full on the testing set?

table(test$not.fully.paid, pred.bivariate >= 0.5)

#3.3

#What is the test set AUC of the bivariate model?

library(ROCR)

pred = prediction(pred.bivariate, test$not.fully.paid)

auc=as.numeric(performance(pred, "auc")@y.values)
auc

#4.1

#How much does a $10 investment with an annual interest rate of 6% pay back after 
#3 years, using continuous compounding of interest?

10*exp(.06*3)

#5.1

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
#max profit of $10 on $10 investment

summary(test$profit)
10*max(test$profit)

#6.1

#What is the average profit of a $1 investment in one of these high-interest loans?

highInterest = subset(test, int.rate >= .15)
summary(highInterest)

#6.2

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans = subset(highInterest, predicted.risk <= 0.1763305)
selectedLoans

selectedLoans1 = highInterest[order(highInterest$predicted.risk)[1:100],]
sum(selectedLoans1$profit)
prop.table(table(selectedLoans1$not.fully.paid))
summary(selectedLoans$profit)
