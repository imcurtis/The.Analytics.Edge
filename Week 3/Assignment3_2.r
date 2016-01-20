#1.1

#How many parolees are contained in the dataset?

parole <- read.csv("parole.csv")
str(parole)

#675

#1.2

#How many of the parolees in the dataset violated the terms of their parole?

  sum(parole$violator == 1)

#78

#or
table(parole$violator)

#2.1

#Which variables in this dataset are unordered factors with at least three levels?

#state
#crime

#2.2

#Using the as.factor() function, convert these variables to factors.

as.factor(parole$state)
as.factor(parole$crime)

summary(parole$state)
summary(as.factor(parole$state))

#The output becomes similar to that of the table() function applied to that variable

#3.1 and 3.2

set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

View(test)

#4.1

set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

#Using glm (and remembering the parameter family="binomial"), train a logistic regression 
#model on the training set.

mod = glm(violator~., data=train, family="binomial")

summary(mod)

#race
#state4
#multple.offenses

#4.2
exp(1.61)

ln(odds of A) = ln(odds of B) + 1.61

exp(ln(odds of A)) = exp(ln(odds of B) + 1.61)

exp(ln(odds of A)) = exp(ln(odds of B)) * exp(1.61)

odds of A = exp(1.61) * odds of B

odds of A= 5.01 * odds of B

#4.3

#Consider...

#According to the model, what are the odds this individual is a violator?

mod <- glm(violator ~ ., data=train, family="binomial")
summary(mod)

Logit = -4.2411574 + 0.8867192*1 + 0.3869904*1 - 0.0001756*50 - 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143
Odds = exp(Logit)
Odds
P = 1/(1+1/Odds)
P
odds=pred/(1-pred)

# Significant predictors
id <- which(summary(mod)$coeff[,4] < 0.05)
# Cofficients of the significant predictors
coeff.sig <- summary(mod)$coeff[,1][id]

# A parolee who is male, of white race, aged 50 years at prison release, from the state 
#of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit 
#multiple offenses, and committed a larceny. Obtain odds and probability that he is a 
#violator. From the logistic regression equation, we have 
#log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2
#+ 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 
#1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4. 
#This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, 
#max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0. We conclude that 
#log(odds) = -1.700629.

#5.1

#Use the predict() function to obtain the model's predicted probabilities for parolees 
#in the testing set, remembering to pass type="response".

#What is the maximum predicted probability of a violation?

predTest<-predict(mod, newdata=test, type="response")
max(predTest)

#5.2

table(test$violator, testPred > 0.5)

#What is the model's sensitivity?

#What is the model's specificity?

#What is the model's accuracy?

# Sensitivity, Specificity and Accuracy
12/23; 167/179; 179/202

#5.3

table(test$violator)
179/202

#5.6

#Using the ROCR package, what is the AUC value for the model?

library(ROCR)

pred = prediction(predictions, test$violator)

auc=as.numeric(performance(pred, "auc")@y.values)
auc

#My data is messed up

#5.7












