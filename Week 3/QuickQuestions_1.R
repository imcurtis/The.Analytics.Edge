quality = read.csv("quality.csv")

install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)

QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
QualityLog


10/25



install.packages("ROCR")
library("ROCR")
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)


QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTest)
QualityLog


predictTest = predict(QualityLog, type="response", newdata=qualityTest)
predictTest

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


#Answer = 0.7994792


11/(187+11)
1069/1075
