#1.1
statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

#1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#2.1
polling <- read.csv("PollingImputed.csv")

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
mean(predictionDataFrame$TestPrediction)

table(TestPredictionBinary)
#2.2
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)

str(statesMap)

#2.4
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

#2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#3.2
table(TestPredictionBinary$Florida)
TestPrediction
str(TestPrediction)
summary(TestPrediction)
predictionDataFrame

#4.1
?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary, size = 3)) + geom_polygon(color = "black")
?par
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary, alpha = 0.3)) + geom_polygon(color = "black")
