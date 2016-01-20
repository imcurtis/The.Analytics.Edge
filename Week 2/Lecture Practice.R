baseball <- read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD, moneyball$W)
WinsREG <- lm(formula = W ~ RD, data=moneyball)
summary(WinsREG)

(99-80.8814)/0.1058
99*.1058+80.8814

str(moneyball)

RunsREG = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsREG)

-804.63+(2737.77*.311)+(1584.91*.405)

-837.38+2913.60*.297+1514.29*.370


teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)





