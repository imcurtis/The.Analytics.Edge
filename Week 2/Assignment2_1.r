climate1 <- read.csv("climate_change.csv")
climate_trainingset = subset(climate1, Year <= 2006)
str(climate_trainingset)
climate_trainingset$Year

climate_testingset = subset(climate1, Year > 2006)
str(climate_testingset)

#1.1-1.2

TempDep <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_trainingset)
summary(TempDep)

#2.2
N2O = climate_testingset$N2O
MEI = climate_testingset$MEI
CO2 = climate_testingset$CO2
CH4 = climate_testingset$CH4
CFC.11 = climate_testingset$CFC.11
CFC.12 = climate_testingset$CFC.12
Aerosols = climate_testingset$Aerosols
TSI = climate_testingset$TSI
cor(N2O, MEI)
cor(N2O, CO2)
cor(N2O, CH4)
cor(N2O,CFC.11)
cor(N2O,CFC.12)
cor(N2O, Aerosols)
cor(N2O, TSI)

cor(CFC.11, MEI)
cor(CFC.11, CO2)
cor(CFC.11, CH4)
cor(CFC.11, N2O)
cor(CFC.11,CFC.12)
cor(CFC.11, Aerosols)
cor(CFC.11, TSI)

cor(climate_trainingset)

MEI

#3

TempDepRed <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_trainingset)
summary(TempDepRed)

#4
TempDep <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_trainingset)

TempDep2 <- step(TempDep)
summary(TempDep2)

N2O

#5
##R^2 for testing set
TempDep <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_testingset)
summary(TempDep)


