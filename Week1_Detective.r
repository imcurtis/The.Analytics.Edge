mvt <- read.csv("mvtWeek1.csv")
summary(mvt)
str(mvt)
nrow(mvt)
max(mvt$ID)
which.min(mvt$Beat)
summary(mvt)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Date)
table(mvt$Month)
table(mvt$Weekday)
table(mvt$Arrest) ##2.4
table(mvt$Month, mvt$Arrest) ##2.5

hist(mvt$Date, breaks=100)
boxplot(mvt$Date, mvt$Arrest)
boxplot(mvt$Date ~ mvt$Arrest) ##3.2

table(mvt$Year, mvt$Arrest) ##3.3-3.5

sort(table(mvt$LocationDescription))

Top5 = subset(mvt, LocationDescription=="STREET" | ##4.2
LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | ##4.2
LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | ##4.2
LocationDescription=="DRIVEWAY - RESIDENTIAL") ##4.2

Top5$LocationDescription = factor(Top5$LocationDescription)

str(Top5)
table(Top5)

table(Top5$LocationDescription, Top5$Arrest) ##4.3
table(Top5$LocationDescription, Top5$Weekday)
