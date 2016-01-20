mvt <- read.csv("mvtWeek1.csv")

#1.1

#How many rows of data (observations) are in this dataset?

summary(mvt)
str(mvt)

#1.2

#How many variables are in this dataset?

#1.3

#Using the "max" function, what is the maximum value of the variable "ID"?

which.max(mvt$ID)

summary(mvt)

#1.4

#What is the minimum value of the variable "Beat"?

#1.5

#How many observations have value TRUE in the Arrest variable (this is the number of crimes 
#for which an arrest was made)?

#15536

#1.6

#How many observations have a LocationDescription value of ALLEY?

#2308

#2.2

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

#What is the month and year of the median date in our dataset?

summary(DateConvert)

#May 2006

#2.3

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

table(mvt$Date)

#In which month did the fewest motor vehicle thefts occur?

table(mvt$Month)

#2.4

#On which weekday did the most motor vehicle thefts occur?

table(mvt$Weekday)

#Friday

#2.5

#Which month has the largest number of motor vehicle thefts for which an arrest was made?

table(mvt$Month, mvt$Arrest)

#3.1

hist(mvt$Date, breaks=100)

#3.2

boxplot(mvt$Arrest)
?boxplot


ArrestYear = table(mvt$Arrest, mvt$Year)

2152 + 2115 + 1798 + 1693 + 1528 + 1302

1212 + 1020 + 840 + 701 + 625 + 550

#You can do it the above way, or the way below

boxplot(mvt$Date ~ mvt$Arrest)

#3.3

#For what proportion of motor vehicle thefts in 2001 was an arrest made?

table(mvt$Arrest, mvt$Year)

2152/(18517+2152)

#0.1041173

#3.4

#For what proportion of motor vehicle thefts in 2007 was an arrest made?

1212/(1212+13068)

#0.08487395

#3.5

#For what proportion of motor vehicle thefts in 2012 was an arrest made?

550/(550+13542)

#0.03902924

#4.1

sort(table(mvt$LocationDescription))

#4.2

Top5

#How many observations are in Top5?

156564 + 14852 + 2308 + 2111 + 1675

Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription==
"PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription
=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")

Top5

summary(Top5)
str(Top5)

#4.3

Top5$LocationDescription = factor(Top5$LocationDescription)

str(Top5)
table(Top5)

#One of the locations has a much higher arrest rate than the other locations. Which is it?

table(Top5$LocationDescription, Top5$Arrest)

#Gas Station

#4.4

#On which day of the week do the most motor vehicle thefts at gas stations happen?

Top5$Weekdays

table(Top5$Weekday, Top5$LocationDescription)

#Saturday

#4.5

#On which day of the week do the fewest motor vehicle thefts in residential driveways 
#happen?

#Saturday











