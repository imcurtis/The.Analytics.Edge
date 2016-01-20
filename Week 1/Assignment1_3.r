#1.1

#How many interviewees are in the dataset?

CPS <- read.csv("CPSData.csv")

str(CPS)

#1.2

#Among the interviewees with a value reported for the Industry variable, what is the 
#most common industry of employment?

sort(head(CPS$Industry))

#I did it using sort(head(CPS$Industry)), but you could also do table(CPS$Industry)

#1.3

sort(table(CPS$State))

#Which state has the fewest interviewees?

#New Mexico

#Which state has the largest number of interviewees?

#California

#1.4

#What proportion of interviewees are citizens of the United States?

table(CPS$Citizenship)

(116639+7073)/(7590+116639+7073)

#0.9421943

#1.5

#For which races are there at least 250 interviewees in the CPS dataset of 
#Hispanic ethnicity?

table(CPS$Race, CPS$Hispanic)

#American Indian, Black, Multiracial, White

#2.1

#Which variables have at least one interviewee with a missing (NA) value?

str(CPS)
summary(CPS)

#2.2

table(CPS$Region, is.na(CPS$Married))

#Age

#Should have run tests for each characteristic

#2.3

#How many states had all interviewees living in a non-metropolitan area (aka they have a 
#missing MetroAreaCode value)?

table(CPS$State, is.na(CPS$MetroAreaCode))

#How many states had all interviewees living in a non-metropolitan area (aka they have 
#a missing MetroAreaCode value)?

#2

#How many states had all interviewees living in a metropolitan area?

#3

#2.4

#Which region of the United States has the largest proportion of interviewees living in 
#a non-metropolitan area?

table(CPS$Region, is.na(CPS$MetroAreaCode))

#2.5

#Which state has a proportion of interviewees living in a non-metropolitan area closest 
#to 30%?

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

#3.1

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

#How many observations (codes for metropolitan areas) are there in MetroAreaMap?

str(MetroAreaCodes)

#How many observations (codes for countries) are there in CountryMap?

str(CountryCodes)

#3.2

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

summary(CPS)

#3.3

#Which of the following metropolitan areas has the largest number of interviewees?

sort(table(CPS$MetroArea))

#3.4

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#3.5

#Determine the number of metropolitan areas in the United States from which at least 20% 
#of interviewees are Asian.

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

#3.6

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))

#4.1

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

#Name of new variable: Country

summary(CPS)

#4.2

#Among all interviewees born outside of North America, which country was the most 
#common place of birth?

sort(table(CPS$Country))

#4.3

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, 
#NY-NJ-PA" metropolitan area have a country of birth that is not the United States?

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
  
#0.309

#4.4

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))











