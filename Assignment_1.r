Week 1

CPS <- read.csv("CPSData.csv")
str(CPS)
summary(CPS)
table(CPS$Race)
#1.5
##The CPS differentiates between race (with possible values American Indian, Asian, Black, 
##Pacific Islander, White, or Multiracial) and ethnicity. A number of interviewees are of 
##Hispanic ethnicity, as captured by the Hispanic variable. For which races are there at 
##least 250 interviewees in the CPS dataset of Hispanic ethnicity? (Select all that apply.)

table(CPS$Race, CPS$Hispanic)

#2.1
##Which variables have at least one interviewee with a missing (NA) value? (Select all that 
##apply.)

table(CPS$PeopleInHousehold)
table(CPS$Region)
summary(CPS)


##2.2
##Often when evaluating a new dataset, we try to identify if there is a pattern in the missing
##values in the dataset. We will try to determine if there is a pattern in the missing values 
##of the Married variable. The function is.na(CPS$Married) returns a vector of TRUE/FALSE 
##values for whether the Married variable is missing. We can see the breakdown of whether 
##Married is missing based on the reported value of the Region variable with the function 
##table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

##2.3
##As mentioned in the variable descriptions, MetroAreaCode is missing if an interviewee does 
##not live in a metropolitan area. Using the same technique as in the previous question, 
##answer the following questions about people who live in non-metropolitan areas.

#How many states had all interviewees living in a non-metropolitan area (aka they have a 
##missing MetroAreaCode value)? For this question, treat the District of Columbia as a state 
##(even though it is not technically a state).

table(CPS$State, is.na(CPS$MetroAreaCode))

##How many states had all interviewees living in a metropolitan area? Again, treat the 
##District of Columbia as a state.

##2.4
##Which region of the United States has the largest proportion of interviewees living in 
##a non-metropolitan area?

table(CPS$Region, is.na(CPS$MetroAreaCode))

##2.5

##While we were able to use the table() command to compute the proportion of interviewees 
##from each region not living in a metropolitan area, it was somewhat tedious (it involved 
##manually computing the proportion for each region) and is not something you would want to 
##do if there were a larger number of options. It turns out there is a less tedious way to 
##compute the proportion of values that are TRUE. The mean() function, which takes the 
##average of the values passed to it, will treat TRUE as 1 and FALSE as 0, meaning it 
##returns the proportion of values that are true. For instance, mean(c(TRUE, FALSE, TRUE, 
##TRUE)) returns 0.75. Knowing this, use tapply() with the mean function to answer the 
##following questions:

##Which state has a proportion of interviewees living in a non-metropolitan area closest 
##to 30%?

tapply(CPS$State, is.na(CPS$MetroAreaCode), mean)
table(CPS$State, is.na(CPS$MetroAreaCode))
tapply(table(CPS$State, is.na(CPS$MetroAreaCode)), mean)
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)

##3.1
##Codes like MetroAreaCode and CountryOfBirthCode are a compact way to encode factor 
##variables with text as their possible values, and they are therefore quite common in 
##survey datasets. In fact, all but one of the variables in this dataset were actually 
##stored by a numeric code in the original CPS datafile.

##When analyzing a variable stored by a numeric code, we will often want to convert it into 
##the values the codes represent. To do this, we will use a dictionary, which maps the the 
##code to the actual value of the variable. We have provided dictionaries MetroAreaCodes.csv
##and CountryCodes.csv, which respectively map MetroAreaCode and CountryOfBirthCode into 
##their true values. Read these two dictionaries into data frames MetroAreaMap and 
##CountryMap.

##How many observations (codes for metropolitan areas) are there in MetroAreaMap?

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryofBirthMap <- read.csv("CountryCodes.csv")

str(MetroAreaMap)
str(CountryofBirthMap)

##3.2
##To merge in the metropolitan areas, we want to connect the field MetroAreaCode from the 
##CPS data frame with the field Code in MetroAreaMap. The following command merges the two 
##data frames on these columns, overwriting the CPS data frame with the result:

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

##The first two arguments determine the data frames to be merged (they are called "x" and 
##"y", respectively, in the subsequent parameters to the merge function). 
##by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the "x" 
##data frame (CPS), while by.y="Code" means we're matching on the Code variable from the 
##"y" data frame (MetroAreaMap). Finally, all.x=TRUE means we want to keep all rows 
##from the "x" data frame (CPS), even if some of the rows' MetroAreaCode doesn't match 
##any codes in MetroAreaMap (for those familiar with database terminology, this parameter 
##makes the operation a left outer join instead of an inner join).

##Review the new version of the CPS data frame with the summary() and str() functions. 
##What is the name of the variable that was added to the data frame by the merge() 
##operation?

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)

##How many interviewees have a missing value for the new metropolitan area variable? 
##Note that all of these interviewees would have been removed from the merged data frame 
##if we did not include the all.x=TRUE parameter.

131302-97064

##3.3

Which of the following metropolitan areas has the largest number of interviewees?

sort(table(CPS$MetroArea))


##3.4

##Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
##Hint: Use tapply() with mean, as in the previous subproblem. Calling sort() on the output
##of tapply() could also be helpful here.

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

##Laredo, TX

##3.5

##Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an 
##interviewee is Asian, determine the number of metropolitan areas in the United States 
##from which at least 20% of interviewees are Asian.

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

##4

##3.6

##Normally, we would look at the sorted proportion of interviewees from each metropolitan 
##area who have not received a high school diploma with the command:

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))

##However, none of the interviewees aged 14 and younger have an education value reported, 
##so the mean value is reported as NA for each metropolitan area. To get mean (and related 
##functions, like sum) to ignore missing values, you can pass the parameter na.rm=TRUE. 
##Passing na.rm=TRUE to the tapply function, determine which metropolitan area has the 
##smallest proportion of interviewees who have received no high school diploma.

rev(sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE)))

##4.1

##Just as we did with the metropolitan area information, merge in the country of birth 
##information from the CountryMap data frame, replacing the CPS data frame with the result. 
##If you accidentally overwrite CPS with the wrong values, remember that you can restore it
##by re-loading the data frame from CPSData.csv and then merging in the metropolitan area
##information using the command provided in the previous subproblem.

CPS = merge(CPS, CountryofBirthMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

str(CPS)

##What is the name of the variable added to the CPS data frame by this merge operation?

Country

##How many interviewees have a missing value for the new country of birth variable?

##4.2

##Among all interviewees born outside of North America, which country was the most 
##common place of birth?

sort(table(CPS$Country))

##Philppines


##4.3

##What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, 
##NY-NJ-PA" metropolitan area have a country of birth that is not the United States? For 
##this computation, don't include people from this metropolitan area who have a missing 
##country of birth.

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

1668/(1668+3736)= .309

##4.4

##Which metropolitan area has the largest number (note -- not proportion) of interviewees 
##with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using 
##tapply() to answer this question.

tail(sort(tapply(CPS$Country=="India", CPS$MetroArea, sum, na.rm=TRUE)))

tail(sort(tapply(CPS$Country=="Brazil", CPS$MetroArea, sum, na.rm=TRUE)))

tail(sort(tapply(CPS$Country=="Somalia", CPS$MetroArea, sum, na.rm=TRUE)))
