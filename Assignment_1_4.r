##Assignment 1 part 4

read.csv("AnonymityPoll.csv")

#1.1
poll <- read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)

#1.2
##Let's look at the breakdown of the number of people with smartphones using the table() and
##summary() commands on the Smartphone variable. (HINT: These three numbers should sum to
##1002.)
table((poll$Smartphone))
summary((poll$Smartphone))

#1.3
table(poll$State, poll$Region)

#2.1
##How many interviewees reported not having used the Internet and not 
##having used a smartphone?

table(poll$Internet.Use, poll$Smartphone)

#2.2
##How many interviewees have a missing value for their Internet use?

table(poll$Internet.Use)
summary(poll$Internet.Use)

##How many interviewees have a missing value for their smartphone use?
summary((poll$Smartphone))

##2.3
#How many interviewees are in the new data frame?

limited = subset(poll, Internet.Use==1 | Smartphone==1)
limited
nrow(limited)

##3.1
#Which variables have missing values in the limited data frame? (Select all that apply.)

summary(limited)

##3.2
##What is the average number of pieces of personal information on the Internet, according 
##to the Info.On.Internet variable?

mean(limited$Info.On.Internet)
summary(limited$Info.On.Internet)

##3.3
##How many interviewees reported a value of 0 for Info.On.Internet?

table(limited$Info.On.Internet)

##How many interviewees reported the maximum value of 11 for Info.On.Internet?

table(limited$Info.On.Internet)

##3.4
##What proportion of interviewees who answered the Worry.About.Info question worry 
##about how much information is available about them on the Internet?

table(limited$Worry.About.Info)

386/(386+404)

##3.5
#What proportion of interviewees who answered the Anonymity.Possible question think 
#it is possible to be completely anonymous on the Internet?

table(limited$Anonymity.Possible)

278/(475+278)

##3.6
#What proportion of interviewees who answered the Tried.Masking.Identity question have 
#tried masking their identity on the Internet?

summary(limited$Tried.Masking.Identity)

table(limited$Tried.Masking.Identity)

128/(128+656)

##3.7

##What proportion of interviewees who answered the Privacy.Laws.Effective question find 
##United States privacy laws effective?

summary(limited$Privacy.Laws.Effective)

##4.1

##Build a histogram of the age of interviewees. What is the best represented age 
##group in the population?

hist(limited$Age)

##4.2

plot(limited$Age, limited$Info.On.Internet)

table(limited$Age)

table(limited$Info.On.Internet)

table(limited$Age, limited$Info.On.Internet)

#or

max(table(limited$Age, limited$Info.On.Internet))

##4.3
##Experimenting with the command jitter(c(1, 2, 3)), what appears to be the functionality 
##of the jitter command?

jitter(c(1, 2, 3))

##4.4
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

##4.5
#What is the average Info.On.Internet value for smartphone users?

tapply(limited$Info.On.Internet, limited$Smartphone)

summary(tapply(limited$Info.On.Internet, limited$Smartphone))

tapply(limited$Smartphone, limited$Info.On.Internet)

summary(tapply(limited$Smartphone, limited$Info.On.Internet))

summary(tapply(limited$Smartphone==1, limited$Info.On.Internet))

##acutal answer
tapply(limited$Info.On.Internet, limited$Smartphone, summary)

##4.6
##Similarly use tapply to break down the Tried.Masking.Identity variable for smartphone 
##and non-smartphone users.

##What proportion of smartphone users who answered the Tried.Masking.Identity question 
##have tried masking their identity when using the Internet?

tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)











