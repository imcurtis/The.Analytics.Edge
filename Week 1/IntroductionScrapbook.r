sd(c(5,8,12))

which.min(c(4,1,6))
WHO <- read.csv("WHO.csv")
str(WHO)
summary(WHO)

which.min(WHO$Over60)
#11.16

WHO[183,]
#UAE

which.max(WHO$LiteracyRate)
WHO[44,]

tapply(WHO$ChildMortality, WHO$Region, mean)



WHO