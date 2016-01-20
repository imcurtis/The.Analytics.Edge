#1.1

IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")


#Our five datasets all have the same number of observations. 
#How many observations are there in each data set?

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(Boeing)

#1.2

#What is the earliest year in our datasets?

#1.3

#What is the latest year in our datasets?

tail(Boeing)

#1.4

#What is the mean stock price of IBM over this time period?

mean(IBM$StockPrice)

#1.5

#What is the minimum stock price of General Electric (GE) over this time period?

summary(GE$StockPrice)

#1.6

#What is the maximum stock price of Coca-Cola over this time period?

summary(CocaCola$StockPrice)

#1.7

#What is the median stock price of Boeing over this time period?

summary(Boeing$StockPrice)

#1.8

#What is the standard deviation of the stock price of Procter & Gamble over this time period?

sd(ProcterGamble$StockPrice)

#2.1

plot(CocaCola$Date, CocaCola$StockPrice, type="l")

#Use plot to answer questions about high and lows

#2.2

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

lines(ProcterGamble$Date, ProcterGamble$StockPrice)

abline(v=as.Date(c("2000-03-01")), lwd=2)

#2.3

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

lines(ProcterGamble$Date, ProcterGamble$StockPrice)

abline(v=as.Date(c("1983-01-01")), lwd=2)

#3.1

#Which stock fell the most right after the technology bubble burst in March 2000?

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="orange", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="green", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="black", ylim=c(0,210))

#3.2

#Which stock reaches the highest value in the time period 1995-2005?

#3.3

#3.4

#boeing

#3.5

#4.1

tapply(IBM$StockPrice, months(IBM$Date), mean)

summary(IBM$StockPrice)

#In which months has IBM historically had a higher stock price (on average)?

#4.2

tapply(GE$StockPrice, months(GE$Date), mean)

summary(GE$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)

summary(CocaColca$StockPrice)

#General Electric and Coca-Cola both have their highest average stock price in the 
#same month. Which month is this?

#April

#4.3

#In which month are the stock prices lower?

tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)

#December
