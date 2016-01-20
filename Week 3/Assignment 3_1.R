songs <- read.csv("songs.csv")
summary(songs)

#1.1
#How many observations (songs) are from the year 2010?
table(songs$year)
#373Using glm (and remembering the parameter family="binomial"), train a logistic regression model on the training set. 

#1.2
#How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(songs$artistname)
#You can also do the following: 
#MichaelJackson = subset(songs, artistname == "Michael Jackson")

#1.3
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)

table(MichaelJackson$Top10)
MichaelJackson$Top10

Top10 = subset(MichaelJackson, Top10 = "1")
Top10

tapply(MichaelJackson$songtitle, MichaelJackson$Top10)

sort(unique(songs$artistname))

Top10 <- c(MichaelJackson$songti)

JacksonHits <- (MichaelJackson$Top10 == 1)
JacksonSongs <- (MichaelJackson$songtitle[JacksonHits])
JacksonSongs

HitSongs <- (songs$artistname == "Michael Jackson" & songs$Top10 == "1")
sum(HitSongs)
HitSongz <- (songs$songtitle[HitSongs])
HitSongz

#1.4
#What are the values of this variable (timesignature) that occur in our dataset?

sort(unique(songs$timesignature))

#Which timesignature value is the most frequent among songs in our dataset?

which.max(songs$timesignature)

table(songs$timesignature)

#1.5

#Out of all of the songs in our dataset, the song with the highest tempo is one of the 
#following songs. Which one is it?

which.max(songs$tempo)
songs$songtitle[6206]

#Wanna Be Startin' Somethin'

#2.1

#We wish to predict whether or not a song will make it to the Top 10. To do this, first 
#use the subset function to split the data into a training set "SongsTrain" consisting 
#of all the observations up to and including 2009 song releases, and a testing set 
#"SongsTest", consisting of the 2010 song releases.

#How many observations (songs) are in the training set?

SongsTrain = subset(songs, year <= 2009)

SongsTest = subset(songs, year == 2010)

str(SongsTrain)

#2.2

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

summary(SongsLog1)

#3.1

#What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness, SongsTrain$energy)

#3.2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#3.3

#Create model 3
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#4.1

#Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the 
#test set, using a threshold of 0.45? (Compute the accuracy as a number between 0 and 1.)

testPredict = predict(SongsLog3, newdata=SongsTest, type="response")

table(SongsTest$Top10, testPredict >= 0.45)

(309+19)/(309+19+40+5)

#4.2

baseline = mean(SongsTest$Top10)
baseline

1 - baseline

table(SongsTest$Top10)

59/(315)

#4.3
#How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all 
#songs in 2010 went into our test set), using a threshold of 0.45?

table(SongsTest$Top10, testPredict >= 0.45)

#4.4

What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?

# Sensitivity = TP/( TP + FN)
19/59

What is the specificity of Model 3 on the test set, using a threshold of 0.45?

# Specificity = TN/( TN + FP)
309/314


















