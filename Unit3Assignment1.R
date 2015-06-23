songs <- read.csv ("songs.csv", header = TRUE)
set10 <- subset(songs, year == 2010)
Mick <-subset(songs, artistname == "Michael Jackson")
Mick10 <-subset(songs, artistname == "Michael Jackson" & Top10 == 1)
str(songs$timesignature)
table(songs$timesignature)
str(songs)
which.max(songs$tempo)
songs[6206,]
SongsTrain <-subset(songs, year <= 2009)
SongsTest <- set10 
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
cor(SongsTrain$loudness, SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
testPrediction <- predict(SongsLog3, newdata = SongsTest, type = "response")
table(SongsTest$Top10, testPrediction >= 0.45)
accuracy = 328/373
baselineAccuracy = 314/373
sensitivity  = 19 /(40 + 19)
specificity = 309 /(309 + 5)
