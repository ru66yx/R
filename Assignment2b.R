pisaTest <- read.csv("pisa2009test.csv", header = TRUE)
pisaTrain <- read.csv("pisa2009train.csv", header = TRUE)
summary(pisaTrain)
tapply(pisaTrain$readingScore,pisaTrain$male,mean)
table(is.na(pisaTrain[,24])) 
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
SSE = sum((lmScore$residuals)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTrain))
mean(pisaTrain$readingScore)
RMSE
# grade9 = subset(pisaTrain,pisaTrain$grade==9)
# grade11 = subset(pisaTrain,pisaTrain$grade==11)
# mean(predict(lmScore, newdata = grade11))
# mean(predict(lmScore, newdata = grade9))
predTest <- predict(lmScore, newdata = pisaTest)
summary(predTest)
637.7 -353.2
sSE = sum((predTest - pisaTest$readingScore)^2)
sSE
sST = sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2 )
1 - sSE/sST
rMSE<- sqrt(sSE/nrow(pisaTest))
rMSE
mean(pisaTest$grade)
