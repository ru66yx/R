rm(list=ls())
parole <- read.csv("parole.csv", header = TRUE)
str(parole)
table(parole$violator)
summary (parole)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
mod1<- glm(violator ~., data = train, family = "binomial")
summary(mod1)
exp(1.6119919) # odds multiple offender
ndata = data.frame (male = 1, race = 1, age =50, state=1,time.served =3,max.sentence =12,multiple.offenses =0, crime =2)
ndata$state <- as.factor (ndata$state)
ndata$crime <- as.factor (ndata$crime)
ndataPredict <-predict(mod1, ndata, type = "response" ) #.154
odds1 <-ndataPredict / (1 - ndataPredict)
testPredict <- predict(mod1, newdata = test, type = "response")
max(testPredict) #  0.9072791
table(test$violator, testPredict >= 0.5)
sens <- 12 / (11 +12)
spec <- 167 / (167 + 12)
acc <- (167 +12)/202
bas <- 179 /202
library(ROCR)
# ROCRPred <- prediction(predictTrain, qualityTrain$PoorCare)
# ROCRperf <- performance(ROCRPred, "tpr","fpr")
# plot(ROCRperf)
# plot(ROCRperf, colorize = TRUE)
# plot(ROCRperf, colorize = TRUE, print.cutoffs.at= seq(0,1,0.1), text.adj = c(-0.2,1.7))
ROCRpredTest = prediction(testPredict, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
