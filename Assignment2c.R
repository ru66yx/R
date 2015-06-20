FluTrain = read.csv("FluTrain.csv", header = T)
summary(FluTrain)
which.max(FluTrain$ILI)
FluTrain[303,]
which.max(FluTrain$Queries)
hist(FluTrain$ILI)
logil <- log(FluTrain$ILI)
plot(FluTrain$Queries,logil )
# FluTrend1 <- lm(log(FluTrain$ILI) ~ FluTrain$Queries)
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
#FluTrend2 <- lm(FluTrain$Queries ~log(FluTrain$ILI) )
summary(FluTrend1)
#summary(FluTrend2)
cor(logil ,FluTrain$Queries)
cor(logil ,FluTrain$Queries)^2
FluTest = read.csv("FluTest.csv", header = T)
summary(FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
FluTest$Week
(-PredTest1[11]+FluTest$ILI[11])/FluTest$ILI[11]
PredTest1[11]
SSE = sum((PredTest1 -FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
# install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
table(is.na(FluTrain$ILILag2))
plot( log (FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 = lm(log(ILI)~Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
table(is.na(FluTest$ILILag2))
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
summary(PredTest2)
SSE2 = sum((PredTest2 -FluTest$ILI)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2
