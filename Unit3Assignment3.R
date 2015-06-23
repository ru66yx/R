loans <- read.csv("loans.csv", header = TRUE) 
table(loans$not.fully.paid)
str(loans)
1533/9578
table (is.na(loans[,5]))
table (is.na(loans[,8]))
table (is.na(loans[,10]))
table (is.na(loans[,11]))
table (is.na(loans[,12]))
table (is.na(loans[,13]))
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
mod1 <- glm(not.fully.paid ~., data = train, family = binomial)
summary(mod1)
#####
-9.406e-03 *10
exp(0.09406)
str (train)
#####
ndata = data.frame (credit.policy = c(1,1), purpose =c("credit_card", "credit_card"), int.rate =c(.11,.11), installment =c(12,12),log.annual.inc = c(11,11),dti =c(15,15), fico = c(700,710), days.with.cr.line =c(2000,2000 ), revol.bal = c(3000,3000), revol.util =c(16,16), inq.last.6mths =c(0,0), delinq.2yrs= c(0,0), pub.rec= c(0,0))
ndataPredict <-predict(mod1, ndata, type = "response" )
loga<- log(0.05416540/(1-0.05416540))
log(0.05726731)
logb <- log(0.04954396/ (1 -0.04954396))
log(0.05212651)
loga -logb # 0.09405617
exp(0.09405617)
####
predicted.risk <- predict(mod1, newdata = test, type = "response")
test$predicted.risk <- predicted.risk
table(test$not.fully.paid, test$predicted.risk >= 0.5)
#   FALSE TRUE
#  0 2400   13
#  1   457    3

acc <- (2400 + 3) /2873
library(ROCR)
ROCRpredTest = prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
mod2 <- glm(not.fully.paid~int.rate, data=train, family=binomial)
summary(mod2)
predicted.risk2 <- predict(mod2, newdata = test, type = "response")
table(test$not.fully.paid, predicted.risk2 >= 0.5)
max(predicted.risk2)
ROCRpredTest2 = prediction(predicted.risk2, test$not.fully.paid)
auc2 = as.numeric(performance(ROCRpredTest2, "auc")@y.values)
10*exp(.06 *3)
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10
highInterest <- subset(test, test$int.rate>=0.15)
summary(highInterest)
mean(highInterest$not.fully.paid)
table(highInterest$not.fully.paid)
110/437
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest,highInterest$predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
