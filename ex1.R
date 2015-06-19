ls()
who <- read.csv("WHO.csv", header = T)
str(who)
summary(who$Over60)
which.min(who$Over60)
who$Over60[183]
who[183,]
which.max(who$LiteracyRate)
who[44,]
tapply(who$ChildMortality, who$Region,mean)

