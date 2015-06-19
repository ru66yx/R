elantra <- read.csv("elantra.csv", header = TRUE)
summary(elantra)
elantrain <- subset(elantra, Year <= 2012)
elantraTest <- subset(elantra, Year > 2012)
elantraLm <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantrain )
summary(elantraLm)
elantraLm2 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + as.factor( Month), data = elantrain )
summary(elantraLm2)
cor(elantrain)
elantraLm3 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + as.factor( Month), data = elantrain )
summary(elantraLm3)
predictions <- predict(elantraLm3, newdata = elantraTest)
SSE = sum((predictions - elantraTest$ElantraSales)^2)
RMSE = sqrt(SSE/nrow(elantraTest))
RMSE
mean(elantrain$ElantraSales)
SST = sum((mean(elantrain$ElantraSales) - elantraTest$ElantraSales)^2)
1 - SSE/SST
max(abs(predictions - elantraTest$ElantraSales))
which.max (abs(predictions - elantraTest$ElantraSales))
which.max(predictions - elantraTest$ElantraSales)
which.min(predictions - elantraTest$ElantraSales)
elantraTest[5,]
predictions[5] - elantraTest$ElantraSales[5]
