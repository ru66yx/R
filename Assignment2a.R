climateChange <- read.csv("climate_change.csv", header = TRUE)
trainCliCha <- subset(climateChange, Year <= 2006)
testCliCha <- subset(climateChange, Year > 2006)
head(trainCliCha)
tail(trainCliCha)
mod1 <- lm(Temp ~ MEI +CO2 + CH4 + N2O + CFC.11 + CFC.12 +  TSI + Aerosols, data = trainCliCha)
summary(mod1)
cor(trainCliCha)
mod2 <- lm(Temp ~ MEI + N2O+ TSI + Aerosols, data = trainCliCha)
summary(mod2)
stepMod1 <- step(mod1)
mod3 <- lm(Temp ~ MEI +CO2 + N2O + CFC.11 + CFC.12 +  TSI + Aerosols, data = trainCliCha)
summary(mod3)
predictions <- predict(mod3, newdata = testCliCha)
SSE = sum((predictions - testCliCha$Temp)^2) # 0.2176444
SSE
SST = sum ((mean(trainCliCha$Temp ) - testCliCha$Temp) ^2) 
SST # 0.26751
1 - SSE/SST # .1864065
mod4 <- lm(Temp ~ MEI +CO2 + N2O + CFC.11 + CFC.12 +  TSI + Aerosols, data = testCliCha)
summary(mod4)
