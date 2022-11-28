setwd("C:/Users/shloc/Nextcloud/THD/3. Semester/Assistenzsysteme/Hable/Projektarbeit")
Daten <- read.csv("SeoulBikeData.csv",header=TRUE,sep=";",fill=TRUE)

#Daten[,"Date"] <- as.factor(Daten[,"Date"])
Daten[,"Hour"] <- as.factor(Daten[,"Hour"])
Daten[,"Seasons"] <- as.factor(Daten[,"Seasons"])
Daten[,"Holiday"] <- as.factor(Daten[,"Holiday"])
Daten[,"Functioning.Day"] <- as.factor(Daten[,"Functioning.Day"])

summary(Daten)

model <- lm( Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Seasons + Holiday + Functioning.Day, data=Daten)

model

library(shiny)

runApp("App-BikeData")
