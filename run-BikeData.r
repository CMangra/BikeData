setwd("C:/Users/shloc/Nextcloud/THD/3. Semester/Assistenzsysteme/Hable/BikeData")
Daten <- read.csv("SeoulBikeData.csv",header=TRUE,sep=";",fill=TRUE)

Daten[,"Hour"] <- as.factor(Daten[,"Hour"])
Daten[,"Seasons"] <- as.factor(Daten[,"Seasons"])
Daten[,"Holiday"] <- as.factor(Daten[,"Holiday"])
Daten <- subset(Daten,Functioning.Day == "Yes")

summary(Daten)

model <- lm( Rented.Bike.Count ~ Hour + Temperature..C. + Seasons + Holiday, data=Daten)

model

library(shiny)

runApp("App-BikeData")
