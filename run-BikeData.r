setwd("C:/Users/shloc/Nextcloud/THD/3. Semester/Assistenzsysteme/Hable/BikeData")
Daten <- read.csv("SeoulBikeData.csv",header=TRUE,sep=";",fill=TRUE)

#Daten[,"Date"] <- as.factor(Daten[,"Date"])
Daten[,"Hour"] <- as.factor(Daten[,"Hour"])
Daten[,"Seasons"] <- as.factor(Daten[,"Seasons"])
Daten[,"Holiday"] <- as.factor(Daten[,"Holiday"])
Daten[,"Functioning.Day"] <- as.factor(Daten[,"Functioning.Day"])

summary(Daten)

model <- lm( Rented.Bike.Count ~ Hour + Temperature..C. + Seasons + Holiday + Functioning.Day, data=Daten)

model

library(shiny)

runApp("App-BikeData")
