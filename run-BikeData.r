setwd("C:/Users/shloc/Nextcloud/THD/3. Semester/Assistenzsysteme/Hable/BikeData")
Daten <- read.csv("SeoulBikeData.csv",header=TRUE,sep=";",fill=TRUE)

Daten[,"Hour"] <- as.factor(Daten[,"Hour"])
Daten[,"Seasons"] <- as.factor(Daten[,"Seasons"])
Daten[,"Holiday"] <- as.factor(Daten[,"Holiday"])

summary(Daten)

for (day in seq(1,8760, 24)){
  highest_temperature <- -1000
  highest_snowfall <- -1000
  highest_rainfall <- -1000
  highest_visibility <- -1000
  highest_humidity <- -1000
  highest_solar_radiation <- -1000
  highest_wind_speed <- -1000

  for (hour in 0:23){

    if (Daten[day+hour, "Temperature..C."] > highest_temperature){
      highest_temperature <- Daten[day+hour, "Temperature..C."]
    }
    
    
    if (Daten[day+hour, "Snowfall..cm."] > highest_snowfall){
      highest_snowfall <- Daten[day+hour, "Snowfall..cm."]
    }
    
    
    if (Daten[day+hour, "Rainfall.mm."] > highest_rainfall){
      highest_rainfall <- Daten[day+hour, "Rainfall.mm."]
    }
    
    
    if (Daten[day+hour, "Visibility..10m."] > highest_visibility){
      highest_visibility <- Daten[day+hour, "Visibility..10m."]
    }
    
    
    if (Daten[day+hour, "Humidity..."] > highest_humidity){
      highest_humidity <- Daten[day+hour, "Humidity..."]
    }
    
    
    if (Daten[day+hour, "Solar.Radiation..MJ.m2."] > highest_solar_radiation){
      highest_solar_radiation <- Daten[day+hour, "Solar.Radiation..MJ.m2."]
    }

    if (Daten[day+hour, "Wind.speed..m.s."] > highest_wind_speed){
      highest_wind_speed <- Daten[day+hour, "Wind.speed..m.s."]
    }
  }   
  
  for (hour in 0:23){
    Daten[day+hour, "Temperature..C."] <- highest_temperature 
    Daten[day+hour, "Snowfall..cm."] <- highest_snowfall 
    Daten[day+hour, "Rainfall.mm."] <- highest_rainfall 
    Daten[day+hour, "Visibility..10m."] <- highest_visibility 
    Daten[day+hour, "Humidity..."] <- highest_humidity 
    Daten[day+hour, "Solar.Radiation..MJ.m2."] <- highest_solar_radiation 
    Daten[day+hour, "Wind.speed..m.s."] <- highest_wind_speed 
    
  }
  
}

Daten <- subset(Daten,Functioning.Day == "Yes")


model <- lm( Rented.Bike.Count ~ Hour + Temperature..C. + Seasons + Holiday + Snowfall..cm. + Rainfall.mm. + Solar.Radiation..MJ.m2. + Visibility..10m. + Humidity... + Wind.speed..m.s., data=Daten)

print(model)


y <- Daten[,"Rented.Bike.Count"] 
Prognosen <- model$fitted.values 
Prognosefehler <- mean( abs( y - Prognosen ) )
print(Prognosefehler)

library(shiny)
library(shinyWidgets)

runApp("App-BikeData")
