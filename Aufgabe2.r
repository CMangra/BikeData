setwd("C:/Users/shloc/Nextcloud/THD/3. Semester/Assistenzsysteme/Hable/BikeData")
Daten <- read.csv("SeoulBikeData.csv",header=TRUE,sep=";",fill=TRUE)

Daten[, "Hour"] <- as.factor(Daten[, "Hour"])
Daten[,"Seasons"] <- as.factor(Daten[,"Seasons"])
Daten[,"Holiday"] <- as.factor(Daten[,"Holiday"])
Daten[,"Functioning.Day"] <- as.factor(Daten[,"Functioning.Day"])

summary(Daten)

Daten <- subset(Daten,Functioning.Day == "Yes")

summary(Daten)

model <- lm( Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Visibility..10m. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Seasons + Holiday, data=Daten)
model

y <- Daten[,"Rented.Bike.Count"] 
Prognosen <- model$fitted.values 
Prognosefehler <- mean( abs( y - Prognosen ) )
Prognosefehler

a <- model$coefficients
a

BikeCount <- a["Hour18"] + a["(Intercept)"] + a["Temperature..C."]*16.8 + a["Humidity..."]*60 + a["Visibility..10m."]*580 + a["Solar.Radiation..MJ.m2."]*0.46 + a["Rainfall.mm."]*0 +a["HolidayNo Holiday"] + a["SeasonsSpring"] + a["Functioning.DayYes"] # Prognose für Zeile 2804

BikeCount

