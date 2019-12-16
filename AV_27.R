### Calidad del Aire en Buenos Aires
### 09/10/2019 La Plata, Argentina
### Sol Represa
### Archivo 27

# Analisis de vientos en la región de La Plata

library(reshape2)
library(lubridate)


smn <- read.csv("/home/usuario/Sol/AQ-CABA/smn/laplata.csv", 
                header = TRUE, sep=";", 
                na.strings = NA, 
                stringsAsFactors = FALSE)
smn$date <- paste(smn$FECHA, smn$HORA.UTC, sep = " ")
smn$date <- as.POSIXct(smn$date, format ="%d/%m/%Y %H", tz = "UTC")


fecha <- data.frame(date = seq.POSIXt(ISOdate(2009,10,01,0), ISOdate(2018,02,21,17), by="hours"))
aero <- merge(fecha, smn, by="date", all=TRUE)
aero <- aero[c(1,2,5:9)]

rm(smn, fecha)

aero$date <- as.POSIXct(aero$date)

names(aero) <- c( "date", "Estacion", "Temperatura", "Humedad", 
                  "wd", "ws", "Nubosidad")  

aero$ws <- aero$ws*1000/3600
aero$wd <- aero$wd * 10

aero[which(aero$wd > 360), "wd"] <- NA

aero$hora <- hour(aero$date)
aero$year <- year(aero$date)

# Ver datos faltantes
ggplot(aero, aes(hora, wd)) + geom_point() + facet_grid(~ year)
# 2009, 2011, 2012, 2013 faltan los datos de la media noche
# 2018 hay mucha falta de datos

aero <- aero %>% filter( year == "2010" |
                         year == "2014" | 
                         year == "2015" | 
                         year == "2016" |
                         year == "2017")


vientos <- windRose(aero, main= " ", paddle = FALSE, key.position = "right",
                    type="season", hemisphere = "southern", angle = 45, grid.line = 10)


vientos <- windRose(aero, main= "  ",
                    paddle = FALSE, key.position = "right",
                    hemisphere = "southern", angle = 45, grid.line = 10)


vientos <- windRose(aero, main= "  ", paddle = FALSE,key.position = "right",
                    type="daylight", hemisphere = "southern", angle = 45, grid.line = 10)


vientos <- windRose(aero, main= "  ", paddle = FALSE,key.position = "right",
                    type="month", hemisphere = "southern", angle = 45, grid.line = 10)


vientos <- windRose(aero, main= "  ", paddle = FALSE,key.position = "right",
                    type="hour", hemisphere = "southern", angle = 45, grid.line = 10)
