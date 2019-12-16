####### Calidad del Aire en Buenos Aires
####### 05/08/2019 La Plata, Argentina
####### Sol Represa
####### Archivo ??


# Objetivo: 
# - Extraer info de las imagenes satelitales interpoladas (AV_7.R) en las estaciones de monitoreo
# (este script utiliza las 3 estaciones de CABA)
# - Extraer datos de .csv monitoreo continuo y juntar con MODIS


#Libreria q se utilizan aqui
library(raster) 
library(sp)
library(rgdal)

library(maptools)
library(maps)
library(mapdata)
#library(gpclib)
#library(spatstat)
#library(RGtk2)
#library(MODIStsp)

library(gdalUtils) 
library(foreign) #read dbf
#library(MODIS)
library(ggplot2)
library(ggmap)

library(lubridate)
library(rgeos)  #buffer de AERONET
library(reshape2)
library(dplyr)

#library(lmtest)
#library(foreign)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 1- Iterativo obtener datos MODIS en sitios monitoreo terrestre (SIN BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# PRESTAR ATENCION A CARPETA DE SATELITE!!!!

## 5.1 Sin utilizar buffer en estaciones

dire = "/home/usuario/Sol/aire_buenos/MODIS/stack/IDW/"
id <- dir(dire, pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..


## Extraer info MODIS para sitios de monitoreo #

sitios <- read.csv("estaciones_caba.csv", sep = ";", dec = ".")
sitios <- data.frame(sitios$lat, sitios$long, sitios$Estacion)
names(sitios) <- c("Latitud", "Longitud", "Codigo")

sit <- data.frame(sitios$Longitud, sitios$Latitud)


MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste(dire, id[i], sep = "")) # abrir geotiff como raster
  cod_sat <- substring(id[i], 13, 20) #tomar dato de la fecha del nombre
  MODIS_ext <- raster::extract(MODIS, sit)
  MODIS_ext <- t(MODIS_ext)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$cod_sat  <- cod_sat #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 


MODIS_point$date <- substring(MODIS_point[,length(MODIS_point)], 2, 8)
MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y%j")

MODIS_point <- data.frame(MODIS_point[,length(MODIS_point)], MODIS_point[,(length(MODIS_point)-1)], MODIS_point[,1:(length(MODIS_point)-2)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", "cod_sat", as.character(sitios_names))



#write.csv(MODIS_point, file="MODIS_1km_est_3.csv", row.names = FALSE)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 2 - Iterativo obtener datos MODIS en sitios monitoreo terrestre (CON BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dire = "/home/usuario/Sol/aire_buenos/MODIS/stack/IDW/"

## 1) Extraer info de MODIS en estaciones utilizando buffer  
sitios <- readOGR("/home/usuario/Sol/aire_buenos/mapa/buff_est_caba.shp") ## Carga buffer de puntos para extraer la info

id <- dir(dire, pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..

# Iterativo para extraer datos con el buffer en estaciones

MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste(dire, id[i], sep = "")) # abrir geotiff como raster
  File <- substring(id[i], 5, 20) # guardar nombre del File
  means <- raster::extract(MODIS, sitios, cellnumbers=TRUE, fun=mean, na.rm=TRUE)
  for (j in 1:length(means)){
    if(is.null( means[[j]])){
      means[[j]] <- NA }    
  }
  MODIS_ext <- t(means)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$File  <- File #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 


MODIS_point$date <- substring(MODIS_point[,length(MODIS_point)], 10, 16)
MODIS_point <- data.frame(MODIS_point[,length(MODIS_point)], MODIS_point[,(length(MODIS_point)-1)], MODIS_point[,1:(length(MODIS_point)-2)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", "cod_sat", as.character(sitios_names))

MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y%j")



#No PISAR
#write.csv(MODIS_point, file="MODIS_1km_est-buff_3.csv", row.names = FALSE)







# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## NUEVO CODIGO  
## 3 - Más rapido: Extraer datos de .csv monitoreo continuo y juntar con MODIS ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(data.table)
library(lubridate)
library(dplyr)

# aod = .csv con mediciones en aod en estaciones
# tiene 1era fila date con la fecha
# tiene 2da fila cod_sat con el codigo de la imagen


# Abrir fichero datos MODIS en sitio monitoreo continuo
#INPUT 1

#aod <- read.csv("MODIS_1km_est_3.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
aod <- read.csv("MODIS_1km_est-buff_3.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

aod$date <- as.Date(aod$date, tz="GMT")
names(aod)[2] <- "File"
names(aod)[5] <- "La_Boca"
aod$File <- paste("MCD19A2.", aod$File, sep="")



# INPUT 2  # # # # #  Datos terrestres

datos_caba <- read.csv("/home/usuario/Sol/AQ-CABA/CABA_datos_limpios.csv")
datos_caba <- datos_caba[, c(1,4, 7, 10)]
names(datos_caba) <- c("date", "Centenario", "Cordoba", "La_Boca")   
 
datos_caba$date <- as.POSIXct(datos_caba$date)
datos_caba$hora <- hour(datos_caba$date)
datos_caba <- datos_caba  %>% filter(hora == 12 | hora == 13 | hora == 14 | hora == 15 ) #selecciono horas de sobrevuelo

#medias de las horas
datos_caba <- aggregate(datos_caba[2:4], FUN = "mean", by = list(format(datos_caba$date, "%Y-%m-%d")), na.rm= TRUE, na.action= NULL)
names(datos_caba)[1] <- "date"
datos_caba$date <- as.Date(datos_caba$date, format = "%Y-%m-%d")

estaciones <- c("Cordoba", "La_Boca", "Centenario")

# # # # # # # # # # # # # # # # # # # #


salida <- data.frame()
for(j in estaciones){
    tabla_aod <- data.frame(date = aod[,1], aod = aod[, j])
    tabla_tierra <- data.frame( date = datos_caba[,1], tierra = datos_caba[,j])
    tabla <- merge(tabla_tierra, tabla_aod)
    tabla$estacion <- j
    salida <- rbind(tabla, salida)
    rm(tabla)
}


# NO PISAR!
# write.csv(salida, "tierra_MODIS_1km_est_3.csv", row.names = FALSE)
# write.csv(salida, "tierra_MODIS_1km_est-buff_3.csv", row.names = FALSE)

rm(list=ls(all=TRUE))




