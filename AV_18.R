### Calidad del Aire en Buenos Aires
### 14/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 18


## Objetivo: extraer datos de raster en sitios de monitoreo y unir para formar una única bases

library(foreign)
library(raster)
library(lubridate)
library(dplyr)




#extent(rst_fire)
#res(rst_fire)
#resample(r1, r2, "bilinear")

setwd("/home/usuario/Sol/aire_buenos")

# # # # # # # # # # # # # # # # # # # # # # 

## Coordenadas de puntos de estaciones ####

# # # # # # # # # # # # # # # # # # # # # # 

# estaciones <- # COMPLETAR
# tienen la misma proyeccion?


sitios <- read.csv("estaciones_caba.csv", sep = ";", dec = ".")
sitios <- data.frame(sitios$lat, sitios$long, sitios$Estacion)
names(sitios) <- c("Latitud", "Longitud", "Codigo")

sitios <- sitios[order(sitios$Codigo),]
sitios_names <- t(sitios$Codigo)

sit <- data.frame(sitios$Longitud, sitios$Latitud)


variables_modelo <- list()


# # # # # # # # # # # # # # # # # # # # # # 

## Extraer de cada data set los datos en los puntos####

# # # # # # # # # # # # # # # # # # # # # # 

#  MERRA ####

MERRA_i <- dir("/media/usuario/Elements SE/ARG_MERRA/raster_res", pattern = ".tif")

# Atencion!
# Se colaron qalgunas imagenes q estran repetidas y con el nombre malo
quitar <- grep("D:",MERRA_i)
MERRA_i <- MERRA_i[-quitar]

#se colo "time_bnds"
quitar <- grep("time_bnds",MERRA_i)  #ATENTI: 14983
MERRA_i <- MERRA_i[-quitar]


cod_i <-  substring(MERRA_i, 37, nchar(MERRA_i)-4) #tomar dato de la variable
cod_i <- cod_i[!duplicated(cod_i)]

j =1 

for( j in 1:length(cod_i)){
  tabla <- MERRA_i[which(substring(MERRA_i, 37, nchar(MERRA_i)-4) == cod_i[j])]
  MERRA_ds <- data.frame()
  
  for( i in 1:length(tabla)){   
    cod <-  substring(tabla[i], 37, nchar(tabla[i])-4) #tomar dato de la variable
    date <- substring(tabla[i], 28, 35) #tomar dato de la fecha
    
    MERRA_raster <- raster(paste("/media/usuario/Elements SE/ARG_MERRA/raster_res/", tabla[i], sep =""))
    ds <- raster::extract(MERRA_raster, sit)
    ds <- t(ds )
    ds <- as.data.frame(ds )
    ds$cod  <- cod 
    ds$date  <- date 
    MERRA_ds <- rbind(ds, MERRA_ds)
  }
  names(MERRA_ds) <- c(as.character(sitios_names), "Codigo", "date")
  variables_modelo[[j]] <- MERRA_ds
  print(j)
  print(Sys.time())
}


rm(MERRA_ds, MERRA_raster, ds, MERRA_i)


# SAVE !!!
#saveRDS(variables_modelo, "variables_modelo.rds")

variables_modelo <- readRDS("variables_modelo.rds")
