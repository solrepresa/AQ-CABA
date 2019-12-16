### Calidad del Aire en Buenos Aires
### 15/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 19


library(dplyr)
library(reshape2)
library(lubridate)


# Elaboracion de data frame para armar modelo


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Armar base por estaciones ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Para facilitar el trabajo voy a armar data set por cada estación

estaciones <- read.csv("estaciones_caba.csv", sep = ";", dec = ".")
estaciones <- estaciones$Estacion


variables_modelo <- readRDS("variables_modelo.rds") ## FALTA incorporar FIRE + distancias


fecha <- seq( as.Date("2009-01-01"), as.Date("2018-12-13"), by = "day")

var_estac_model <- list()

for( k in 1: length(estaciones)){   #iteracion por estacion k 
  BASE <- data.frame(fecha)
  
  # MERRA ###
  for( i in 1:length(variables_modelo)){
    V4 <- variables_modelo[[i]]
    V4 <- melt(V4, id = c("date", "Codigo"))
    names(V4) <- c("fecha", "Variable", "Codigo", "Value")
    V4 <- V4 %>% filter(Codigo == estaciones[k])
    
    V4$fecha <- as.Date(V4$fecha, "%Y%m%d")
    names(V4)[4] <- V4[1,2]
    V4 <- V4[,c(1,4)]
    BASE <- merge(BASE, V4, by = "fecha", all = TRUE)
  }
  
  rm(V4)
  

  # CIERRE ###
  BASE <- unique(BASE)
  var_estac_model[[k]] <- BASE 
  
  rm(BASE)
}



# guardo las variables por estacion para armar modelo
#saveRDS(var_estac_model, "variables_estacion_modelo.rds")





# # # # # # # # # # # # # # # # # # # # # # 

## Unir datos del modelo con AOD + mediciones

# # # # # # # # # # # # # # # # # # # # # # 

var_estac_model <- readRDS("variables_estacion_modelo.rds")

tabla <- read.table("tierra_MODIS_1km_est_3.csv", header=TRUE, sep=",", dec=".")

estaciones <- c("Centenario", "Cordoba", "La_Boca")



var_estac_model_aod <- list()

for( k in 1: length(estaciones)){
  BASE <- var_estac_model[[k]]
  AOD_tierra <- tabla
  
  AOD_tierra  <- AOD_tierra[ which(AOD_tierra$estacion == estaciones[k]),]
  
  if(nrow(AOD_tierra ) != 0 ){
    AOD_tierra$date <- as.Date(AOD_tierra$date, "%Y-%m-%d")
    names(AOD_tierra)[1] <- "fecha"
    
    AOD_tierra <- AOD_tierra[,-4]
    BASE <- merge(BASE, AOD_tierra, by = "fecha")
    
    var_estac_model_aod[[k]] <- BASE
    
    rm(BASE)
    
  }else{
    print(paste("Atenti!", estaciones[k], "no hay datos"))
  }
  
}



# Guardo las variables por estaciones con AOD para armar modelo
# saveRDS(var_estac_model_aod, "variables_estacion_aod_modelo.rds")


