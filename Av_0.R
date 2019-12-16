###  Edición de archivo: Calidad del Aire en Buenos Aires
###  21/06/2018 La Plata, Argentina
###  Sol Represa
###  Archivo 0

# Desarrollo de algoritmo para limpiar raster con valores de calidad
# Esto debe hacerse PRIMERO que todo!!


library(rgdal)
library(raster)
library(R.utils)
library(reshape2)
library(ggplot2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# MCD19A2
# Imagenes MAIAC - MODIS

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# en carpeta "ARG_MODIS/raster" las imagenes de MODIS
# en carpeta "ARG_MODIS/QA" las de calidad de MODIS


dire_raster <- "/media/usuario/Elements SE/ARG_MODIS/"
id_aq <- dir(paste(dire_raster, "QA", sep=""), pattern = ".tif")
id <- dir(paste(dire_raster, "raster", sep=""), pattern = ".tif")


# Control de archivos para ver si estan todos

data <- data.frame()
for (i in 1:length(id_aq)){
  datos <- data.frame(raster = substr(id[i], 10, 16), 
                      r_n = substr(id[i], 81, 81), 
                      AQ = substr(id_aq[i], 10, 16),
                      AQ_n = substr(id_aq[i], 70, 70))
  data <- rbind(datos, data)  
}

View(data) # falta el 2009-293 AQ 1


# Graficar: ¿estan todos?  ####

data[,1] <- as.Date( data[,1], format = "%Y%j")
data[,3] <- as.Date( data[,3], format = "%Y%j")
data$r_n <- factor(data$r_n)
data$AQ_n <- factor(data$AQ_n)

fecha <- as.data.frame(seq.Date(as.Date(ISOdate(2009,01,01)), as.Date(ISOdate(2016,12,31)), by = "day", tz = "GMT")) 
names(fecha) <- "fecha"
names(data)[1] <- "fecha"

missing_data <- merge(data[data$r_n == 1,], fecha, by = "fecha", all = TRUE)


## Funcion de datos Missing  ####

# Me genera con una tabla donde
# 0 = NA |  1 = hay dato


miss <- function(tabla, date){
  miss <- data.frame(date) #creo un data.frame con fechas
  for(j in 2:length(tabla)){ #recorro todas las columnas j de datos
    for(i in 1:nrow(tabla)){ #recorro todas las filas i
      if(is.na(tabla[i,j])){
        miss[i,j] <- 0
      }else{
        miss[i,j] <- 1
      }
    }
  }
  names(miss) <- names(tabla)
  return(miss)
}

miss_data <- miss(missing_data, missing_data$fecha)
miss_data <- miss_data[-4]
names(miss_data)[2] <- "raster"
miss_data <- melt(miss_data, id.vars = "fecha" )


# Plot para visualizar los datos que hay ####
ggplot(miss_data, aes(x = fecha, y = value)) + 
  geom_bar(stat="identity") + 
  theme_bw() + ylab("") + 
  scale_y_continuous(breaks =  c(0, 1)) + 
  theme(axis.text.y = element_text(colour = "white" )) +
  facet_wrap(~variable) + 
  ggtitle("Datos descargados")

## Lista de archivos que faltan
falta <- miss_data %>% filter(variable == "raster") %>% filter( value == "0")

falta$value = 1

ggplot(falta, aes( x = fecha, y = value)) + geom_point() +
  theme(axis.text.y = element_text(colour="white")) +
  facet_wrap(~variable) + 
  ylab("") + 
  ggtitle("Datos que faltan")


# LEER ####
# all HDFCEOS products are written in the big-endian referencing scheme, 
# which means the "first" bit is the farthest to the right and the "last" bit is the farthest to the left.
# https://www.nceas.ucsb.edu/~pau/StephanieSite/Home_files/MODIS_LP_QA_Tutorial-1.pdf

# QA for AOD en numeros BINARIOS
# 0000 --- Best quality = 0
# 0001 --- Water Sediments are detected (water)
# 0011 --- There is 1 neighbor cloud
# 0100 --- There is >1 neighbor clouds
# 0101 --- No retrieval (cloudy, or whatever)
# 0110 --- No retrievals near detected or previously detected snow
# 0111 --- Climatology AOD: altitude above 3.5km (water) and 4.2km (land)
# 1000 --- No retrieval due to sun glint (water)
# 1001 --- Retrieved AOD is very low (<0.05) due to glint (water)
# 1010 --- AOD within +-2km from the coastline (may be unreliable)
# 1011 --- Land, research quality: AOD retrieved but CM is possibly cloudy

# Sacado de https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mcd19_user_guide_v6.pdf

# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# NA value -28672



# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Aplicar mascara de calidad (QA= 0000) a imagenes MODIS

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

dire_raster <- "/media/usuario/Elements SE/ARG_MODIS/"
id_aq <- dir(paste(dire_raster, "QA", sep=""), pattern = ".tif")
id <- dir(paste(dire_raster, "raster", sep=""), pattern = ".tif")


for (i in 1:length(id_aq)){
  if(substr(id[i], 1, 61) == substr(id_aq[i], 1, 61) | substr(id[i], 81, 81) == substr(id_aq[2], 70, 70)){
    raster_aq <- raster(paste(dire_raster, "QA/", id_aq[i], sep = ""))
    #range(raster_aq[raster_aq]) #ver rango de valores
    
    # 1) Crear mascara
    # plot(raster_aq)
    
    # intToBien convierte el numero a binario
    # substring se queda con los characteres de interes
    # integer transforma character a numero
    raster_aq[ raster_aq ] <- as.integer(substring(intToBin(raster_aq[ raster_aq]), 4, 7)) #nos quedamos con los bits 8-11
    raster_aq[ raster_aq != 0] <- NA
    # plot(raster_aq)
    
    # 2) Abrir raster de AOD
    MODISraster <- raster(paste(dire_raster, "raster/", id[i], sep = ""))
    MODISraster <- MODISraster*0.001   #factor de escala por imagenes MODIS
    # plot(MODISraster)
    
    # 3) Aplicar máscara
    salida <- mask(MODISraster, raster_aq)
    # plot(salida)
    
    # 4) Guardar
    writeRaster(salida, paste(dire_raster, "quality/", id[i], sep = ""), format = "GTiff")
    rm(salida, raster_aq, MODISraster)
  }else{
    a <- data.frame(i, id[i]) # cuando la indexacion no coincide
    tabla <- rbind(tabla, a)
  }
}


# OJO: exploracion de archivos!
# cuando la indexacion no coincide >> hay problemas con todos los n =3
names(tabla)[2] <- "file"
tabla$file <- as.character(tabla$file)

i=1130
id_aq <- data.frame( file_aq = id_aq, file = substr(id_aq[i], 1, 61), n = substr(id_aq[2], 70, 70))
tabla$files <- substr(id, 1, 61)
tabla$n <- substr(id, 81, 81)

for(j in 1: length(tabla)){
  while(i < length(id_aq)){
    
    id <- tabla[j, 2]
    
    if(substr(id, 1, 61) == substr(id_aq[i], 1, 61) | substr(id, 81, 81) == substr(id_aq[2], 70, 70)){
      print(c(i, id))
    } 
  }
}




######################################################



# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# MOD04_3k
# Aplicar mascara de calidad (QA=3) a imagenes MODIS

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# en carpeta "raster" las imagenes de MODIS
# en carpeta "quality" las de calidad de MODIS

## MYD
dire_raster <- paste("/home/usuario/Escritorio/MODIS","/MYD/raster", sep="")

id_aq <- dir(dire_raster, pattern = "Land_Ocean_Quality_Flag.tif")
id <- dir(dire_raster, pattern = "Optical_Depth_Land_And_Ocean.tif")


for (i in 1:length(id_aq)){
  raster_aq <- raster(paste(dire_raster, "/", id_aq[i], sep = ""))
  
  # Crear mascara
  # Antes plot(raster_aq)
  raster_aq[ raster_aq ==-9999] <- NA   #quito los -9999
  raster_aq[ raster_aq ==0] <- NA       #quito los QA=0  
  raster_aq[ raster_aq ==1] <- NA       #quito los QA=1  
  raster_aq[ raster_aq ==2] <- NA       #quito los QA=2  
  # Despues plot(raster_aq)
  
  MODISraster <- raster(paste(dire_raster, "/", id[i], sep = ""))
  MODISraster <- MODISraster*0.001   #factor de escala por im?genes MODIS
  # plot(MODISraster)
  
  salida <- mask(MODISraster, raster_aq)
  # plot(salida)
  writeRaster(salida, paste(dire_raster, "/quality/", id[i], sep = ""), format = "GTiff")
  rm(salida, raster_aq, MODISraster)
} 


## MOD
dire_raster <- paste("/home/usuario/Escritorio/MODIS","/MOD/raster", sep="")

id_aq <- dir(dire_raster, pattern = "Land_Ocean_Quality_Flag.tif")
id <- dir(dire_raster, pattern = "Optical_Depth_Land_And_Ocean.tif")


for (i in 1:length(id_aq)){
  raster_aq <- raster(paste(dire_raster, "/", id_aq[i], sep = ""))
  
  # Crear mascara
  # Antes plot(raster_aq)
  raster_aq[ raster_aq ==-9999] <- NA   #quito los -9999
  raster_aq[ raster_aq ==0] <- NA       #quito los QA=0  
  raster_aq[ raster_aq ==1] <- NA       #quito los QA=1  
  raster_aq[ raster_aq ==2] <- NA       #quito los QA=2  
  # Despues plot(raster_aq)
  
  MODISraster <- raster(paste(dire_raster, "/", id[i], sep = ""))
  MODISraster <- MODISraster*0.001   #factor de escala por im?genes MODIS
  # plot(MODISraster)
  
  salida <- mask(MODISraster, raster_aq)
  # plot(salida)
  writeRaster(salida, paste(dire_raster, "/quality/", id[i], sep = ""), format = "GTiff")
  rm(salida, raster_aq, MODISraster)
} 
