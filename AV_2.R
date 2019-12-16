####### Calidad del Aire en Buenos Aires
####### 16/07/2019 La Plata, Argentina
####### Sol Represa
####### Archivo 2

## Trabajar con MODIS 1km - MOSAICOS


library(caret)
library(rgeos)

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

### Sitio AERONET  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Estación CEILAP
# Longitud: -58.50641, Latitud = -34.55542

### 2.1  Analisis datos AERONET ####
data <- read.csv("CEILAP-BA_tot.csv", header=TRUE, sep=",", dec=".", skip= 6, na.strings = "N/A", stringsAsFactors = FALSE  )
date <- paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "  )
date <- strptime(date, format="%d:%m:%Y %H:%M:%S", tz="GMT") 

### 2.2 - Calculo AOD 550 con interpolacion cuadratica ####


# # #  FUNCION INTERPOLACION CUADRATICA  # # # # # # # 

# Esta funcion realiza una interpolacion tomando 3 puntos,
# basandose en los polinomios de Lagrange.

interpol_cuad <- function(x, x0, y0, x1, y1, x2, y2){ 
  a = ((x - x1)*(x-x2))/((x0-x1)*(x0-x2))
  b = ((x - x0)*(x-x2))/((x1-x0)*(x1-x2))
  c = ((x - x0)*(x-x1))/((x2-x0)*(x2-x1))
  y = (y0*a) + (y1*b) + (y2*c)
  return(y)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Utilizando 675nm

x = log(550)  #incognita

x0= log(440)         #punto 1
y0= log(data$AOD_440nm)
x1= log(500)         #punto 2
y1=log(data$AOD_500nm)
x2= log(675)         #punto 3
y2= log(data$AOD_675nm)

y <- interpol_cuad(x, x0, y0, x1, y1, x2, y2)

y <- exp(y)

data_aeronet <- data.frame(date, y)
names(data_aeronet) <- c("date", "AOT_550")


# Utilizando 870 nm


x2= log(870)
y2= log(data$AOD_870nm)

data_aeronet$AOT_550_2  <- interpol_cuad(x, x0, y0, x1, y1, x2, y2)
data_aeronet$AOT_550_2  <- exp(data_aeronet$AOT_550_2)

# Utilizando 1020nm

x2= log(1020)
y2= log(data$AOD_1020nm)

data_aeronet$AOT_550_3 <- interpol_cuad(x, x0, y0, x1, y1, x2, y2)
data_aeronet$AOT_550_3 <- exp(data_aeronet$AOT_550_3)

# MEDIAN

data_aeronet$AOT_550_mod <- rowMeans(data_aeronet[,2:4], na.rm = TRUE)


#Analizar correlacion entre los AOD 550 calculados con los distintos puntos 
ggplot(data= data_aeronet, aes(x= AOT_550, y= AOT_550_2)) + geom_point(na.rm=TRUE, colour="gray48") + theme_bw() +
  labs(x= "AOT 550 (675 Angstrom) ", y="AOT 550 (870 Angstrom)", title= "Transformaci?n valores AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) 

ggplot(data= data_aeronet, aes(x= AOT_550_3, y= AOT_550_2)) + geom_point(na.rm=TRUE, colour="gray48") + theme_bw() +
  labs(x= "AOT 550 (870 Angstrom) ", y="AOT 550 (1020 Angstrom)", title= "Transformaci?n valores AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) 



# NO PISAR CSV!
#write.csv(data_aeronet, file="CEILAP-BA.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # 

### 3 - CALIBRAR imagenes MODIS con AERONET ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

### 3. 1. -  Extraer de MODIS valores en BUFFER de Aeronet ####

aeronet <- data.frame(-58.50641, -34.55542)
names(aeronet) <- c("Longitud", "Latitud")
coordinates(aeronet) <- ~Longitud+Latitud
proj4string(aeronet) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#aeronet_sp <- spTransform(aeronet, CRS()) # Transformar proyeccion 

### FUNCION para hacer BUFFER de puntos ####

custom.buffer <- function(p, r) {        # p  son coordenadas, r es la distancia en metros
  stopifnot(length(p) == 1)
  cust <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",    #Azimuthal equidistant projection (AEQ) An AEQ projection centred on each point will project equal distances in all directions.
                  p@coords[[2]], p@coords[[1]])
  projected <- spTransform(p, CRS(cust))                           #transforma el punto en proyeccion AEQ
  buffered <- gBuffer(projected, width=r, byid=TRUE)               #genera el buffer
  spTransform(buffered, p@proj4string)                             #transforma el buffer a la proyeccion original del punto
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# buffer de 1.5km de radio = 3km de diametro  >> imagen pixel de 1km  
# buffered <- custom.buffer(aeronet, 1500)     


# buffer de 3.25km de radio = 7.5 de diametro  >> imagen pixel de 3km  
# Da mejores resultados
buffered <- custom.buffer(aeronet, 3250)      

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Iterativo para extraer datos con el buffer MCD19A2 (MODIS-MAIAC)  #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

dire_raster <- "/media/usuario/Elements SE/ARG_MODIS/"
dire_quality <- paste(dire_raster, "quality", sep="")

id <- dir(dire_quality, pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..

aod_point_raster <- data.frame()
for (i in 1:length(id)){ 
  aod <- raster(paste(dire_quality, "/", id[i], sep = ""))  # abrir geotiff como raster
  date <- substring(id[i], 10, 16) #tomar dato de la fecha del nombre
  means <- raster::extract(aod, buffered, cellnumbers=TRUE, fun=mean, na.rm=TRUE)  # ver: weights = TRUE
  if(is.null( means[[1]])){
    means <- NA } 
  file <- substring(id[i], 1, 16)
  aero_dato <- data.frame( date , means, file )   #armar data frame con: fecha + datos en aeronet
  names(aero_dato) <- c("date", "aod_550", "file")
  aod_point_raster <- rbind(aero_dato, aod_point_raster)
  rm(aero_dato)
} 

aod_point_raster$date <- as.character(aod_point_raster$date)
fechas <- strptime(aod_point_raster$date, tz= "GMT", format = "%Y%j")
fechas <- as.data.frame(fechas)
aod_point_raster[1] <- fechas


#write.csv(aod_point_raster, file="MODIS_sitio_raster_aeronet_1km.csv", row.names = FALSE)
write.csv(aod_point_raster, file="MODIS_sitio_raster_aeronet_7km.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### 6 - Extraer datos de .csv AERONET "CEILAP-BA.csv" ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# 6.1  Abrir fichero datos MODIS en sitio aeronet ####

aod_point <- read.csv("MODIS_sitio_raster_aeronet_7km.csv", header=TRUE, sep=",", 
                      dec=".", stringsAsFactors = FALSE)

#aod_point <- read.csv("MODIS_sitio_raster_aeronet_1km.csv", header=TRUE, sep=",", 
#                      dec=".", stringsAsFactors = FALSE)
aod_point$date <- as.character(aod_point$date)
aod_point$date <- as.POSIXlt(aod_point$date, tz="GMT") 

# Abrir fichero AERONET y acomodar el formato
data_aeronet <- read.csv("CEILAP-BA.csv", header=TRUE, sep=",", dec=".", na.strings = "NA", 
                         stringsAsFactors = FALSE  )
data_aeronet$date <- as.POSIXlt(data_aeronet$date, tz="GMT") 

# 6.2 Tomar valores de AERONET dentro de 30min (1800 sec) que pasa MODIS ####
# Como el producto es un promedio de las imágenes Aqua y Terra 
# el criterio debe ser el promedio de los valores registrados por AOD en ese rango
# en Buenos Aires: Aura pasa entre las 13-14hs

AOD <- data.frame()

#i=7915
for (i in 1: nrow(aod_point)){                    ### Me devuelve la media de datos que comparten año, mes y dia
  tabla_aeronet <- data_aeronet
  eq_year <- which(year(tabla_aeronet$date) == year(aod_point[i,]$date))
  tabla_aeronet <- tabla_aeronet[eq_year,] 
  eq_month <- which(month(tabla_aeronet$date) == month(aod_point[i,]$date))
  tabla_aeronet <- tabla_aeronet[eq_month,] 
  eq_day <- which(day(tabla_aeronet$date) == day(aod_point[i,]$date))
  tabla_aeronet <- tabla_aeronet[eq_day,]
  dim_tabla <- dim(tabla_aeronet)
  if(dim_tabla[1] == 0){
    salida <- data.frame(Date_MODIS = aod_point[i,1],
                         AOD_MODIS = aod_point[i,2], 
                         mean_AERO = NA, 
                         median_AERO = NA, 
                         sd_AERO  = NA, 
                         n_AERO = NA )                ## si no tengo datos ese dia, coloca un NA :) para evitarnos el warning
  }else{
    ### VER criterio -->  
    mach <- which(hour(tabla_aeronet$date) == 13 | hour(tabla_aeronet$date) == 14 |
                    hour(tabla_aeronet$date) == 15 | hour(tabla_aeronet$date) == 12) # <---- busco dentro de las 12 y 15hs
    tabla_dif <- tabla_aeronet[mach,]
    dim_tabla <- dim(tabla_dif)
    # # # # # # # # # # # # # # # # # # # # # # # # #     
    if(dim_tabla[1] == 0){                                          ## si no tengo datos para esas horas, coloca un NA
      salida <- data.frame(Date_MODIS = aod_point[i,1],
                           AOD_MODIS = aod_point[i,2], 
                           mean_AERO = NA, 
                           median_AERO = NA, 
                           sd_AERO  = NA, 
                           n_AERO = NA ) 
    }else{
      salida <- data.frame(Date_MODIS = aod_point[i,1],
                           AOD_MODIS = aod_point[i,2],
                           mean_AERO = mean(tabla_dif[,5]), 
                           median_AERO = median(tabla_dif[,5]),
                           sd_AERO = sd(tabla_dif[,2], na.rm=TRUE), 
                           n_AERO = dim_tabla[1])
    }
  }
  AOD <- rbind(AOD, salida)

}


# NO PISAR BASE! 
#write.csv(AOD, "AOD_MODIS_1km_AERONET_mean_12-15.csv", row.names = FALSE)
#write.csv(AOD, "AOD_MODIS_1km_AERONET_mean_12-15_buff_7km.csv", row.names = FALSE)



AOD <- read.csv("AOD_MODIS_1km_AERONET_mean_12-15.csv")

AOD <- AOD[complete.cases(AOD),] #9250 > 2103
cor.test(AOD$AOD_MODIS, AOD$mean_AERO) #la correlacion es MUY fulera

AOD <- AOD[which(AOD$AOD_MODIS > 0), ] #solo 782
cor.test(AOD$AOD_MODIS, AOD$mean_AERO)  # 0.5412817  


# Grafica de tiempo
ggplot(data = AOD) + 
  geom_point(aes(x = Date_MODIS, y = AOD_MODIS), 
                               colour="firebrick2", na.rm=TRUE) + 
  geom_point(aes(x = Date_MODIS,  y = mean_AERO), 
            colour ="dodgerblue2", na.rm=TRUE ) + 
  theme_bw() +  
  labs(x= "Fecha", y="550 nm AOD", title = "Serie de tiempo AERONET ")

#Grafica de correlacion
ggplot(data= AOD, aes(x= AOD_MODIS, y= mean_AERO)) + 
  geom_point(na.rm=TRUE) + theme_bw() +
  labs(x= "MODIS 550 nm AOD", y="AERONET 550 nm AOD", title= "Correlacion MODIS - AERONET ")  + 
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) + 
  geom_smooth(method = "lm", formula= y ~ x, se=TRUE, na.rm=TRUE, fullrange=TRUE) + 
  coord_cartesian( xlim= c(0,1), ylim= c(0,1)) + 
  scale_colour_distiller(palette = "Spectral", limits=c(0,0.07))



#Generar set de entrenamiento
set.seed(123) #seteamos para obtener resultados reproducibles
i_entrena <- createDataPartition(y = AOD$AOD_MODIS, 
                                 p = 0.8, 
                                 list = FALSE)
entrena <- AOD[i_entrena,]
test <- AOD[-i_entrena,]

# modelo para set de entrenamiento
reg <- lm(entrena$AOD_MODIS ~ entrena$mean_AERO )  
summary(reg) #R2 =  0.3406 
plot(reg) # quitar valores baja el R^2


entrena$modis_coregido <- reg$fitted.values
entrena$sd_AERO <- abs(entrena$modis_coregido - entrena$AOD_MODIS)

# Funcion RMSE + MAE ###
RMSE = function(m, o){   # m = model, o = observed
  sqrt(mean((m - o)^2))
}

MAE <- function(m, o) { mean(abs(m - o)) }

RMSE(entrena$modis_coregido, entrena$AOD_MODIS) # 0.045 con el set de entrenamiento
MAE(entrena$modis_coregido, entrena$AOD_MODIS) # 0.03 con el set de entrenamiento

coef(reg)
test$modis_coregido <- test$mean_AERO * 0.6187 + 0.0345

RMSE(test$modis_coregido, test$AOD_MODIS) # 0.043 con el set de entrenamiento
MAE(test$modis_coregido, test$AOD_MODIS) # 0.03 con el set de entrenamiento

AOD$modis_coregido <- AOD$mean_AERO * 0.6187 + 0.0345
AOD$sd_AERO <- abs(AOD$modis_coregido - AOD$AOD_MODIS)


#grafica de correlacion
ggplot(data= AOD, aes(x= AOD_MODIS, y= mean_AERO)) + 
  geom_point(na.rm=TRUE, col =  "palegreen3") + 
  theme_bw() +
  labs(x= "MODIS 550 nm AOD", y="AERONET 550 nm AOD", title= "r = 0.54")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_smooth(method = "lm", formula= y ~ x, se=TRUE, na.rm=TRUE, fullrange=TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian( xlim= c(0,0.5), ylim= c(0,0.5))


#grafica del modelo
ggplot(data= AOD, aes(x= AOD_MODIS, y= modis_coregido, colour=sd_AERO)) + 
  geom_point(na.rm=TRUE) + theme_bw() +
  labs(x= "AOD por MODIS", y="AOD calibrado con AERONET", title= "Calibracion MODIS - AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian( xlim= c(0,0.6), ylim= c(0,0.6))+ 
  scale_colour_distiller(palette = "Spectral", limits=c(0,0.25))
