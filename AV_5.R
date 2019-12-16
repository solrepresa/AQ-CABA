### Calidad del Aire en Buenos Aires
### 18/07/2019 La Plata, Argentina
### Sol Represa
###  Archivo 3


## Objetivo:
## Recortar, Calibrar y realizar un resampling de las medidas AOD MAIAC calibradas con AERONET
## para que todos los raster tengan iguales dimensiones y poder hacer stack



library(sp)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
#library(gpclib)
#library(spatstat)
#library(RGtk2)
#library(MODIStsp)

library(gdalUtils) 
library(raster)
#library(MODIS)

library(lubridate)
library(rgeos)
library("reshape2")
library(dplyr)

library(lmtest)


dir = "/home/usuario/Sol/aire_buenos/MODIS_1k"


# La proyeccion del proyecto:
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Calibracion    #### 

# # # # # # # # # # # # # # # # # # # # # # # # #

fs <- list.files(path = "/media/usuario/Elements SE/ARG_MODIS/quality/", 
                 pattern = "tif", full.names = TRUE)

for (i in 1:length(fs)){
  archivo_name <- paste("/media/usuario/Elements SE/ARG_MODIS/cal",
                        substring(fs[i], 46, 70), substring(fs[i], 127, 127), ".tif", sep="")   #seteo nombre de guardado
  crop <- raster(fs[i], values = TRUE)
  crop_cal <- (crop * 0.6187) + 0.0345   #calibracion (ver AV_2.R)
  writeRaster(crop_cal, 
              filename = archivo_name, 
              format = "GTiff",
              overwrite = TRUE) # Guardo imagen
  print(i)
}


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Resampling   #### 

# Es necesario hacerlo para igualar la extensión de todas las imagenes y poder hacer el stack

# # # # # # # # # # # # # # # # # # # # # # # # #



fs <- list.files(path = "/media/usuario/Elements SE/ARG_MODIS/cal/", 
                 pattern = ".tif", full.names = TRUE)

extension <- data.frame()
for( i in 1:length(fs)){
  a <- raster(fs[i], values=TRUE)
  ex <- extent(a)
  res <- res(a)
  data <- data.frame(substring(fs[i], 67, 83),ex@xmin, ex@xmax, ex@ymin, ex@ymax, res[1], res[2], ncol(a), nrow(a))
  extension <- rbind(extension, data) 
}

names(extension) <- c("name", "xmin", "xmax", "ymin", "ymax", "res_1", "res_2", "ncol", "nrow")

#extent(a)
#res(a)
#ncol(a)
#nrow(a)

xmin <- min(extension$xmin)
ymin <- min( extension$ymin)
xmax <- max(extension$xmax)
ymax <- max( extension$ymax)
ncol <- max(extension$ncol)
nrow <- max(extension$nrow)

e <- extent(c(xmin = xmin, 
              xmax = xmax, 
              ymin = ymin, 
              ymax = ymax))  # extension donde entren todas las imagenes
s <- raster(e, nrows = nrow, ncols = ncol, crs= crs_project ) #raster con 3 veces resolucion


for (i in 1:length(fs)){
  archivo_name <- paste("/media/usuario/Elements SE/ARG_MODIS/cal_res",
                        substring(fs[i], nchar(fs[i])-29, nchar(fs[i])), sep="")   #seteo nombre de guardado
  crop <- raster(fs[i], values=TRUE)
  if(!all(is.na(crop[]))){
    r1<- raster::resample(crop, s, method = "ngb")
    writeRaster(r1, 
                filename = archivo_name,
                format = "GTiff",
                overwrite = TRUE) # Guardo imagen
    print(i)
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Eliminar valores negativos   #### 

# Es necesario hacerlo en este paso

# # # # # # # # # # # # # # # # # # # # # # # # #

library(raster)
library(maptools)
library(rgdal)

fs <- list.files(path = "/media/usuario/Elements SE/ARG_MODIS/cal_res/", 
                 pattern = ".tif", full.names = TRUE)

shape <- readOGR("/home/usuario/Sol/aire_buenos/mapa/BA-CABA.shp")


for (i in 6001:length(fs)){
  archivo_name <- paste("MODIS/cal_res_crop_z",
                        substring(fs[i], nchar(fs[i])-29, nchar(fs[i])), sep="")   #seteo nombre de guardado
  MODISraster <- raster( fs[i])
  if (tryCatch(!is.null(crop(MODISraster,shape)), error = function(e) return(FALSE))){ # cuando no hay superposicion, no corre
    data_recorte <- crop(MODISraster, shape)  #recorto imagen para Valencia
    values(data_recorte)[values(data_recorte) < 0] <- NA  # reemplazo valores 0 por NA
    writeRaster(data_recorte, 
                format = "GTiff",
                filename = archivo_name, 
                overwrite = TRUE) # Guardo imagen
    print(i)
  }
}