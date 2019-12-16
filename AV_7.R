### Calidad del Aire en Buenos Aires
### 18/07/2019 La Plata, Argentina
### Sol Represa
###  Archivo 7



# Rellenado de gaps en raster ##

# "we use an inverse-distance weighted method 
# with a 50km smoothing distance using the IDL function “GridData”."


library(raster)
library(gstat)
library(gdata)
library(maptools)
library(dismo) #kfold
library(caret)
library(parallel)
library(rgdal)

# # # # # # # # # # # # # # # # # # # # # # # # #

# 1- Recorte al area de interes

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "MODIS/stack/diarios/"
dir_salida = "MODIS/stack/diarios/crop/"
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

id <- list.files(path = dir, 
                 pattern = "tif",
                 full.names = FALSE)

poligono <- extent(-59, -34.3, -57.8, -35 ) 

for (i in 1:length(id)){
  MODISraster <- raster(paste(dir, id[i], sep=""))
  archivo_name <- paste(dir_salida, id[i], sep = "")  #seteo nombre de guardado

  if (tryCatch(!is.null(crop(MODISraster, poligono)), error = function(e) return(FALSE))){ # cuando no hay superposicion, no corre
    data_recorte <- crop(MODISraster, poligono)  #recorto imagen 
    writeRaster(data_recorte, 
                format = "GTiff",
                filename = archivo_name, 
                overwrite = TRUE) # Guardo imagen
    
    rm(MODISraster, data_recorte)
    print(i)
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # #

# 2- Interpolacion IDW 

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "MODIS/stack/diarios/"
dir_salida = "MODIS/stack/IDW/"
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

shape <- readOGR("/home/usuario/Sol/aire_buenos/mapa/CABA+LP.shp")
shape2 <- readOGR("/home/usuario/Sol/aire_buenos/mapa/BA-CABA.shp")


fs <- list.files(path = dir, 
                 pattern = "tif",
                 full.names = FALSE)

# Crear raster template >>> spatial Pixels DF
r <- raster(paste(dir, fs[1], sep=""))
r <- crop(r, shape)  
raster_template <- raster(nrows = nrow(r), ncols = ncol(r), #100m de resolucion aprox
                          crs = crs_project, 
                          ext = extent(r))  # toma las extensiones
idw.grid <- rasterToPoints(raster_template, spatial = TRUE)
gridded(idw.grid) <- TRUE   #SpatialPixelsDataFrame
rm(r)


RMSE_IDW <- data.frame()
kfold = 5 # numero de k-fold cross validation

  

for( i in 2001:length(fs)){
  print(i)
  raster_fs <- raster(paste(dir, fs[i], sep=""))
  raster_fs <- crop(raster_fs, shape)  
  filename <- paste("IDW-", fs[i], sep="")
  raster_points <- as.data.frame(rasterToPoints(raster_fs))
  rm(raster_fs)
  
  if(nrow(raster_points)*100/20160 > 30){  #### control: raster con un % mayor al 30% de datos del total ( 20160)
    coordinates(raster_points) <- ~x + y
    #raster_points@proj4string
    projection(raster_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    names(raster_points)[1] <- "AOD"
    
    set.seed(513)  # 5-fold cross-validation
    
    kf <- kfold(nrow(raster_points), k = kfold)
    
    rmse <- rep(NA, kfold)
    for (k in 1:kfold) {   
      test <- raster_points[kf == k, ]
      train <- raster_points[kf != k, ]
      kat.idw <- gstat::idw(AOD ~ 1, train, idw.grid, 
                            debug.level = -1,   #para ver grado de progreso
                            idp = 3) # IDW power 5
      
      final.idw <- raster(kat.idw)
      t <- SpatialPoints(test, proj4string = CRS(crs_project)) #convierto formato para aplicar extract
      #plot(final.idw)
      model <- extract(final.idw, t)
      rmse[k] <- caret::RMSE(test$AOD, model)
    }
    
    #selecciono la interpolacion de menor rmse
    k <- which(rmse == min(rmse, na.rm = TRUE))
    
    test <- raster_points[kf == k, ]
    train <- raster_points[kf != k, ]
    kat.idw <- gstat::idw(AOD ~ 1, train, idw.grid, 
                          debug.level = -1,   #para ver grado de progreso
                          idp = 3) # IDW power 5
    
    final.idw <- raster(kat.idw)
    t <- SpatialPoints(test, proj4string = CRS(crs_project)) #convierto formato para aplicar extract
    #plot(final.idw)
    
    tabla <- data.frame(archivo = filename, 
                        RMSE = min(rmse, na.rm = TRUE), 
                        mean = cellStats(final.idw, mean),
                        sd = cellStats(final.idw, sd))
    RMSE_IDW <- rbind(RMSE_IDW, tabla)
    
    # Recorto el ultimo armado para buenos aires
    data_recorte <- mask(final.idw, shape2)  
    
    # Guardar
    writeRaster(data_recorte, paste(dir_salida, filename, sep = ""), 
                format = "GTiff",
                overwrite = TRUE)
    
    rm(data_recorte, raster_template, train, test, kat.idw)
  }
}

write.csv(RMSE_IDW, "error_IDW_diarios_aod_3.csv",row.names = FALSE)



# # # # # # # # # # # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # # #

a <- read.csv("error_IDW_diarios_aod_1.csv")
b <- read.csv("error_IDW_diarios_aod_2.csv")
c <- read.csv("error_IDW_diarios_aod_3.csv")

a <- rbind(a,b)
a <- rbind(a,c)

write.csv(a, "error_IDW_diarios_aod_tot.csv" , row.names = FALSE)


mean(a$RMSE)  
#[1] 0.007229066

sd(a$RMSE)
#[1] 0.003353565