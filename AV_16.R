### Calidad del Aire en CABA
### 05/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 16



### Objetivo: Abrir .hdf de MERRA y guardar en tif las SDS

library(gdalUtils)
library(raster)
library(rgdal)
library(R.utils)
library(maptools)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# CORROBORAR:
# gdal_setInstallation(verbose=TRUE) # ver version gdal
# getOption("gdalUtils_gdalPath")  #verificar que podamos abrir HDF5

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


### ATENCION
#  Este codigo tira error cuando en el .hdf hay solo una SDS
#  en ese caso utilizar el código que está más abajo :)


setwd("/media/usuario/Elements SE/ARG_MERRA/tavg1_2d_aer_Nx") #fundamental para q funcione gdal!
#id <- dir(pattern = ".hdf") 

id <- list.files(path = getwd(),
                 pattern = "*.hdf",
                 full.names = FALSE)

## Shape recorte
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shape <- readOGR("/home/usuario/Sol/aire_buenos/mapa/CABA+LP.shp")


# Para resampling
# Uso imagen MODIS MCD19A2 como modelo para crear raster
MODIS <- raster("/home/usuario/Sol/aire_buenos/MODIS/stack/diarios/MCD19A2.A2009001.tif")


raster_template <- raster(nrows = nrow(MODIS), ncols = ncol(MODIS), 
                          crs = crs_project, 
                          ext = extent(MODIS))  # toma las extensiones


sds_error <- list() # junto file que dan error
k=1 
mapply(file = id,
       FUN = function(file){
         tryCatch(
           { #si no sale error quiero que haga todo lo siguiente:
             sds <- get_subdatasets(file) #lista de nombres de las SDS 
             
             for( j in 1:length(sds)){
               name_sds <- sds[j]
               filename <- paste(substr(name_sds, 19, 54), substr(name_sds, 71, nchar(name_sds)), ".tif", sep="")
               gdal_translate(name_sds, dst_dataset = filename)
               MIRRAraster <- raster(filename)
               
               # 2) Reproyectar
               MIRRAraster <- projectRaster(MIRRAraster,
                                            crs = crs_project,
                                            method = "bilinear")
               
               # 3) Resampling
               rst_resampling <- raster::resample(MIRRAraster, raster_template)
               
               # 4) Recortar
               data_recorte <- crop(rst_resampling, shape)  #recorto imagen para CABA + LP
               
               # 5) Guardar resampling
               writeRaster(data_recorte, paste("/media/usuario/Elements SE/ARG_MERRA/raster_res/", filename, sep = ""), 
                           format = "GTiff",
                           overwrite = TRUE)
               
               rm(data_recorte, MIRRAraster, rst_resampling)
             }
           },
           
           error = function(error_message){
             message("Posible error en get_subdatasets(). Esto ocurre cuando hay solo 1 SDS")
             message(error_message)
             sds_error[k] <- file
             k = k + 1
           }
         )
       }
)








# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## VERSION 2  >> cuando solo hay 1 SDS   ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Cuando en el .hdf solo tiene una unica SDS
## get_subdatasets() da error

setwd("/media/usuario/Elements SE/ARG_MERRA/tavg1_2d_slv_Nx") #fundamental para q funcione gdal!
#id <- dir(pattern = ".hdf") 

id <- list.files(path = getwd(),
                 pattern = "*.hdf",
                 full.names = FALSE)

## Shape recorte

crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shape <- readOGR("/home/usuario/Sol/aire_buenos/mapa/CABA+LP.shp")


# Para resampling
# Uso imagen MODIS MCD19A2 como modelo para crear raster
MODIS <- raster("/home/usuario/Sol/aire_buenos/MODIS/stack/diarios/MCD19A2.A2009001.tif")


raster_template <- raster(nrows = nrow(MODIS), ncols = ncol(MODIS), 
                          crs = crs_project, 
                          ext = extent(MODIS))  # toma las extensiones


SDS = "H1000"



mapply(file = id,
       FUN = function(file){
         tryCatch(
           { #si no sale error quiero que haga todo lo siguiente:
             
             filename <- paste(substr(file, 0, 36), SDS, ".tif", sep="")
             gdal_translate(file, dst_dataset = filename)
             MIRRAraster <- raster(filename)
             
             # 2) Reproyectar
             MIRRAraster <- projectRaster(MIRRAraster,
                                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
                                          method = "bilinear")
             
             # 5) Resampling
             rst_resampling <- raster::resample(MIRRAraster, raster_template)
             
             # 3)Recortar
             data_recorte <- crop(rst_resampling, shape)  #recorto imagen para Valencia
             
             # 6) Guardar resampling
             writeRaster(data_recorte, paste("/media/usuario/Elements SE/MERRA/raster_res/", filename, sep = ""), 
                         format = "GTiff",
                         overwrite = TRUE)
             
             rm(data_recorte, MIRRAraster, rst_resampling)
             
           },
           
           error = function(error_message){
             message("ATENTI")
             message(error_message)
             
           }
         )
       }
)







# # # # # # # # # # # # # # # # # # # # # # # # #

# Variables de interes de MERRA

SDS <- vector( mode = "list", length = 5)
names(SDS) <- c("tavg1_2d_aer_Nx", "tavg1_2d_slv_Nx", "tavg1_2d_flx_Nx", "avg1_2d_rad_Nx", "inst3_3d_asm_Np" )

SDS[[1]] <- c("BCSMASS", "DMSSMASS", "DUSMASS", "DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25")
SDS[[2]] <- "H1000" 
SDS[[3]] <- c("PBLH" , "SPEED", "SPEEDMAX", "USTAR", "PRECTOT")
SDS[[4]] <- c("ALBEDO", "CLDHGH", "CLDLOW")
SDS[[5]] <- c("PS", "T", "RH", "U", "V")

