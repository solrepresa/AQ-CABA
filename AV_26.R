## Comunidad Valenciana
## 04 de Octubre, 2019
##  Sol Represa
## Archivo 26

# Generar modelos de RR y de FA > 0 para PM10
# RR = exp ( B (x - x0))  
# donde B = 0.0008 todas las causas de muerte, todas las edades
# x0 = 10 para PM10

# FA = (RR - 1)/ RR


library(raster)

dir = "modelo_rf_1000/RR_anual/"

id <- dir("modelo_rf_1000/year/", 
          pattern = "_mean.tif",
          full.names = TRUE) # atencion q no haya .tif.xml en la carpeta..

for(i in 1:length(id)){
  rs <- raster(id[i])
  y <- substring(id[i], 32, 35)
  RR <- exp( 0.0008 * (rs - 10))  # RR todas enfermedades
  FA <- (RR - 1)/ RR
  
  writeRaster(RR, filename = paste(dir, "PM10_RR_", y, ".tif", sep=""), overwrite =TRUE)
  writeRaster(FA, filename = paste(dir, "PM10_FA_", y, ".tif", sep=""), overwrite =TRUE)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # #

