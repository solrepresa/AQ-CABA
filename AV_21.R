### Calidad del Aire en B
### 16/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 21



library(raster)
library(rgdal)



# Objetivo: 
# Generar raster a partir del modelo de prediccion con Random Forest  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Periodo de estudio
date <- seq(from = as.Date("2009-01-01"), to = as.Date("2017-07-30"), by = "day")
date <- format(date, "%Y%j")

# 2) Abrir modelo
modelo_rf <- readRDS("modelo_rf_1000tree.rds")

#modelo_rf_10tree.rds


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 3) Variables por dia 
# >> TODOS deben empezar en igual fecha y estar completos:20091001

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

dir_merra = "/media/usuario/Elements SE/ARG_MERRA/raster_res/"


AOD_i <- dir("MODIS/stack/IDW/", pattern = ".tif$")


PM10 ~ PS + RH + T + U + V + DMSSMASS + DUSMASS + SO2SMASS + SO4SMASS + 
  SSSMASS + PBLH + PRECTOT + SPEED + ALBEDO + CLDHGH + aod

k =1
variables <-c("PS", "RH", "T","U", "V", "DMSSMASS", "DUSMASS", "SO2SMASS", "SO4SMASS", 
              "SSSMASS", "PBLH", "PRECTOT", "SPEED", "ALBEDO", "CLDHGH")



i = 1
j = 1

while(i < length(date)){

    # busco file AOD para ese dia
    aod <- dir("MODIS/stack/IDW/", 
               pattern = paste( as.character(date[i]),".tif" , sep = ""),
               full.names = TRUE)      
    
    #busco files MERRA para ese dia
    for( k in 1:length(variables)){
      file <- dir(dir_merra, 
                  pattern = paste("\\.", format(as.Date(date[i], "%Y%j"), "%Y%m%d"),".", 
                                  variables[k], ".tif$", sep = ""),
                  full.names = TRUE)
      
      if(length(file) != 0){  # Si no está vacio el archivo continuar
        assign(variables[k], file)
      }
    }
    
    # <<<  Variables unicas se incorporan ACA
    # Por ejemplo: 
    # DEM <- "variables/DEM/DEM.tif"

    # Compruebo si tengo todas con un tryCatch:
    tryCatch({
      # Esto es lo q quiero hacer:
      
      l <- list(PS, RH, T, U, V, DMSSMASS, DUSMASS, SO2SMASS, SO4SMASS, 
                SSSMASS, PBLH, PRECTOT, SPEED, ALBEDO, CLDHGH, aod)
      archivo <- brick(l)
      names(archivo) <- c("PS", "RH", "T", "U", "V", "DMSSMASS", "DUSMASS", "SO2SMASS", "SO4SMASS", 
                          "SSSMASS", "PBLH", "PRECTOT", "SPEED", "ALBEDO", "CLDHGH", "aod")
      
      raster_salida <- predict(archivo, 
                               modelo_rf, 
                               type = "raw", 
                               #progress = "text",
                               predict.all = FALSE,
                               na.rm =TRUE,  ## Agregue estas 2 condiciones nuevas
                               inf.rm = TRUE)
      
      writeRaster(raster_salida, 
                  file = paste("modelo_rf_1000/salida/RF-PM10-", as.character(date[i]), ".tif", sep=""), 
                  format= "GTiff", 
                  overwrite = TRUE )
      
    }, 
    #TryCatch: Si no tengo todos los datos de MERRA o AOD envio mensaje
    error = function(mensaje_error){
      print(paste(date[i], "sin datos MERRA"))},
    
    #TryCatch: Es necesario pasar a la siguiente fecha de AOD
    finally = {
      j <- j + 1   
    })  

 i <- i + 1
 print(i)
}








# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# cluster en raster:
# Ver pagina 34 https://rspatial.org/rs/rs.pdf
