### Calidad del Aire en Buenos Aires
### 26/09/2019 La Plata, Argentina
### Sol Represa
### Archivo 24


# Objetivo: Generar las medias por comunas + municipios >> PM25-poligonos


require(raster)
require(sp)
require(rgdal)
require(dplyr)
#library(exactextractr)

dire = "/home/usuario/Sol/aire_buenos/mapa"
dir = "modelo_rf_1000/year/"
fs <- list.files(path = dir, 
                 pattern = "mean.tif", full.names = TRUE)


polys <- readOGR("mapa/CABA+LP-files.shp")
polys@data[1] <- 1:42
names(polys@data)[1] <- "ID"

for(j in 1:length(fs)){
  r <- raster(fs[j])
  rv <- raster::extract(r, polys, weights=T, method="simple", normalizeWeights=T)
  
  dfx <- data.frame()
  for (i in seq(rv)){
    x <- as.data.frame(rv[[i]])
    x$ID <- polys@data$ID[[i]]
    
    # x$WeightedValue <- x$value * x$weight
    # suma <- sum(x$WeightedValue, na.rm = T)  #me dan valores muuuy raros / sirve para polionos con 100% de cobertura
    # suma <- data.frame(x$ID[1], suma)

    mean <- mean(x$value, na.rm = TRUE)
    rango <- max(x$value, na.rm = TRUE) - min(x$value, na.rm = TRUE)
  
    suma <- data.frame(x$ID[1], mean, rango)
    dfx <- rbind(dfx, suma)
  }
  
  polys@data <- dfx
  names(polys@data) <- c("ID", "PM10_mean", "PM10_rango")
  
  polys@data$nivel <- cut(polys@data$PM10_mean, 
                          breaks = c(0, 29.9, 31, 32, 33, 34, 50), 
                          labels = c("<30", "(30-31]", "[31-32]", "[32-33]", "[33-34]", ">34"))
  
  year <- substr(fs[j], 32, 35)
  
  # Guardar
  writeOGR(polys, dire, 
           paste("PM10-poligonos-", year, sep = ""), 
           driver="ESRI Shapefile", 
           overwrite_layer = TRUE)
  
}



