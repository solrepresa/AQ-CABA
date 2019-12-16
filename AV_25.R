### Calidad del Aire en Buenos Aires
### 20/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 25

## OBjetivo:
## Relacionar modelos de PM10 con datos de salud

require(raster)
require(sp)
require(rgdal)
require(dplyr)
require(reshape2)

#library(exactextractr)

dire = "/home/usuario/Sol/aire_buenos/mapa"
dir = "modelo_rf_1000/year/"
fs <- list.files(path = dir, 
                 pattern = "mean.tif", full.names = TRUE)


polys <- readOGR("mapa/CABA+LP-files.shp")
names(polys@data)[3] <- "ID"

PM10_modelado <- data.frame()
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
  polys@data$year <- substr(fs[j], 32, 35)
  names(polys@data) <- c("ID", "PM10_mean", "PM10_rango", "year")

  # Guardar
  PM10_modelado <- rbind(polys@data, PM10_modelado )
  
}

PM10_modelado <- PM10_modelado[complete.cases(PM10_modelado),]

### Cargar datos de SALUD  ###

#salud <- read.csv("defuncionesCABA.csv", sep = " ")  ## defunciones de MENORES DE 1 anio
#salud <- read.csv("defuncionesCABA2.csv", sep = " ")  ## defunciones fetales
salud <- read.csv("defuncionesCABA3.csv", sep = " ")  ## defunciones totales


salud <- salud[-1,] #elimino la linea de totales

salud <- melt(salud, "CE")
salud$variable <- substring(salud$variable, 2, 5)
names(salud) <- c("ID", "year", "muertos")

tabla <- merge(salud, PM10_modelado, by = c("ID", "year"))

plot(tabla$muertos, tabla$PM10_mean)  # ns
cor.test(tabla$muertos, tabla$PM10_mean)  # ns

tabla %>% group_by(ID) %>% 
  summarise(cor =   cor.test(muertos, PM10_mean)$estimate,
            p.value =  cor.test(muertos, PM10_mean)$p.value)  #ns
