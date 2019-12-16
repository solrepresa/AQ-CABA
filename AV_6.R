### Calidad del Aire en Buenos Aires
### 18/07/2019 La Plata, Argentina
### Sol Represa
###  Archivo 6

# Objetivo: hacer stack


library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 0 - Stack diarios   ####

# # # # # # # # # # # # # # # # # # # # # # # # #


aod <- dir("/home/usuario/Sol/aire_buenos/MODIS/cal_res_crop_z/", pattern = ".tif$")

i=1
while(i <= length(aod)){
  current = i
  lista_dias <- data.frame()
  
  while( substring(aod[i], 10, 16) == substring(aod[current], 10, 16)){
    lista_dias <- c(lista_dias, paste("/home/usuario/Sol/aire_buenos/MODIS/cal_res_crop_z/", aod[i], sep=""))
    i = i + 1
  }
  
  if( length(lista_dias) > 1){
    archivo <- brick(lista_dias)
    mean_s <- calc(archivo, fun = mean, na.rm = TRUE)
    writeRaster(mean_s, 
                filename = paste("MODIS/stack/diarios/", substring(aod[current], 1, 16), ".tif", sep=""))
  }else{
    archivo <- raster(paste("/home/usuario/Sol/aire_buenos/MODIS/cal_res_crop_z/", aod[current], sep = ""))
    writeRaster(archivo, 
                filename = paste("MODIS/stack/diarios/", substring(aod[current], 1, 19), ".tif", sep=""))
    
  }
  print(substring(aod[i], 10, 16))
}



# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Stack por year-mes   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

library(lubridate)
library(raster)

aod <- dir("/home/usuario/Sol/aire_buenos/MODIS/cal_res_crop_z/", pattern = ".tif$")


i=1
while(i <= length(aod)){
  current = i
  l <- data.frame()
  mes <- as.character(month(as.Date(substring(aod[current], 10, 16), "%Y%j")))
  if(nchar(mes) == 1){ mes <- paste("0", mes, sep ="")}
  name <- paste("MODIS/stack/year-month/AOD_", substring(aod[current], 10, 13), "_",mes, sep="")
    
  #year-month
  while( year(as.Date(substring(aod[i], 10, 16), "%Y%j")) == year(as.Date(substring(aod[current], 10, 16), "%Y%j")) &&  
         month(as.Date(substring(aod[i], 10, 16), "%Y%j")) == month(as.Date(substring(aod[current], 10, 16), "%Y%j")) ){ 
    l <- c(l, paste("/home/usuario/Sol/aire_buenos/MODIS/cal_res_crop_z/", aod[i], sep=""))
    i = i + 1
  }
  
  s <- brick(l)
  
  #min_s <- min(s, na.rm=TRUE)  
  #max_s <- max(s, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  mean_s <- calc(s, fun=mean, na.rm=TRUE)
  sd_s <- calc(s, fun=sd, na.rm=TRUE)
  cv_s <- sd_s / mean_s
  
  fun <- function(x) { sum(!is.na(x)) }
  n_s <- calc(s, fun = fun )
  
  
  writeRaster(cv_s, filename = paste(name, "_cv.tif", sep=""))
  writeRaster(mean_s, filename = paste(name, "_mean.tif", sep=""))
  writeRaster(n_s, filename = paste(name, "_n.tif", sep=""))
  
  print(c(substring(aod[i-1], 10, 13), mes) )
}



# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Slack por year   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

library(lubridate)
library(raster)

fs <- list.files(path = "/home/usuario/Sol/aire_buenos/MODIS/cal_res_crop_z", 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for (j in 2009:2017){
  for (i in 1:length(fs)){
    year <- as.numeric(substring(fs[i], 61, 64))
    if (year == j){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  s <- raster::brick(l)
  mean_s <- calc(s, fun=mean, na.rm=TRUE)
  sd_s <- calc(s, fun=sd, na.rm=TRUE)
  cv_s <- sd_s / mean_s
  
  fun <- function(x) { sum(!is.na(x)) }
  n_s <- calc(s, fun = fun )
  
  
  writeRaster(mean_s, filename = paste("MODIS/stack/year/AOD_year_", j, "_mean.tif", sep=""))
  writeRaster(cv_s, filename = paste("MODIS/stack/year/AOD_year_", j, "_cv.tif", sep=""))
  writeRaster(n_s, filename = paste("MODIS/stack/year/AOD_year_", j, "_n.tif", sep=""))
  
  print(j)
}


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 4 - Slack por mes  ??? ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "/home/usuario/Sol/aire_comunitat/MODIS/cal_res/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


month_name <- c("01", "02", "03", "04", "05", "06", "07",
                "08", "09", "10", "11", "12")
