### Calidad del Aire en Buenos Aires
### 20/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 22


# Objetivo: Armar slack de los mapas de PM10


library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Slack por year   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "modelo_rf_1000/salida/"
dir_slack = "modelo_rf_1000/year/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for (j in 2015:2017){
  for (i in 1:length(fs)){
    year <- as.numeric(substring(fs[i], 32, 35))
    if (year == j){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  s <- raster::brick(l)
  
  #min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  #mean_s <- calc(s, fun = mean, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  #sd_s <- calc(s, fun=sd, na.rm=TRUE)
  #cv_s <- sd_s/mean_s
  
  #fun <- function(x) { sum(!is.na(x)) }
  #n_s <- calc(s, fun = fun )
  
  writeRaster(min_s, filename = paste(dir_slack, "PM10_year_", j, "_min.tif", sep=""),
              overwrite = TRUE, format= "GTiff")
  writeRaster(max_s, filename = paste(dir_slack, "PM10_year_", j, "_max.tif", sep=""),
              overwrite = TRUE, format= "GTiff")
  #writeRaster(mean_s, 
     #         filename = paste(dir_slack, "PM10_year_", j, "_mean.tif", sep = ""),  
    #          overwrite = TRUE,
    #         format= "GTiff")
  #writeRaster(median_s, filename = paste(dir_slack, "PM25_year_", j, "_median.tif", sep=""))
  #writeRaster(sd_s, filename = paste(dir_slack, "PM10_year_", j, "_sd.tif", sep=""),
                                  #   overwrite = TRUE, format= "GTiff")
  #writeRaster(n_s, filename = paste(dir_slack, "PM25_year_", j, "_n.tif", sep=""))
  #writeRaster(cv_s, filename = paste(dir_slack, "PM10_year_", j, "_cv.tif", sep=""),
                                   #  overwrite = TRUE, format= "GTiff")
  
  print(j)
  rm(s, min_s, max_s, sd_s, cv_s)
}




# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Slack por year - mes   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "modelo_rf_1000/salida/"
dir_slack = "modelo_rf_1000/year-mes/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for (j in 2009:2017){  #yars
  for(m in 1:12){  #meses
  
    for (i in 1:length(fs)){
      year <- as.numeric(substring(fs[i], 32, 35))
      mes <- month(as.Date( substring(fs[i], 32, 39), format = "%Y%j"))
      if (year == j && mes == m){
        l[[k]] <- fs[i]
        k <- k + 1
      }
    }
    
    # control >> solo si l guarda algun elemento 
    if(length(l) > 0){
      s <- raster::brick(l)
      
      min_s <- min(s, na.rm=TRUE)  
      max_s <- max(s, na.rm=TRUE)
      mean_s <- calc(s, fun = mean, na.rm=TRUE)
      #median_s <- calc(s, fun = median, na.rm = TRUE)
      sd_s <- calc(s, fun=sd, na.rm=TRUE)
      cv_s <- sd_s/mean_s
      
      fun <- function(x) { sum(!is.na(x)) }
      n_s <- calc(s, fun = fun )
      
      writeRaster(min_s, filename = paste(dir_slack, "PM10_year-mes_", j, m, "_min.tif", sep=""))
      writeRaster(max_s, filename = paste(dir_slack, "PM10_year-mes_", j, m, "_max.tif", sep=""))
      writeRaster(mean_s, 
                  filename = paste(dir_slack, "PM10_year-mes_", j, m, "_mean.tif", sep = ""),  
                  overwrite = TRUE,
                  format= "GTiff")
      #writeRaster(median_s, filename = paste(dir_slack, "PM25_year_", j, "_median.tif", sep=""))
      writeRaster(sd_s, filename = paste(dir_slack, "PM10_year-mes_", j, m, "_sd.tif", sep=""))
      writeRaster(n_s, filename = paste(dir_slack, "PM10_year-mes_", j, m, "_n.tif", sep=""))
      writeRaster(cv_s, filename = paste(dir_slack, "PM10_year-mes_", j, m, "_cv.tif", sep=""))
      
      print(j)
      rm(s, min_s, max_s, sd_s, cv_s)
    }
   
  }
}



# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Slack por mes   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "modelo_rf_1000/salida/"
dir_slack = "modelo_rf_1000/mes/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for(m in 1:12){  #meses
  
  for (i in 1:length(fs)){
    mes <- month(as.Date( substring(fs[i], 32, 39), format = "%Y%j"))
    if (mes == m){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  
  # control >> solo si l guarda algun elemento 
  if(length(l) > 0){
    s <- raster::brick(l)
    
    min_s <- min(s, na.rm=TRUE)  
    max_s <- max(s, na.rm=TRUE)
    mean_s <- calc(s, fun = mean, na.rm=TRUE)
    #median_s <- calc(s, fun = median, na.rm = TRUE)
    sd_s <- calc(s, fun=sd, na.rm=TRUE)
    cv_s <- sd_s/mean_s
    
    fun <- function(x) { sum(!is.na(x)) }
    n_s <- calc(s, fun = fun )
    
    writeRaster(min_s, filename = paste(dir_slack, "PM10_mes_", m, "_min.tif", sep=""))
    writeRaster(max_s, filename = paste(dir_slack, "PM10_mes_", m, "_max.tif", sep=""))
    writeRaster(mean_s, 
                filename = paste(dir_slack, "PM10_mes_", m, "_mean.tif", sep = ""),  
                overwrite = TRUE,
                format= "GTiff")
    #writeRaster(median_s, filename = paste(dir_slack, "PM10_mes_", m, "_median.tif", sep=""))
    writeRaster(sd_s, filename = paste(dir_slack, "PM10_mes_", m, "_sd.tif", sep=""))
    writeRaster(n_s, filename = paste(dir_slack, "PM10_mes_", m, "_n.tif", sep=""))
    writeRaster(cv_s, filename = paste(dir_slack, "PM10_mes_", m, "_cv.tif", sep=""))
    
    print(m)
    rm(s, min_s, max_s, sd_s, cv_s)
  }
  
}

