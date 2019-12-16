### Calidad del Aire en Buenos Aires
### 11/10/2019 La Plata, Argentina
### Sol Represa
### Archivo 23


# Objetivo: Extraer datos de sitios donde se mido PM10 y del modelo de PM10

library(dplyr)
library(raster)
library(ggplot2)
library(colorRamps)
library(reshape2)
library(lubridate)
library(openair)

## Estaciones ####
dire = "/home/usuario/Sol/aire_buenos/modelo_rf_1000/salida/"
id <- dir(dire, pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..


## Extraer info MODIS para sitios de monitoreo #

# Sitios CABA
sitios <- read.csv("estaciones_caba.csv", sep = ";", dec = ".")
sitios <- data.frame(lat = sitios$lat, long = sitios$long, sitios = sitios$Estacion)

# Sitios en La Plata
sitios_LP <- read.csv("sitios_monitoreo_LP.csv", sep=";", stringsAsFactors = FALSE)
sitios_LP <- data.frame(lat = sitios_LP$lat, long = sitios_LP$long, sitios = sitios_LP$Sitio)

# Unir
sitios <- rbind(sitios, sitios_LP)
names(sitios) <- c("Latitud", "Longitud", "Codigo")
rm(sitios_LP)

sitios <- sitios[order(sitios$Codigo),]
sitios_names <- t(sitios$Codigo)

sit <- data.frame(sitios$Longitud, sitios$Latitud)


# Extraer info en puntos de sitios de monitoreo
MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste(dire, id[i], sep = "")) # abrir geotiff como raster
  cod_sat <- substring(id[i], 9, 15) #tomar dato de la fecha del nombre
  MODIS_ext <- raster::extract(MODIS, sit)
  MODIS_ext <- t(MODIS_ext)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$cod_sat  <- cod_sat #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 


MODIS_point$date <- as.Date(MODIS_point$cod_sat, tz= "GMT", format = "%Y%j")
MODIS_point <- data.frame(MODIS_point[,length(MODIS_point)], MODIS_point[,1:(length(MODIS_point)-1)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", as.character(sitios_names))
MODIS_point <- MODIS_point[,-12] #eliminar el cod_sat

#write.csv(MODIS_point, file="PM10_modelado_sitios_monitoreo.csv", row.names = FALSE)



# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Cruzar con mediciones terrestres

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

IDW <- read.csv( "PM10_modelado_sitios_monitoreo.csv")

## Mediciones en La Plata
LP <- read.csv("PM10_monitoreo_LP.csv", sep = ";", dec=",", stringsAsFactors = FALSE)
LP$Fecha.inicio <- as.Date(LP$Fecha.inicio, format = "%d/%m/%y")
LP$Fecha.final <- as.Date(LP$Fecha.final, format = "%d/%m/%y")


mean(LP$Fecha.final-LP$Fecha.inicio)
# Hay un promedio de 2.7 dias que componen las mediciones


IDW$date <- as.character(IDW$date)

salida <- data.frame()

for( i in 1:nrow(LP)){   #avanza por toda la tabla de Dani
  ini <-  as.character(LP$Fecha.inicio[i]) #fecha inicio
  fin <- as.character(LP$Fecha.final[i]) #fecha final
  
  if( ini >= IDW$date[1] && fin <= IDW$date[nrow(IDW)] ){   #estructura de control: tiene que estar dentro de las fechas modeladas
    j = 1
    
    while( IDW$date[j] < ini ){ #recorro buscando el archivo cercano a inicio
      #  print(j)
      j = j + 1
    }
    
    k = 1
    while(IDW$date[k] < fin){  #recorro buscando el archivo cercano a fin
      #  print(k)
      k = k + 1
    }
    
    #IDW$date[j:(k-1)] # lapso de fechas
    
    data <- data.frame( PM_medido = LP$Concentracion..ug.m3.[i],
                        PM_modelado = max(IDW[j:(k-1), LP$Sitio[i]], na.rm = TRUE), 
                      #  PM_modelado = mean(IDW[j:(k-1), LP$Sitio[i]], na.rm = TRUE), 
                        sitio = LP$Sitio[i],
                        ini = as.character(LP$Fecha.inicio[i]),
                        fin = as.character(LP$Fecha.final[i]),
                        n = length(j:(k-1)))
    
    salida <- rbind(salida, data) 
  }
}

salida <- salida[complete.cases(salida),]

#Recategorizar
salida$sitio <- as.character(salida$sitio)
salida[which(salida$sitio == "Colman"), 3] <- "Urbano"
salida[which(salida$sitio == "Porta"), 3] <- "Urbano"
salida[which(salida$sitio == "UTN"), 3] <- "Industrial"
salida[which(salida$sitio == "Lula"), 3] <- "Urbano"
salida[which(salida$sitio == "Laura"), 3] <- "Urbano"
salida[which(salida$sitio == "Dani"), 3] <- "Urbano"
salida[which(salida$sitio == "CIOp"), 3] <- "Urbano"

salida <- salida[-11,] # Es Inf
salida <- salida[-10,] # Se va MUCHO!

## Evaluamos el error del modelo

salida$Error <- abs(salida$PM_medido - salida$PM_modelado)

modelo_lm <- lm(salida$PM_medido ~ salida$PM_modelado)

RMSE = function(m, o){   # m = model, o = observed
  sqrt(mean((m - o)^2))}

MAE <- function(m, o) { mean(abs(m - o)) }


RMSE(salida$PM_modelado, salida$PM_medido) # 20.12777 mean - 18.69854 max
MAE(salida$PM_modelado, salida$PM_medido) # 18.74512 mean - 14.99416 max
cor.test(salida$PM_medido, salida$PM_modelado)
summary(modelo_lm)

ggplot(salida, aes( x = PM_modelado, y = PM_medido, colour = Error)) +
  geom_point(alpha = 0.8) + 
  #geom_smooth(method = lm, fullrange= TRUE, col = "gray38")+ 
  ggtitle("") +
  xlab(expression(paste("PM"[10]," predicho")))+
  ylab(expression(paste("PM"[10]," observado"))) +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme_bw() +
  coord_fixed() +
  geom_abline(slope = 1, intercept = 0) +
  scale_colour_gradientn(colours = matlab.like(10)) +
  xlim(0, 70)+
  ylim(0, 70) #+  facet_grid(~sitio)


salida$sitio <- factor(salida$sitio)

salida %>% group_by(sitio) %>% 
  summarise(RMSE = sqrt(mean((PM_modelado - PM_medido)^2)),
            MAE = mean(abs(PM_modelado - PM_medido)))


#write.csv(salida, file="PM10_modelado_error_sitios_monitoreo_LP.csv", row.names = FALSE )
salida <- read.csv("PM10_modelado_error_sitios_monitoreo_LP.csv")

format(as.Date(salida$ini[1]), format = "%Y%j")

salida$n <- as.factor(salida$n)

ggplot(salida, aes( x = n, y = Error, fill= sitio)) +
 # geom_bar(stat = "identity", position=position_dodge()) + 
  geom_boxplot(position = position_dodge(preserve = 'total'))+
  geom_point(position = position_dodge(0.75, preserve = 'total')) + 
  xlab("Imagenes diarias incluida") +
  ylab("Error") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme_bw()


# ANOVA en el error
boxplot(salida$Error ~ salida$sitio)
boxplot(salida$Error ~ salida$n)
c <- aov(Error ~ n * sitio , data = salida)
summary(c)


ggplot(salida, aes( x = n, y = Error)) +
  geom_boxplot(fill= "cadetblue") +
  geom_point(position = position_dodge(0.75, preserve = 'total')) + 
  xlab("Imagenes diarias incluida") +
  ylab("Error") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme_bw()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## GRAFICAS

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


salida$ini <- as.Date(salida$ini)

ggplot(salida) + 
  geom_point(aes( ini, PM_modelado), col = "red") +
  geom_line(aes( ini, PM_modelado), col = "red") +
  geom_point(aes (ini, PM_medido), col ="blue")+
  geom_line(aes (ini, PM_medido), col ="blue")+
  facet_grid( ~ sitio)

IDW <- read.csv( "PM10_modelado_sitios_monitoreo.csv")
IDW$date <- as.Date(IDW$date)
IDW <- IDW[, -c(2,3,4)]
tabla <- melt(IDW, id = "date")

tabla[which(tabla$variable == "Colman"), 4] <- "Urbano"
tabla[which(tabla$variable == "Porta"), 4] <- "Urbano"
tabla[which(tabla$variable == "UTN"), 4] <- "Industrial"
tabla[which(tabla$variable == "Lula"), 4] <- "Urbano"
tabla[which(tabla$variable == "Laura"), 4] <- "Urbano"
tabla[which(tabla$variable == "Dani"), 4] <- "Urbano"
tabla[which(tabla$variable == "CIOp"), 4] <- "Urbano"

names(tabla)[4] <- "Estacion"

ggplot(tabla) + 
  geom_line(aes(date, value)) +
  facet_grid(~variable) +
  theme_bw()


tabla$mes <- month(tabla$date)
tabla$year <- year(tabla$date)
tabla$mes <- factor( as.character(tabla$mes))
tabla$year <- factor( as.character(tabla$year))
tabla$Estacion <- factor( as.character(tabla$Estacion))
boxplot(tabla$value  ~ tabla$mes )
boxplot(tabla$value  ~ tabla$year )

# ANOVA
a <- aov(tabla$value  ~ tabla$mes * tabla$year * tabla$Estacion * tabla$variable )
summary(a)

# Comparar entre medias 
# Wilcoxon signed-rank test >> muestras no normales
# t-test muestras normales


pairs <- list( c(1, 7), c(7, 12))

lapply(pairs, function(pr) {
  t.test( value ~ Estacion, 
          data = tabla[tabla$mes %in% pr, c("value", "mes")] )
}
)

# si p-value es menor a 0.05 entonces las muestras difieren en sus medias

urbano <- tabla %>% filter(Estacion == "Urbano")
plot(as.factor(format(urbano$date, "%Y-%B")), 
     urbano[,3], 
     ylab="Concentracion",  col="grey", 
     frame.plot=FALSE)


# BOXPLOT MENSUALES ## 
ggplot(urbano, aes( x = format(date, "%Y-%m"),
                    y = value)) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  xlab(label = "") +
  ylab(label = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 6)])
  
# Tendencia ##
dani <- tabla %>% filter(variable == "Dani")
dani$date <- as.POSIXct(paste(dani$date, "00:00"), tz = "UTC")
names(dani)[3] <- "PM10"

TheilSen(dani, 
         pollutant = "PM10", 
         ylab=expression("PM"[10]~" (" *mu* "g.m"^-3 *")"), 
         slope.percent = TRUE,
         avg.time="month")



ind <- tabla %>%
  filter(Estacion == "Industrial")
ind$date <- as.POSIXct(paste(ind$date, "00:00"), tz = "UTC")
names(ind)[3] <- "PM10"

TheilSen(ind, 
         pollutant = "PM10", 
         ylab=expression("PM"[10]~" (" *mu* "g.m"^-3 *")"), 
         slope.percent = TRUE,
         avg.time="month")


# Calculo del error medio absoluto porcentual (MAPE) ####
salida$eror_prom <- salida$Error/salida$PM_medido
sum(salida$eror_prom)/nrow(salida)*100


# ahora por año
salida$year <- year(salida$ini)
salida %>% group_by(sitio, year) %>%
  summarise(error_porcent = sum(eror_prom)/n()*100)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Analisis de PM10 medido por Dani

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

datos <- read.csv("PM10_modelado_error_sitios_monitoreo_LP.csv")

summary(datos$PM_medido)

datos %>% group_by(sitio) %>% 
  summarise(min = min(PM_medido, na.rm = TRUE),
            mean = mean(PM_medido, na.rm = TRUE),
            median = median(PM_medido, na.rm = TRUE),
            max = max(PM_medido, na.rm = TRUE),
            rango = max(PM_medido, na.rm = TRUE) - min(PM_medido, na.rm = TRUE),
            num = length(PM_medido))  #n() IQR()  rango intercuartil

datos$ini <- as.Date(as.character(datos$ini))
datos$mes <- month(datos$ini)
datos$year <- year(datos$ini)

ggplot(datos) + 
  geom_point(aes(mes, PM_medido, col = sitio)) +
  #facet_grid(~ year) +
  theme_bw()

ggplot(datos, aes(factor(year), PM_medido)) +
  geom_boxplot() +
  facet_grid(~sitio)


# ANOVA en el error
boxplot(datos$PM_medido ~ datos$sitio)
boxplot(datos$PM_medido  ~ datos$mes )
c <- aov(PM_medido ~ sitio , data = datos)
summary(c)

#            Df Sum Sq Mean Sq F value Pr(>F)  
# sitio        1   1463  1462.6   4.343 0.0464 *



datos %>% group_by(sitio, year) %>%
  summarise(media_modelado = mean(PM_modelado),
            media_medido = mean(PM_medido))

