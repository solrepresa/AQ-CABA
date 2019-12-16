####### Calidad del Aire en Buenos Aires
####### 05/08/2019 La Plata, Argentina
####### Sol Represa
####### Archivo 9


# Objetivo:
# - Analisis de correlacion
# - Analisis de tendencia de AOD

# Se parte de las bases creadas en AV_8.R
# "tierra_MODIS_1km_est_3.csv"
# "tierra_MODIS_1km_est-buff_3.csv"


library(ggplot2)

#sin buffer
#tabla <- read.table("tierra_MODIS_1km_est_3.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

#con buffer
tabla <- read.table("tierra_MODIS_1km_est-buff_3.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)


tabla$date <- as.Date(tabla$date)
tabla$estacion <- factor(tabla$estacion)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 1 - Correlacionar datos de monitoreo continuo y MODIS ####
##     Todos los datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


cor.test(tabla$aod,tabla$tierra, method = "spearman") # NS
cor.test(tabla$aod,tabla$tierra, method = "pearson") # 0.1103498  p<0.005



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 2 - Plot de correlacion    ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## 2.1 Plot correlacion TODOS los datos AOD vs PM

ggplot(data = tabla, aes(x = tierra, y= aod, shape = estacion)) + 
  geom_point(na.rm = TRUE) + 
  theme_bw() +
  labs(y = "MODIS 550 nm AOD", x=expression(paste("PM"[10]," (" , mu,"g.m"^-3, ")")), 
       title = "Correlacion MODIS - Estaciones terrestres ")  +
  # geom_abline(aes(intercept=0, slope=1 )) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0))


## 2.2 Plot de correlacion por estacion

cors_PM10 <-tabla[complete.cases(tabla),] %>% 
  group_by(estacion) %>%
  summarize(pearson = round(cor.test(tierra, aod)$estimate, 2),
            pValue = cor.test(tierra, aod)$p.value,
            spearman = round(cor.test(tierra, aod, method = "spearman")$estimate, 2),
            pValue_s = cor.test(tierra, aod, method = "spearman")$p.value)

cors_PM10[which(cors_PM10$pValue < 0.05), 3] <- "*"
cors_PM10[which(cors_PM10$pValue > 0.05), 3] <- ""

cors_PM10[which(cors_PM10$pValue_s < 0.05), 5] <- "*"
cors_PM10[which(cors_PM10$pValue_s > 0.05), 5] <- ""

## PM10

png("correlacion_PM10-AOD.png", width = 500, height = 200) 
tabla %>%
  ggplot(aes( x = tierra, y = aod)) + 
  geom_point(  na.rm = TRUE,  fill = "#999999") +
  #geom_abline(aes(intercept=0, slope=1 ), col = "#999999", alpha = 0.6) +
  geom_smooth(method = "lm", na.rm = TRUE, se = FALSE, col = "#0072B2") + 
  facet_wrap( ~estacion, ncol =  4 ) +   #scales = "free 
  labs( x = expression(paste("PM"[10]," (" , mu,"g.m"^-3, ")")), 
        y = "MODIS 550 nm AOD",
        title = "") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  geom_text(data = cors_PM10, 
            aes(label = paste("r = ", pearson, pValue, sep="")), x= 100, y=0.75) +
  geom_text(data = cors_PM10, 
            aes(label = paste("rho = ", spearman, pValue_s, sep="")), x= 100, y=0.65)


dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 3 - Analisis de correlacion por estacion de monitoreo ####
##     PM10

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

estaciones <- levels(tabla$estacion)
salida <- data.frame()

for( i in 1:length(estaciones)){
  a <- tabla %>%
    filter( estacion == estaciones[i])
  cor <- data.frame(estacion = estaciones[i],
                    cor_kendall = round(as.numeric(cor.test(a$aod,  a$tierra, method = "kendall")$estimate), 2),
                    p_kendall = round(as.numeric(cor.test(a$aod,  a$tierra, method = "kendall")$p.value), 4) ,
                    cor_spearman = round(as.numeric(cor.test(a$aod, a$tierra, method = "spearman")$estimate), 2),
                    p_spearman = round(as.numeric(cor.test(a$aod, a$tierra, method = "spearman")$p.value), 4) ,
                    cor_pearson = round(as.numeric(cor.test(a$aod, a$tierra, method = "pearson")$estimate), 2),
                    p_cor = round(as.numeric(cor.test(a$aod, a$tierra, method = "pearson")$p.value), 4),
                    N = as.numeric(cor.test(a$aod, a$tierra, method = "pearson")$parameter) + 2)
  salida <- rbind(salida, cor)
  
}

View(salida) # solo da correlacion de Pearson y es muy debil 0.1


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 4 - Modelo lineal AOD - PM10 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



reg <- lm( tabla$tierra ~ tabla$aod )  
summary(reg) #R2 =  0.01
plot(reg)

reg_resid <- reg$residuals
shapiro.test(sample(reg_resid)) # si p < 0.05 entonces los residuos no son normales

