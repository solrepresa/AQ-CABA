### Calidad del Aire en Buenos Aires
### 15/08/2019 La Plata, Argentina
### Sol Represa
### Archivo 20


# Elaboracion de modelos 

library(corrplot)
library(caret)
library(colorRamps)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1. Carga de datos  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


data <- readRDS("/home/usuario/Sol/aire_buenos/variables_estacion_aod_modelo.rds")

tabla <- data[[1]]  
for(i in 2:length(data)){ 
  datos <- data[[i]]
  tabla <- rbind(tabla, datos)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### Analisis de correlacion
tabla <- tabla[, -1]

# Evaluar correlacion de nuevo
corr <- cor(tabla[,  c(1:length(tabla))], method = "pearson", use = "complete.obs")
p.mat <- cor.mtest(tabla[, c(1:length(tabla))])
corrplot(corr, 
         method = "color",
         type = "lower",
         #insig = "p-value", 
         sig.level = 0.01, 
         diag = FALSE, 
         p.mat = round(p.mat$p, 3),
         tl.cex = 0.5)

# Se van: 
# BCSMASS correlaciona fuerte con OCSMASS
# OCSMASS
# SPEEDMAX correlaciona con SPEED y USTAR
# USTAR
# H1000
# CLDLOW

names(tabla)
tabla <- tabla[, -c(6, 9, 15, 17, 20, 21)]

## tabla completa
tabla <- tabla[complete.cases(tabla),]
# 3750 completa >> 2299

names(tabla)[16] <- "PM10"



#SAVE
#write.csv(tabla, "variables_estacion_aod_modelo.csv", row.names = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Armar base de entrenamiento

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

base <- read.csv("variables_estacion_aod_modelo.csv")


set.seed(123) #seteamos para obtener resultados reproducibles

i_entrena <- createDataPartition(y = base$PM10, 
                                 p = 0.8, 
                                 list = FALSE)
entrena <- base[i_entrena,]
test <- base[-i_entrena,]


rm(i_entrena)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Linear Regression model

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

model_lm <- caret::train(PM10 ~ . , 
                         data = entrena,
                         method = "lm",
                         preProcess= c("center", "scale"))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(123)

# Random forest con 10 fold cv - library(caret) ####

train.control <- trainControl(method = "cv",    #"repeatedcv", 
                              number = 10,
                            # repeats = 10,
                              search = "random") # Set up repeated k-fold cross-validation

model_rf <- train(PM10 ~ PS + RH + T + U + V + DMSSMASS + DUSMASS + SO2SMASS + SO4SMASS + 
                    SSSMASS + PBLH + PRECTOT + SPEED + ALBEDO + CLDHGH + aod, 
                  data = entrena,
                  method = 'rf', 
                  trControl = train.control,
                  #tuneGrid = data.frame(mtry = 1:34),
                  ntree = 1000,
                  # preProcess = c("scale", "center"),
                  importance = TRUE)

# Evaluar si vale la pena gaurdarlo >>
saveRDS(model_rf, file = "modelo_rf_1000tree.rds")

# (ntree = 1000) cv simple 10-fold
#   mtry  RMSE      Rsquared   MAE     
#   5     11.27593  0.6224408  6.981418

# (ntree = 100) cv repeates 10-fold * 10  >> incluye AOD
# mtry  RMSE      Rsquared   MAE     
# 9    11.49402  0.5835058  7.180369

# (ntree= 1000) Cross-Validated (10 fold, repeated 10 times) 
# mtry  RMSE      Rsquared   MAE     
# 7    11.35640  0.6021936  7.102989

model_rf <- readRDS("modelo_rf_1000tree.rds")

plot(varImp(model_rf))
plot(model_rf)


pred_rf <- predict(model_rf, test)

my_data <-  as.data.frame(cbind(predicted = pred_rf,
                                observed = test$PM10,
                                Error = abs(pred_rf - test$PM10)))

ggplot(my_data, aes(predicted, observed, colour = Error)) +
  geom_point(alpha = 0.8) + 
  #geom_smooth(method = lm, fullrange= TRUE, col = "gray38")+ 
  ggtitle("Random Forest: ntree = 1000, mtry = 5") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme_bw() +
  coord_fixed() +
  geom_abline(slope = 1, intercept = 0) +
  scale_colour_gradientn(colours = matlab.like(10)) +
  xlim(0,200)+
  ylim(0, 200)



RMSE = function(m, o){   # m = model, o = observed
  sqrt(mean((m - o)^2))
}

MAE <- function(m, o) { mean(abs(m - o)) }


RMSE(my_data$predicted, my_data$observed) # con el set de prueba  10.15742 (ntree = 1000)
MAE(my_data$predicted, my_data$observed) # con el set de prueba  6.825291 (ntree = 1000)


datos_rf <- data.frame(ID = row.names(test), PM10_medido = test$PM10, PM10_modelado = pred_rf)
#write.csv(datos_rf, file = "modelo_rf_1000.csv", row.names = F)



# Calculo de MAPE #

datos <- read.csv("modelo_rf_1000.csv") #solo datos test
pred_mod <- predict(model_rf, entrena)
datos_rf <- data.frame(PM10_medido = entrena$PM10, PM10_modelado = pred_mod)

datos <- rbind(datos, datos_rf)


datos$eror_prom <- abs(datos$PM10_medido - datos$PM10_modelado)/datos$PM10_medido
sum(datos$eror_prom)/nrow(datos)*100 

# PARA TODOS LOS DATOS MAPE = 12.93%
# PARA los datos del test es del 24.4 %


#Verificar RMSE y MAE para datos entrena

postResample(datos_rf$PM10_modelado, datos_rf$PM10_medido)

RMSE(datos_rf$PM10_modelado, datos_rf$PM10_medido) # con el set de entrenamiento  4.94 (ntree = 1000)
MAE(datos_rf$PM10_modelado, datos_rf$PM10_medido) # con el set de entrenamiento 2.92 (ntree = 1000)

