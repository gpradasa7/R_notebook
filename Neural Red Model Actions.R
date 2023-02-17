#Instalar librerias
#install.packages(c("quantmod", "lubridate", "dplyr", "tidyr", 
#                   "caret", "neuralnet", "NeuralNetTools", "randomForest"))

#Invocar librerias
library(quantmod)
library(lubridate)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)

hoy <- today()

acciones <- getSymbols('CIB', from = '2016-01-01', to = hoy, src = 'yahoo',
                       auto.assign = F)[,6]

plot(acciones)

acciones <- as.data.frame(acciones)
acciones$fecha <- rownames(acciones)
rownames(acciones) <- NULL
names(acciones) = c("Precio","Fecha")
acciones$Fecha = as.Date(acciones$Fecha)

rango_fecha = (hoy + 1) : (hoy + 30)
Precio = as.numeric(NA)
rango_fecha = as.data.frame(cbind(Precio,rango_fecha))
rango_fecha$Fecha = as.Date(rango_fecha$rango_fecha)
rango_fecha$rango_fecha = NULL

acciones <- rbind(acciones,rango_fecha)



###############################################################################################
acciones$Fecha_dup = acciones$Fecha
acciones <- acciones %>% separate(Fecha, c('Año','Mes','Dia'))
acciones$Año = as.numeric(acciones$Año)
acciones$Mes = as.numeric(acciones$Mes)
acciones$Dia = as.numeric(acciones$Dia)

set.seed(1998)
acciones.sc <- as.data.frame(cbind(acciones$Precio, acciones$Fecha_dup, scale(acciones[,c(2:4)])))
names(acciones.sc)[1] = 'Precio'
names(acciones.sc)[2] = 'Fecha'
acciones.sc$Fecha = as.Date(acciones.sc$Fecha)

set.seed(1998)
train = createDataPartition(na.omit(subset(acciones, acciones$Fecha_dup < today()))$Precio,
                            p = 0.7, list = F)
test = rbind(acciones[-train,] , subset(acciones,acciones$Fecha_dup >= today()))
test.sc = as.data.frame(cbind(test$Precio,test$Fecha_dup, scale(test[,c(2,3,4)])))
names(test.sc)[1] = 'Precio'
names(test.sc)[2] = 'Fecha'
test.sc$Fecha = as.Date(test.sc$Fecha)

##############################################################################################
library(neuralnet)
library(NeuralNetTools)

mod = neuralnet(formula = Precio ~ Año + Mes + Dia, data = acciones.sc[train,], hidden = 2,
                threshold = 0.01, stepmax = 1e+05, rep = 1, linear.output = TRUE)
plotnet(mod)

pred <- compute(mod, test.sc)

datos = cbind(pred$net.result, test.sc)

error_abs = RMSE(datos$Precio,datos$`pred$net.result`,na.rm = TRUE)
error_por = error_abs /  datos[datos$Fecha == max(na.omit(datos)$Fecha),]$Precio * 100

ggplot() + 
  geom_line(data = datos, aes(x = Fecha, y = Precio, color = "Datos Reales")) +
  geom_line(data = datos, aes(x = Fecha, y = `pred$net.result`, color = "Datos Entrenados")) +
  labs(title="MODELO DE REDES NEURONALES ENTRENADO VS DATOS REALES", x="AÑO", y="PRECIO") +
  scale_color_discrete(name = "Leyenda", labels = c("Datos Reales", "Datos Entrenados"))+
  theme(legend.position = "top")


##################APLICACIÓN DEL MODELO RANDOM FOREST (BOSQUE ALEATORIO)###################

library(randomForest)

mod_rf = randomForest(Precio ~ Año + Mes + Dia, data = acciones[train,],
                      type = 'regression', ntree = 100)
pred_rf = predict(mod_rf, test)
datos_rf = cbind(pred_rf,test)

error_abs_rf = RMSE(datos_rf$Precio,datos_rf$pred_rf,na.rm = TRUE)

error_por_rf = error_abs_rf /  datos_rf[datos_rf$Fecha_dup == max(na.omit(datos_rf)$Fecha_dup),]$Precio * 100

ggplot() + 
  geom_line(data = datos_rf, aes(x = Fecha_dup, y = Precio, color = "Datos Reales")) +
  geom_line(data = datos_rf, aes(x = Fecha_dup, y = pred_rf, color = "Datos Entrenados")) +
  labs(title="MODELO DE BDSQUE ALEATORIO VS DATOS REALES", x="AÑO", y="PRECIO") +
  scale_color_discrete(name = "Leyenda", labels = c("Datos Entrenados", "Datos Reales"))+
  theme(legend.position = "top")
