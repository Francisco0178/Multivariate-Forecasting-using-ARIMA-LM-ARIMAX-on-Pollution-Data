#####################################################################
# 
# Autor: Francisco Bahamondes
# https://github.com/Francisco0178
#
# Los datos fueron obtenidos del sitio oficial del SINCA
# https://sinca.mma.gob.cl/index.php/
# 
# El data set contempla informacion de la estacion Quintero,
# desde el 2017-01-01 hasta el 2022-06-30, y esta compuesta por:
# 
# Contaminantes:
# - Pm 2.5                 (Diario) -> Quintero [痢/m設
# - SO2                    (Diario) -> Quintero [ppb]
#
# Variables Meteorologicas:
# - Temperatura            (Prom Diaria) -> Concon    [蚓]
# - Humedad                (Prom Diaria) -> Concon    [%]
# - Velocidad del viento   (Prom Diario) -> Quintero  [m/s]
# - Direccion del viento   (Prom Diario) -> Quintero  [財
# - Radiacion Global       (Prom Diario) -> Loncura   [W/m淫
# - Presion atmosferica    (Prom Diario) -> Loncura   [mmHg]
# 
#####################################################################

#file.choose()
setwd("E:\\OneDrive\\Documentos\\Francisco\\Universidad\\2022\\Fund. Ciencia de Datos\\PROYECTO FINAL\\data\\Quintero\\")

#############
# Librerias
#############

#install.packages("textshape")
#install.packages("hrbrthemes")
#install.packages("mice")
#install.packages("calendR")
#install.packages("zoo")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("stats")
#install.packages("hrbrthemes")
#install.packages("TSstudio")
#install.packages("reshape2")

library(tidyverse)
library(textshape)
library(mice)
library(ggplot2)
library(hrbrthemes)
library(calendR)
library(dplyr)
library(factoextra)
library(PerformanceAnalytics)
library(corrplot)
library(forecast)
library(tseries)
library(urca)
library(hrbrthemes)
library(TSstudio)
library(reshape2)

########################
# Seleccionamos la Data
########################

quintero <- read.csv("quintero_v1.csv", sep=";",stringsAsFactors=FALSE, dec = ",")
quintero$Fecha <- as.Date(as.character(quintero$Fecha), format = "%y%m%d")

##########################
# Imputacion de faltantes
##########################
my_imp = mice(quintero, m=5, method=c("","norm","norm","","","","","",""), maxit=20)
quintero = complete(my_imp,5)

####################
# Seleccion de a隳s
####################
quintero.2021 <- subset(quintero, Fecha >= "2021-01-01" & Fecha <= "2021-12-31")
quintero.2020 <- subset(quintero, Fecha >= "2020-01-01" & Fecha <= "2020-12-31")
quintero.2019 <- subset(quintero, Fecha >= "2019-01-01" & Fecha <= "2019-12-31")
quintero.2018 <- subset(quintero, Fecha >= "2018-01-01" & Fecha <= "2018-12-31")

#########
# Plot
#########

# Pm 2.5
ggplot(quintero, aes(x=Fecha)) + 
  geom_line(aes(y=pm25),
            color="#69b3a2") + 
  labs(title="Series de Tiempo del Pm 2.5", 
       subtitle="Comportamiento del Pm 2.5 los �ltimos 5 a隳s", 
       y="Pm 2.5 [痢/m設") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "4 month",
               date_labels = "%b %Y",
               limit=c(as.Date("2017-01-01"),as.Date("2022-06-30")))

# SO2
ggplot(quintero, aes(x=Fecha)) + 
  geom_line(aes(y=so2),
            color="#E7816B") + 
  labs(title="Series de Tiempo del SO2", 
       subtitle="Comportamiento del SO2 los �ltimos 5 a隳s", 
       y="SO2 [ppb]") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "4 month",
               date_labels = "%b %Y",
               limit=c(as.Date("2017-01-01"),as.Date("2022-06-30")))

#################
# Calendarios
#################

# Pm 2.5 - 2021
calendR(year = 2021,
        special.days = quintero.2021$pm25,
        low.col = "white",
        special.col = "#035B46",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

# Pm 2.5 - 2020
calendR(year = 2020,
        special.days = quintero.2020$pm25,
        low.col = "white",
        special.col = "#035B46",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

# Pm 2.5 - 2019
calendR(year = 2019,
        special.days = quintero.2019$pm25,
        low.col = "white",
        special.col = "#035B46",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

# Pm 2.5 - 2018
calendR(year = 2018,
        special.days = quintero.2018$pm25,
        low.col = "white",
        special.col = "#035B46",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

##################################################

# SO2 - 2021
calendR(year = 2021,
        special.days = quintero.2021$so2,
        low.col = "white",
        special.col = "#A91F00",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

# SO2 - 2020
calendR(year = 2020,
        special.days = quintero.2020$so2,
        low.col = "white",
        special.col = "#A91F00",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

# SO2 - 2019
calendR(year = 2019,
        special.days = quintero.2019$so2,
        low.col = "white",
        special.col = "#A91F00",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))

# SO2 - 2018
calendR(year = 2018,
        special.days = quintero.2018$so2,
        low.col = "white",
        special.col = "#A91F00",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait") +
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)+
  theme(axis.line=element_line(colour = "white"))


################
# Analisis PCA
################

# PCA ------
pca <- prcomp(quintero[,-1], scale=TRUE)
#summary(pca)
quintero.pca <- cbind(quintero, pca$x[,1:2])
cor(quintero[,-1], quintero.pca[,10:11])

fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x="PC1 (35.6%)",y="PC2 (19.9%)")+
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25))+
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25))

#####################
# Chart Correlation
#####################

# Pm 2.5
corrplot.mixed(cor(quintero[,-c(1,3)]),
               lower = "number", 
               upper = "circle",
               tl.col = "black")
# SO2
corrplot.mixed(cor(quintero[,-c(1:2)]),
               lower = "number", 
               upper = "circle",
               tl.col = "black")


##############
# Time Series
##############

# https://stackoverflow.com/questions/33128865/starting-a-daily-time-series-in-r

######################################################
# ------------------- Pm 2.5 ------------------------#
######################################################

pm25 <- quintero %>% select("pm25")
pm25.ts = ts(pm25,
             frequency=365,
             start = c(2017, as.numeric(format(as.Date("2017-01-01"), "%j"))),
             end = c(2022, as.numeric(format(as.Date("2022-06-30"), "%j"))))

#adf.test(pm25.ts, alternative="stationary") -> p-value = 0.01
#ts_info(pm25.ts)

######################################################
# --------------------- SO2 -------------------------#
######################################################

so2 <- quintero %>% select("so2")
so2.ts = ts(so2,
             frequency=365,
             start = c(2017, as.numeric(format(as.Date("2017-01-01"), "%j"))),
             end = c(2022, as.numeric(format(as.Date("2022-06-30"), "%j"))))

#adf.test(pm25.ts, alternative="stationary") # p-value = 0.01
#ts_info(so2.ts)


########################### 
# Training & Testing Data   
###########################

# Pm 2.5 -----------------------
pm25_par <- ts_split(pm25.ts, sample.out = 30)
train.pm25 <- pm25_par$train
test.pm25 <- pm25_par$test

#ts_info(train.pm25)
#ts_info(test.pm25)

# SO2 -----------------------
so2_par <- ts_split(so2.ts, sample.out = 30)
train.so2 <- so2_par$train
test.so2 <- so2_par$test

# -------------------------------------------------- #
# Funcion para obtener el performance de los modelos
# -------------------------------------------------- #

getPerformance = function(pred, val) {
  res = pred - val
  MAE = sum(abs(res))/length(val)
  RSS = sum(res^2)
  MSE = RSS/length(val)
  RMSE = sqrt(MSE)
  perf = data.frame(MAE, RSS, MSE, RMSE)
}

# -------------------------------------------------- #

################
# Modelo ARIMA
################

# Pm 2.5 ----------------------------

# Ver estacionariedad
#ur.kpss(train.pm25) %>% summary() 
#train.d1 <- diff(train,differences=1)
#ur.kpss(train.d1) %>% summary()

arima.pm25 <- auto.arima(train.pm25,
                         seasonal = T,
                         stepwise = T,
                         approximation = T)
arima.pm25
summary(arima.pm25)

# SO2 ------------------------------

arima.so2 <- auto.arima(train.so2,
                         seasonal = T,
                         stepwise = T,
                         approximation = T)
arima.so2
summary(arima.so2)

##################################
# Forecasting & Diagnostico ARIMA
##################################

# Pm 2.5 ----------------------------

pronostico.arima.pm25 <- forecast(arima.pm25, h=30)
test_forecast(actual = pm25.ts, forecast.obj = pronostico.arima.pm25, test = test.pm25)
accuracy(pronostico.arima.pm25, test.pm25)

pred.arima.pm25 <- as.vector(pronostico.arima.pm25$mean)

# SO2 ------------------------------

pronostico.arima.so2 <- forecast(arima.so2, h=30)
test_forecast(actual = so2.ts, forecast.obj = pronostico.arima.so2, test = test.so2)
accuracy(pronostico.arima.so2, test.so2)

pred.arima.so2 <- as.vector(pronostico.arima.so2$mean)


##########
# ARIMAX
##########

# Pm 2.5 ----------------------------

qntr.pm25 <- quintero %>% select("pm25","temperatura","humedad","ws","wd","radglobal")
qntr.pm25.ts = ts(qntr.pm25,
             frequency=365,
             start = c(2017, as.numeric(format(as.Date("2017-01-01"), "%j"))),
             end = c(2022, as.numeric(format(as.Date("2022-06-30"), "%j"))))

# SO2 ------------------------------

qntr.so2 <- quintero %>% select("so2","temperatura","humedad","ws","wd","radglobal")
qntr.so2.ts = ts(qntr.so2,
                  frequency=365,
                  start = c(2017, as.numeric(format(as.Date("2017-01-01"), "%j"))),
                  end = c(2022, as.numeric(format(as.Date("2022-06-30"), "%j"))))


########################### 
# Training & Testing Data   
###########################

# Pm 2.5 ----------------------------

qntr.pm25_par <- ts_split(qntr.pm25.ts, sample.out = 30)
train.qntr.pm25 <- qntr.pm25_par$train
test.qntr.pm25 <- qntr.pm25_par$test

# SO2 ------------------------------

qntr.so2_par <- ts_split(qntr.so2.ts, sample.out = 30)
train.qntr.so2 <- qntr.so2_par$train
test.qntr.so2 <- qntr.so2_par$test

# Prueba de estacionariedad de las variables
#ur.kpss(qntr.ts[,1]) %>% summary() #pm25
#ur.kpss(qntr.ts[,2]) %>% summary() #temp
#ur.kpss(qntr.ts[,3]) %>% summary() #hum
#ur.kpss(qntr.ts[,4]) %>% summary() #ws
#ur.kpss(qntr.ts[,5]) %>% summary() #wd
#ur.kpss(qntr.ts[,6]) %>% summary() #radg

# Pm 2.5 ----------------------------

# Modelo ARIMAX
modelo_arimax.pm25 <- auto.arima(train.qntr.pm25[,1],
                       xreg= train.qntr.pm25[,2:6],
                       seasonal = T,
                       stepwise = T,
                       approximation = T)

summary(modelo_arimax.pm25)
#checkresiduals(modelo_arimax.pm25)

# SO2 ------------------------------

modelo_arimax.so2 <- auto.arima(train.qntr.so2[,1],
                            xreg= train.qntr.so2[,2:6],
                            seasonal = T,
                            stepwise = T,
                            approximation = T)

summary(modelo_arimax.so2)

###################################
# Forecasting & Diagnostico ARIMAX
###################################

junio.2022 <- subset(quintero, Fecha >= "2022-06-01" & Fecha <= "2022-06-30")
regresores <- junio.2022 %>% select("temperatura","humedad","ws","wd","radglobal")
regresores <- as.matrix(regresores)

# Pm 2.5 ----------------------------

pronostico.arimax.pm25 <- forecast(modelo_arimax.pm25, h=30, xreg = regresores)
accuracy(pronostico.arimax.pm25, junio.2022$pm25)

pred.arimax.pm25 <- as.vector(pronostico.arimax.pm25$mean)
#test_forecast(actual = qntr.pm25.ts, forecast.obj = pronostico.arimax.pm25, test = test.qntr.pm25)

# SO2 ------------------------------

pronostico.arimax.so2 <- forecast(modelo_arimax.so2, h=30, xreg = regresores)
accuracy(pronostico.arimax.so2, junio.2022$pm25)

pred.arimax.so2 <- as.vector(pronostico.arimax.so2$mean)
#test_forecast(actual = qntr.so2.ts, forecast.obj = pronostico.arimax.so2, test = test.qntr.so2)


####################
# Linear Regression
####################
# https://datascienceplus.com/how-to-apply-linear-regression-in-r/
# https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/forecast.lm
# https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/tslm

# Pm 2.5 ----------------------------

fit.pm25 <- tslm(pm25 ~ temperatura + humedad + ws + wd + radglobal, data=train.qntr.pm25)
pronostico.lm.pm25 <- forecast(fit.pm25, newdata=as.data.frame(regresores), h=30)
accuracy(fit.pm25)

pred.lm.pm25 <- as.vector(pronostico.lm.pm25$mean)
#test_forecast(actual = qntr.pm25.ts, forecast.obj = pronostico.lm.pm25, test = test.qntr.pm25)

# SO2 ------------------------------

fit.so2 <- tslm(so2 ~ temperatura + humedad + ws + wd + radglobal, data=train.qntr.so2)
pronostico.lm.so2 <- forecast(fit.so2, newdata=as.data.frame(regresores), h=30)
accuracy(fit.so2)

pred.lm.so2 <- as.vector(pronostico.lm.so2$mean)
#test_forecast(actual = qntr.ts, forecast.obj = pronostico.lm.so2, test = test.qntr.so2)

#############################
# Comparacion de los modelos
#############################

# Pm 2.5 ---------------------------

# Actual vs ARIMA
gfg_data.arima <- data.frame(x = seq(as.Date("2022-06-01"),
                                  as.Date("2022-06-30"),"day"),
                          Actual = junio.2022$pm25,
                          ARIMA = pred.arima.pm25)
data_long.arima <- melt(gfg_data.arima, id = "x")
gfg_plot.arima <- ggplot(data_long.arima,            
                      aes(x = x,
                          y = value,
                          color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones PM 2.5 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot.arima

# Actual vs LM
gfg_data.lm <- data.frame(x = seq(as.Date("2022-06-01"),
                               as.Date("2022-06-30"),"day"),
                       Actual = junio.2022$pm25,
                       LM = pred.lm.pm25)
data_long.lm <- melt(gfg_data.lm, id = "x")
gfg_plot.lm <- ggplot(data_long.lm,            
                   aes(x = x,
                       y = value,
                       color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones PM 2.5 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot.lm

# Actual vs ARIMAX
gfg_data.arimax <- data.frame(x = seq(as.Date("2022-06-01"),
                                  as.Date("2022-06-30"),"day"),
                          Actual = junio.2022$pm25,
                          ARIMAX = pred.arimax.pm25)
data_long.arimax <- melt(gfg_data.arimax, id = "x")
gfg_plot.arimax <- ggplot(data_long.arimax,            
                      aes(x = x,
                          y = value,
                          color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones PM 2.5 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot.arimax

# Todos
gfg_data <- data.frame(x = seq(as.Date("2022-06-01"),
                               as.Date("2022-06-30"),"day"),
                       Actual = junio.2022$pm25,
                       ARIMA = pred.arima.pm25,
                       LM = pred.lm.pm25,
                       ARIMAX = pred.arimax.pm25)
data_long <- melt(gfg_data, id = "x")
gfg_plot <- ggplot(data_long,            
                   aes(x = x,
                       y = value,
                       color = variable)) +
            geom_line() +
            geom_point() +
            ggtitle("Predicciones PM 2.5 Junio 2022")+
            labs(y="Pm 2.5 [痢/m設",x="Dias")+
            theme(axis.text.x=element_text(angle=60, hjust=1),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black")) +
            scale_x_date(date_breaks = "1 day",
                         date_labels = "%d",
                         limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))
            
gfg_plot

# SO2  ----------------------------

# Actual vs ARIMA
gfg_data.arima <- data.frame(x = seq(as.Date("2022-06-01"),
                                     as.Date("2022-06-30"),"day"),
                             Actual = junio.2022$pm25,
                             ARIMA = pred.arima.so2)
data_long.arima <- melt(gfg_data.arima, id = "x")
gfg_plot.arima <- ggplot(data_long.arima,            
                         aes(x = x,
                             y = value,
                             color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones SO2 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot.arima

# Actual vs LM
gfg_data.lm <- data.frame(x = seq(as.Date("2022-06-01"),
                                  as.Date("2022-06-30"),"day"),
                          Actual = junio.2022$pm25,
                          LM = pred.lm.so2)
data_long.lm <- melt(gfg_data.lm, id = "x")
gfg_plot.lm <- ggplot(data_long.lm,            
                      aes(x = x,
                          y = value,
                          color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones SO2 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot.lm

# Actual vs ARIMAX
gfg_data.arimax <- data.frame(x = seq(as.Date("2022-06-01"),
                                      as.Date("2022-06-30"),"day"),
                              Actual = junio.2022$pm25,
                              ARIMAX = pred.arimax.so2)
data_long.arimax <- melt(gfg_data.arimax, id = "x")
gfg_plot.arimax <- ggplot(data_long.arimax,            
                          aes(x = x,
                              y = value,
                              color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones SO2 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot.arimax

# Todos
gfg_data <- data.frame(x = seq(as.Date("2022-06-01"),
                               as.Date("2022-06-30"),"day"),
                       Actual = junio.2022$pm25,
                       ARIMA = pred.arima.so2,
                       LM = pred.lm.so2,
                       ARIMAX = pred.arimax.so2)
data_long <- melt(gfg_data, id = "x")
gfg_plot <- ggplot(data_long,            
                   aes(x = x,
                       y = value,
                       color = variable)) +
  geom_line() +
  geom_point() +
  ggtitle("Predicciones SO2 Junio 2022")+
  labs(y="Pm 2.5 [痢/m設",x="Dias")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limit=c(as.Date("2022-06-01"),as.Date("2022-06-30")))

gfg_plot

##################### fin #####################
  


