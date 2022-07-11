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
# - Pm 2.5                 (Diario) -> Quintero [µg/m³]
# - SO2                    (Diario) -> Quintero [ppb]
#
# Variables Meteorologicas:
# - Temperatura            (Promedio Diario) -> Concon    [°C]
# - Humedad                (Promedio Diario) -> Concon    [%]
# - Velocidad del viento   (Promedio Diario) -> Quintero  [m/s]
# - Direccion del viento   (Promedio Diario) -> Quintero  [°]
# - Radiacion Global       (Promedio Diario) -> Loncura   [W/m²]
# - Presion atmosferica    (Promedio Diario) -> Loncura   [mmHg]
# 
#####################################################################

#file.choose()
setwd("E:\\OneDrive\\Documentos\\Francisco\\Universidad\\2022\\Fund. Ciencia de Datos\\PROYECTO FINAL\\data\\Puchuncavi\\")

#############
# Librerias
#############

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


########################
# Seleccionamos la Data
########################

puchuncavi <- read.csv("puchuncavi.csv", sep=";",stringsAsFactors=FALSE, dec = ",")
puchuncavi$Fecha <- as.Date(as.character(puchuncavi$Fecha), format = "%y%m%d")

##########################
# Imputacion de faltantes
##########################

my_imp = mice(puchuncavi, m=5, method=c("","mean","mean","",""), maxit=20)
puchuncavi = complete(my_imp,5)

#########
# Plot
#########

# Pm 2.5
ggplot(puchuncavi, aes(x=Fecha)) + 
  geom_line(aes(y=pm25),
            color="#69b3a2") + 
  labs(title="Series de Tiempo del Pm 2.5", 
       subtitle="Comportamiento del Pm 2.5 los últimos 5 años", 
       y="Pm 2.5 [µg/m³]") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_breaks = "4 month",
               date_labels = "%b %Y",
               limit=c(as.Date("2017-01-01"),as.Date("2022-06-30")))

# SO2
ggplot(puchuncavi, aes(x=Fecha)) + 
  geom_line(aes(y=so2),
            color="#E7816B") + 
  labs(title="Series de Tiempo del SO2", 
       subtitle="Comportamiento del SO2 los últimos 5 años", 
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
# Poluttion Rose
#################
puchuncavi.2021 <- subset(puchuncavi, Fecha >= "2021-01-01" & Fecha <= "2021-12-31")
ggplot(puchuncavi.2021, aes(x=Fecha)) + 
  geom_line(aes(y=wd),
            color="#E7816B")

windRose(puchuncavi.2021, ws="ws", wd="wd")

pollutionRose(puchuncavi.2021, ws="ws", wd="wd",
              pollutant = "so2",
              breaks = c(0,10,20,30,40,50),
              
              key = list(labels = c(">0 - 10",
                                    ">10 - 20",
                                    ">20 - 30",
                                    ">30 - 40",
                                    ">40 - 50")),
              par.settings=list(axis.line=list(col="lightgray")),
              col = c("#4f4f4f","#0a7cb9","#f9be00","#ff7f2f","#d7153a")
)
