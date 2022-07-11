

###############
#
# Poluttion Roses
# Quintero & Puchuncavi
# PM 2.5 & SO2
# Datos horarios
#
###############

#file.choose()
setwd("E:\\OneDrive\\Documentos\\Francisco\\Universidad\\2022\\Fund. Ciencia de Datos\\PROYECTO FINAL\\data\\Quintero\\")


library(openair)


quintero <- read.csv("quintero_v2.csv", sep=";",stringsAsFactors=FALSE, dec = ",")
quintero.2021 <- subset(quintero, anho == 2021)

summary(quintero.2021$pm25)
summary(quintero.2021$so2)

#################
# Poluttion Rose
#################

# Quintero ------------

pollutionRose(quintero.2021, ws="ws", wd="wd",
              pollutant = "pm25",
              
              par.settings=list(axis.line=list(col="lightgray")),
              col = c("#4f4f4f","#0a7cb9","#f9be00","#ff7f2f","#d7153a"))

pollutionRose(quintero.2021, ws="ws", wd="wd",
              pollutant = "so2",
              
              par.settings=list(axis.line=list(col="lightgray")),
              col = c("#4f4f4f","#0a7cb9","#f9be00","#ff7f2f","#d7153a"))

# Puchuncavi ---------

