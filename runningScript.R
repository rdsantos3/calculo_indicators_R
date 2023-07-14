# to do make it generalized so that you only have to specity country, type and year.
# Path of the data
library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(parallel)

pais<-"ARG"
anio<-"2010"
tipo<-"censos"
# geolevel: country, geolev1, ine01
geoLevel <- "geolev1"
# temporal directory censos, only works for SLV 2007 and ARG 2010
direccionCensos <- "C://Users//DCOR//OneDrive - Inter-American Development Bank Group//Documents//Indicadores encuestas de hogares//Indicadores//censos//ARG_2010_censusBID.dta"

source("scl_indicators.R")
if (tipo == "encuestas") {
  write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais,"_",anio,".csv",sep = ""), row.names=FALSE)
}

if (tipo=="censos"){
  write.csv(data_total, paste("Outputs/indicadores_censos_hogares_", pais,"_",anio,".csv",sep = ""), row.names=FALSE)
}