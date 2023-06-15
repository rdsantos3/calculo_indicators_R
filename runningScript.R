
# to do make it generalized so that you only have to specity country, type and year.
# Path of the data

pais<-"SLV"
anio<-"2021"
tipo<-"encuestas"

source("scl_indicators.R")
if (tipo == "encuestas") {
write.csv(data_total, paste("Outputs\\indicadores_encuestas_hogares_", pais,"_",anio,".csv",sep = ""), row.names=FALSE)
}

if (tipo=="censos"){
  
}