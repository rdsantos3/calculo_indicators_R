pais <- c("BRA", "COL", "CRI", "DOM", "ECU")
anio <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
tipo <- "encuestas"

# Loop over each country
for (country in pais) {
  # Loop over each year
  for (year in anio) {
    # Loop over each type
 
      # Set variables
      pais <- country
      anio <- year
      tipo <- tipo
      
      # Source script
      source("scl_indicators.R")
      
      # Write output
      if (tipo == "encuestas") {
        write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_wash_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
      }
      
      # Add more conditions for other types if needed
      if (tipo == "censos") {
        # do something
      }
    }
}
