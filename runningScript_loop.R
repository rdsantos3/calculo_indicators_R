# Read your csv file

library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(parallel)
options(scipen = 999)

available_years <- read.csv("Inputs/running_survey.csv") %>% 
  filter(availability==1)
# if needed you can run by chunks of countries here

# Get unique combinations of country and year
unique_combinations <- unique(available_years[c("Pais", "year")])

# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]
  tipo <- "encuestas"
  
  # Source script
  source("scl_indicators.R")
  
  # Write output
  if (tipo == "encuestas") {
    write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
  }
  
  # Add more conditions for other types if needed
  if (tipo == "censos") {
    # do something
  }
}
