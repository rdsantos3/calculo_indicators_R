# script general para crear indicadores de SCL

##### Ejemplo para calcular indicadores 

##### Libraries -----


library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library("writexl")
options(scipen = 999)

### Data ----

# to do make it generalized so that you only have to specity country, type and year.
# Path of the data


base <- "//sdssrv03//surveys//harmonized//SLV//EHPM//data_arm//SLV_2021a_BID.dta"

# Read data
data <- read_dta(base)


# define type
tipo <- "encuestas"

if (tipo == "censos") {
#Keep only needed variables
variables_censos <- readxl::read_xlsx("Inputs/D.7.1.3 Diccionario variables censos.xlsx")

varlist_censos <- variables_censos %>% 
  filter(!is.na(Variable))

data_filt <- data[,varlist_censos$Variable]
}

if (tipo == "encuestas") {
  # to do si no encuentra las variables ponlas en missing
  
#Keep only needed variables
variables_encuestas <- readxl::read_xlsx("Inputs/D.1.1.4 Diccionario microdatos encuestas de hogares.xlsx") %>% 
  filter(!(Variable %in% c("region_ci", "afroind_ano_ci", "atención_ci")))

variables_encuestas <- variables_encuestas %>% 
  filter(!is.na(Variable))

data_filt <- data[,variables_encuestas$Variable]
}

#### Compute intermediate variables  ####

source("var_LMK.R")

source("var_EDU.R")

source("var_GDI.R")

source("var_SOC.R")


#### Join final data with intermediate variables #####

if (tipo == "censos") {

  
data_scl <- data_filt %>%  
  select(-c(afroind_ci)) %>% 
  left_join(data_lmk, by = c("region_BID_c", "pais_c","geolev1",  "estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
  left_join(data_edu, by = c("region_BID_c", "pais_c", "geolev1", "estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>%
  left_join(data_soc, by = c("region_BID_c", "pais_c", "geolev1", "estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
  left_join(data_gdi, by = c("region_BID_c", "pais_c", "geolev1","estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
  rename(year = anio_c, isoalpha3 = pais_c)

}

if (tipo == "encuestas") {


  
data_scl <- data_filt %>%  
  select(-c(afroind_ci)) %>% 
  left_join(data_lmk, by = c("region_BID_c", "pais_c","estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "idp_ci", "factor_ci")) %>% 
  left_join(data_edu, by = c("region_BID_c", "pais_c","estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "idp_ci", "factor_ci")) %>%
  left_join(data_soc, by = c("region_BID_c", "pais_c", "estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch","idp_ci", "factor_ci")) %>% 
  left_join(data_gdi, by = c("region_BID_c", "pais_c", "ine01","estrato_ci", "zona_c",
                             "relacion_ci", "idh_ch", "idp_ci", "factor_ci")) %>% 
#  left_join(base_geo, by = c("ine01" = "ine01", "pais_c" = "pais")) %>% 
    rename(year = anio_c, isoalpha3 = pais_c)
  
}

# Remove data we do not need and free memory
rm("data_lmk", "data_edu", "data_soc", "data_gdi", "data", "data_filt")
gc()

##### In construction: generate variables and disaa. ######

# todo: 
# 0. generar variables desagregaciones como en la base general. done
# 1. incluir distintas desagregaciones forma más eficiente.
# 2. Incluir variables de calidad coeficiente de variación, N, var.
# 3. Incluir la parte de condiciones (ya quedo para poner condiciones, ahora faltaría incorporar el excel que controle las condiciones).
# 4. Agregar variables de calidad 
# 5. Si es posible y no toma mucho tiempo en correr agregar componente de survey

# function to restrict combinations
evaluatingFilter <- function(x, variable) {
  # Initialize result as FALSE. It will be set to TRUE if the conditions are met.
  result<-FALSE
  
  # Iterate over each element in variable
  for(condicionExcluyente in variable) {

    # If more than one element of 'condicionExcluyente' is found in 'x'
    # The 'unlist' function is used to flatten 'condicionExcluyente' into a vector
    # The '%in%' operator is used to find matching elements in 'x'
    # The 'sum' function counts the number of TRUEs (matches found)
    
    # If condition is met, set result to TRUE

    if(sum(unlist(condicionExcluyente) %in% x)==length(condicionExcluyente)){
      result<-(TRUE)}
    else{
      # If condition is not met, continue with the next iteration of the loop
      next
    }
  }
  return(result)
}

# function to calculate pct with conditions. 

scl_pct <- function(.data, .nombre, .condicion1, .condicion2, .group_vars) {
  
  # Convert conditions to expressions
  .condicion1 <- rlang::parse_expr(.condicion1)
  .condicion2 <- rlang::parse_expr(.condicion2)
  
  if (!is.null(.group_vars)) {
    data_aux <- .data %>%
      dplyr::group_by_at(.group_vars) %>%
      dplyr::summarise(
        value = sum(factor_ci[!!.condicion1], na.rm=TRUE) / sum(factor_ci[!!.condicion2], na.rm=TRUE),
        indicator = .nombre,
        se = sqrt(stats::var(!!.condicion1, na.rm = TRUE)),
        cv = se * 100 / (sum(value * factor_ci) * 100 / sum(factor_ci)),
        sample = length(na.omit(!!.condicion2))
      ) %>% 
      dplyr::ungroup()
  } else {
    data_aux <- .data %>%
      dplyr::summarise(
        value = sum(factor_ci[!!.condicion1], na.rm=TRUE) / sum(factor_ci[!!.condicion2], na.rm=TRUE),
        indicator = .nombre,
        se = sqrt(stats::var(!!.condicion1, na.rm = TRUE)),
        cv = se * 100 / (sum(value * factor_ci) * 100 / sum(factor_ci)),
        sample = length(na.omit(!!.condicion2))
      )
  }
  
  # Add disaggregation columns if not already present
  for (disaggregation_col in c("sex", "education_level", "disability", "quintile", "ethnicity", "migration", "age", "area", "year", "isoalpha3", "geolev1")) {
    if (!(disaggregation_col %in% colnames(data_aux))) {
      data_aux[[disaggregation_col]] <- "Total"
    }
  }
  
  # Rearrange columns
  data_aux <- data_aux %>% 
    dplyr::select(isoalpha3, year, geolev1, indicator, sex, education_level, disability, quintile, ethnicity, migration, age, area,
                  value, se, cv, sample)
  
  return(data_aux)
}


# Function to use CSV and create dataframe
calculate_indicators <- function(i, data, indicator_definitions) {
  
  # Extract each component of the current indicator definition
  ind <- indicator_definitions[i, ]
  aggregation_function <- ind$aggregation_function
  numerator_condition <- ind$numerator_condition
  denominator_condition <- ind$denominator_condition
  disaggregation <- strsplit(ind$disaggregation, ",")[[1]]
  excludeDisaggregation <- strsplit(strsplit(ind$excludeDisaggregation," ")[[1]],",")
  
  # Check aggregation function
  if (aggregation_function == "pct") {
    
    # Initialize a list to store results
    res_list <- list()
    
    # Generate all possible combinations of disaggregations
    disaggregation_combinations <- expand.grid(lapply(disaggregation, function(x) {
      if (x %in% c("year", "isoalpha3")) {
        return(x)
      } else {
        return(c(x, "Total"))
      }
    }))
    disaggregation_combinations <- unique(disaggregation_combinations) # Remove duplicates
    
    # Iterate over each disaggregation combination
    for (j in 1:nrow(disaggregation_combinations)) {
      
      current_disaggregation <- as.vector(unlist(disaggregation_combinations[j, ]))
      current_disaggregation <- current_disaggregation[current_disaggregation != "Total"]
      
      # Evaluate exclusion condition
      conditionDesaggregation <- evaluatingFilter(as.vector(t(current_disaggregation)),excludeDisaggregation)
      
      # If the condition for exclusion is not met, calculate the indicator
      if(!conditionDesaggregation) {
        res <- scl_pct(data, ind$indicator_name, numerator_condition, denominator_condition, current_disaggregation)
        res_list[[j]] <- res
      }
    }
    
    # Combine all disaggregated and total results
    res <- do.call(rbind, res_list)
  } else {
    # Handle other aggregation functions if needed
  }
  
  return(res)
}

##### Use parallel programming -----

num_cores <- detectCores() - 1  # number of cores to use, often set to one less than the total available
cl <- makeCluster(num_cores)

# Export data, indicator definitions and the necessary functions to the cluster
clusterExport(cl, c("data_scl", "indicator_definitions", "scl_pct", "calculate_indicators", "evaluatingFilter"))

# Load necessary packages on each node of the cluster
clusterEvalQ(cl, {
  library(magrittr)
  library(dplyr)
  library(rlang)
})

is_haven_labelled <- function(x) {
  inherits(x, "haven_labelled")
}

# Convert all haven_labelled columns to numeric
data_scl <- data_scl %>%
  mutate(across(where(is_haven_labelled), as.numeric))

# Call the function in parallel
results <- parLapply(cl, 1:nrow(indicator_definitions), calculate_indicators, data_scl, indicator_definitions)

# Combine results
data_total <- do.call(rbind, results)

# Stop the cluster
stopCluster(cl)

# remove NA 

# disaggregations to remove NA
# to do add this to the code so that they are removed
vars_to_check <- c("sex", "disability", "ethnicity")

data_total <- data_total %>%
  purrr::reduce(vars_to_check, function(data, var) {
    data %>% dplyr::filter(!is.na(.data[[var]]))
  }, .init = .)
