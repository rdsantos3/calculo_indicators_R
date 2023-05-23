# script general para crear indicadores de SCL

##### Ejemplo para calcular indicadores 

##### Preliminares -----
start.time <- Sys.time()
library(tidyverse)
library(haven)
library(srvyr)
options(scipen = 999)

### Data ----

base <- "//sdssrv03//surveys//harmonized//SLV//EHPM//data_arm//SLV_2020a_BID.dta"

data <- read_dta(base)

# definir tipo 

# to do poner if encuestas y censos, ahorita está para censos pero la idea es empezar con encuesta

tipo <- "encuestas"

#variables necesarias en censos remover el resto


if(tipo=="censos"){
  variables_censos <- readxl::read_xlsx("Inputs/D.7.1.3 Diccionario variables censos.xlsx")
  varlist_censos <- variables_censos %>% 
    filter(!is.na(Variable))
  
  data_filt <- data[,varlist_censos$Variable]
}else if(tipo=="encuestas"){ 
  #nota variables de salud no existen en la encuesat de Brasil
  variables_encuestas <- readxl::read_xlsx("Inputs/D.1.1.4 Diccionario microdatos encuestas de hogares.xlsx")
  #quito las variables de salud ya que no estan en todos las encuestas aún
  varlist_encuestas <- variables_encuestas %>%
    filter((Tematica!="Salud")&!is.na(Variable))
  data_filt <- data[,varlist_encuestas$Variable]
}




#### Data intermedia  ####

source("var_LMK.R")

source("var_EDU.R")

source("var_GDI.R")

# esto corre muy lento hay que hacerlo más eficiente
source("var_SOC.R")


# function para detectar variables de muestreo y convertir en survey design objetct
intoSurveyDesignObject <- function(.data) {
  if (!all(is.na(.data$upm_ci)) & !all(is.na(.data$estrato_ci)) ) {
    .data<- .data %>%
      as_survey_design(upm_ci, strata = estrato_ci, weight = factor_ch, nest = TRUE)
  } else if( !all(is.na(.data$upm_ci)) & !all(is.na(.data$estrato_ci)) ){
    .data<- .data %>%
      as_survey_design(upm_ci, weight = factor_ch, nest = TRUE)
  }
  else{ 
    .data<- .data %>%
      as_survey_design(weight = factor_ch, nest = TRUE)
  }
  return(.data)
}

#### Juntar bases con variables intermedias #####

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
    left_join(data_lmk, by = c("region_BID_c", "pais_c","ine01",  "estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
    left_join(data_edu, by = c("region_BID_c", "pais_c","ine01", "estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>%
    left_join(data_soc, by = c("region_BID_c", "pais_c","ine01", "estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
    left_join(data_gdi, by = c("region_BID_c", "pais_c", "ine01","estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>%
    rename(year = anio_c, isoalpha3 = pais_c)
  #left_join(base_geo, by = c("ine01" = "ine01", "pais_c" = "pais")) %>% #
  
  data_scl<-   intoSurveyDesignObject(data_scl)
}

#### Quitar las bases que no necesitamos y liberar espacio ####

# Quitar bases de datos innecesarias y liberar espacio
rm("data_lmk", "data_edu", "data_soc", "data_gdi", "data", "data_filt")
gc()

##### In construction: generate variables and disaa. ######

# todo: 
# 0. generar variables desagregaciones como en la base general. done
# 1. incluir distintas desagregaciones forma más eficiente.
# 2. Incluir variables de calidad coeficiente de variación, N, var.
# 3. Incluir la parte de condiciones (ya quedo para poner condiciones, ahora faltaría incorporar el excel que controle las condiciones).

# function to calculate pct with conditions. 

scl_pct <- function(.data, .nombre, .condicion1, .condicion2, .group_vars) {
  
  .condicion1 <- rlang::parse_expr(.condicion1)
  .condicion2 <- rlang::parse_expr(.condicion2)
  
  if (!is.null(.group_vars)) {
    data_aux <- .data %>%
      dplyr::group_by_at(.group_vars) %>%
      dplyr::summarize(proportion = survey_ratio(eval(.condicion1), eval(.condicion2),vartype = c("cv","se"),na.rm=TRUE),
                       indicator = .nombre,
                       n =sum(eval(.condicion2))) %>%
      rename(
        value = proportion,
        cv = proportion_cv,
        se = proportion_se,
        sample = n
      )%>% 
      dplyr::ungroup()
  } else {
    data_aux <- .data %>%
      dplyr::summarize(proportion = survey_ratio((.condicion1), (.condicion2),vartype = c("cv","se"),na.rm=TRUE),
                       indicator = .nombre,
                       n =sum((.condicion2))) %>%
      rename(
        value = proportion,
        cv = proportion_cv,
        se = proportion_se,
        sample = n
      )
  }
  
  # Add disaggregation columns if not already present
  # to do: here the geolev and year are treated as one disagregation
  for (disaggregation_col in c("sex", "education_level", "disability", "quintile", "ethnicity", "age", "area", "year", "isoalpha3", "geolev1")) {
    if (!(disaggregation_col %in% colnames(data_aux))) {
      data_aux[[disaggregation_col]] <- "Total"
    }
  }
  
  # Rearrange columns
  data_aux <- data_aux %>% 
    dplyr::select(isoalpha3, year, geolev1, indicator, sex, education_level, disability, quintile, ethnicity, age, area,
                  value, se, cv, sample)
  
  return(data_aux)
}


# function to use csv and create dataframe
calculate_indicators <- function(data, indicator_definitions) {
  results <- list()
  
  for (i in 1:nrow(indicator_definitions)) {
    ind <- indicator_definitions[i, ]
    aggregation_function <- ind$aggregation_function
    numerator_condition <- ind$numerator_condition
    denominator_condition <- ind$denominator_condition
    disaggregation <- strsplit(ind$disaggregation, ",")[[1]]
    
    if (aggregation_function == "pct") {
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
      
      for (j in 1:nrow(disaggregation_combinations)) {
        current_disaggregation <- as.vector(unlist(disaggregation_combinations[j, ]))
        current_disaggregation <- current_disaggregation[current_disaggregation != "Total"]
        print(indicator_definitions[i, ])
        res <- scl_pct(data, ind$indicator_name, numerator_condition, denominator_condition, current_disaggregation)
        res_list[[j]] <- res
      }
      
      # Combine all disaggregated and total results
      res <- do.call(rbind, res_list)
    } else {
      # Handle other aggregation functions if needed
    }
    
    results[[i]] <- res
  }
  
  final_result <- do.call(rbind, results)
  return(final_result)
}


# i need year and geolev as character
data_scl <- data_scl %>% 
  mutate(year = as.character(year))#, geolev1 = as.character(geolev1))

# read the indicators definitions in the csv
indicator_definitions <- read.csv("Inputs/idef.csv")

# use the function to compute indicators
data_total <- calculate_indicators(data_scl, indicator_definitions = indicator_definitions)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
