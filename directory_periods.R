
# Function that returns the address of the survey based on country, type and period


functionRoundAndSurvey <- function(pais, tipo, anio) {


if (tipo == "encuestas") {

# 1. reading dataset with surveys, round and year
planificacionSurveys <- readxl::read_xlsx("Inputs/Planeación - Armonización de Encuestas de Hogares.xlsx", sheet = "HH surveys")

# 2. Pivoting to transform excel
# 2.1 Getting list of years availables
yearsAvailable <- planificacionSurveys %>% 
                  dplyr::select(where(is.numeric)) %>% 
                  select(-"Total") %>% 
                  colnames()
# choosing survey depending on year and country and round depending on that information
planificacionSurveysPivot <- planificacionSurveys %>%
                    pivot_longer(cols=yearsAvailable,
                                 names_to='year',
                                 values_to='availability')

# choosing round depending on that information
round <- planificacionSurveysPivot %>% 
         filter(`País`== pais & `year`== anio & availability ==1) %>% 
         pull(`Ronda armonizada BID`)

survey <- planificacionSurveysPivot %>% 
          filter(`País`== pais & `year`== anio & availability ==1) %>% 
          pull(Encuesta)

base <- paste("//sdssrv03//surveys//harmonized//",pais,"//",survey,"//data_arm//",pais,"_",anio,round,"_BID.dta",sep = "")
# return database address
return(base)


}

if (tipo == "censos"){
 ##### to be done
  # if (pais=="SLV" & anio=="2007"){
  #   base <- direccionCensos
  #   # return database address
  #   return(base)
  #   
  # }
  # if (pais=="ARG" & anio=="2010"){
  #   base <- direccionCensos
  #   # return database address
  #   return(base)
  #   
    
    
  #}
  #temporal - hacer generalizable
base <- paste("C:/Users/MARIAREY/OneDrive - Inter-American Development Bank Group/Documents/Data Governance - SCL/Censos/clean/",pais,"/", pais,"_",anio, "_censusBID.dta",sep = "")  
  
}
}
