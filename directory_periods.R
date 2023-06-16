

# Function that returns the address of the survey based on country, type and period

functionRoundAndSurvey <- function(pais, tipo, period) {


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
         filter(`País`== pais,`year`== period) %>% 
         pull(`Ronda armonizada BID`)

survey <- planificacionSurveysPivot %>% 
          filter(`País`== pais,`year`== period) %>% 
          pull(Encuesta)

base <- paste("//sdssrv03//surveys//harmonized//",pais,"//",survey,"//data_arm//",pais,"_",period,round,"_BID.dta",sep = "")
# return database address
return(base)


}

if (tipo == "censos"){
 ##### to be done
}
}