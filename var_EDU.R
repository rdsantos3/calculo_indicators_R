# Script para generar variables intermedias - EDU

# 1. Censos 

if (tipo == "censos") {
  data_edu <- data_filt %>% 
    mutate(age_25_mas = ifelse(edad_ci>=25, 1, 0),
           age_edu = case_when(edad_ci>=4 & edad_ci<=5 ~ "preescolar",
                               edad_ci>=6 & edad_ci<=11 ~ "primaria", 
                               edad_ci>= 12 & edad_ci<=17 ~ "secundaria", 
                               edad_ci>= 18 ~ "terciaria",
                               edad_ci>=18 & edad_ci <=20 ~ "terciaria_inc", 
                               TRUE ~ NA_character_),
           asis = case_when((asiste_ci==1 & aedu_ci==0) | (asiste_ci==1 & edad_ci<=5) ~ "preescolar",
                            (asiste_ci==1 & (aedu_ci>=0  & aedu_ci<  6) & !is.na(asiste_ci)) & edad_ci >=6 ~ "primaria",
                            ((aedu_ci>=6  & aedu_ci<12) & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 ~ "secundaria", 
                            (aedu_ci>=12 & asiste_ci == 1 & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 ~ "terciaria",
                            asiste_ci==0 ~"no asiste",
                            is.na(asiste_ci) ~NA_character_),
           asis_neta = case_when((asiste_ci==1 & aedu_ci==0) | (asiste_ci==1 & edad_ci<=5) & age_edu == "preescolar" ~ "preescolar",
                                 (asiste_ci==1 & (aedu_ci>=0  & aedu_ci<  6) & !is.na(asiste_ci)) & edad_ci >=6  & age_edu == "primaria" ~ "primaria",
                                 ((aedu_ci>=6  & aedu_ci<12) & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 & age_edu == "secundaria"  ~ "secundaria", 
                                 (aedu_ci>=12 & asiste_ci == 1 & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 & age_edu == "terciaria" ~ "terciaria",
                                 asiste_ci==0 ~"no asiste",
                                 is.na(asiste_ci) ~NA_character_),
           grupo_etario = case_when(edad_ci>=4 & edad_ci<=5 ~ "age_4_5", 
                                    edad_ci >=6 & edad_ci<=11 ~ "age_6_11", 
                                    edad_ci >= 12 & edad_ci<=14 ~ "age_12_14", 
                                    edad_ci >=15 & edad_ci <=17 ~ "age_15_17", 
                                    edad_ci >=18 & edad_ci <=23 ~"age_18_23", 
                                    edad_ci >=25 ~"25_mas"), 
           anos_edu = case_when(aedu_ci==0 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_0",
                                aedu_ci>=1 & aedu_ci <=5 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_1_5", 
                                aedu_ci>=6 & aedu_ci <=6 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_6", 
                                aedu_ci>=7 & aedu_ci <=11 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_7_11", 
                                aedu_ci>=12 & aedu_ci <=12 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_12",
                                aedu_ci>=13 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_13_mas"),
           age_15_24_edu = ifelse(edad_ci>=15 & edad_ci<=24 & !is.na(asiste_ci), 1, 0),
           age_15_29_edu = ifelse(edad_ci>=15 & edad_ci<=29 & !is.na(asiste_ci), 1, 0),
           age_18_24_edu = ifelse(edad_ci>=18 & edad_ci<=24 & !is.na(asiste_ci), 1, 0)) %>% 
    select(age_25_mas, age_edu, asis, asis_neta, grupo_etario, age_15_24_edu, age_15_29_edu, age_18_24_edu, 
           anos_edu, asiste_ci,  region_BID_c, pais_c, geolev1, estrato_ci, zona_c, relacion_ci, idh_ch, factor_ch, factor_ci, 
           idp_ci) 
}

# 2. Encuestas

if (tipo == "encuestas") {
  
data_edu <- data_filt %>% 
  mutate(age_25_mas = ifelse(edad_ci>=25, 1, 0),
         age_edu = case_when(edad_ci>=4 & edad_ci<=5 ~ "preescolar",
                             edad_ci>=6 & edad_ci<=11 ~ "primaria", 
                             edad_ci>= 12 & edad_ci<=17 ~ "secundaria", 
                             edad_ci>= 18 ~ "terciaria",
                             edad_ci>=18 & edad_ci <=20 ~ "terciaria_inc", 
                             TRUE ~ NA_character_),
         asis = case_when(asispre_ci==1 | (asiste_ci==1 & aedu_ci==0) | (asiste_ci==1 & edad_ci<=5) ~ "preescolar",
                         (asiste_ci==1 & (aedu_ci>=0  & aedu_ci<  6) & !is.na(asiste_ci)) & edad_ci >=6 ~ "primaria",
                         ((aedu_ci>=6  & aedu_ci<12) & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 ~ "secundaria", 
                         (aedu_ci>=12 & asiste_ci == 1 & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 ~ "terciaria",
                         asiste_ci==0 ~"no asiste",
                         is.na(asiste_ci) ~NA_character_),
         asis_neta = case_when(asispre_ci==1 | (asiste_ci==1 & aedu_ci==0) | (asiste_ci==1 & edad_ci<=5) & age_edu == "preescolar" ~ "preescolar",
                              (asiste_ci==1 & (aedu_ci>=0  & aedu_ci<  6) & !is.na(asiste_ci)) & edad_ci >=6  & age_edu == "primaria" ~ "primaria",
                              ((aedu_ci>=6  & aedu_ci<12) & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 & age_edu == "secundaria"  ~ "secundaria", 
                              (aedu_ci>=12 & asiste_ci == 1 & asiste_ci == 1 & !is.na(asiste_ci)) & edad_ci>=6 & age_edu == "terciaria" ~ "terciaria",
                              asiste_ci==0 ~"no asiste",
                              is.na(asiste_ci) ~NA_character_),
         grupo_etario = case_when(edad_ci>=4 & edad_ci<=5 ~ "age_4_5", 
                                  edad_ci >=6 & edad_ci<=11 ~ "age_6_11", 
                                  edad_ci >= 12 & edad_ci<=14 ~ "age_12_14", 
                                  edad_ci >=15 & edad_ci <=17 ~ "age_15_17", 
                                  edad_ci >=18 & edad_ci <=23 ~"age_18_23", 
                                  edad_ci >=25 ~"25_mas"), 
         anos_edu = case_when(aedu_ci==0 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_0",
                              aedu_ci>=1 & aedu_ci <=5 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_1_5", 
                              aedu_ci>=6 & aedu_ci <=6 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_6", 
                              aedu_ci>=7 & aedu_ci <=11 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_7_11", 
                              aedu_ci>=12 & aedu_ci <=12 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_12",
                              aedu_ci>=13 & (!is.na(aedu_ci) | !is.na(edad_ci)) & edad_ci >=25 ~ "anos_13_mas"),
         age_15_24_edu = ifelse(edad_ci>=15 & edad_ci<=24 & !is.na(asiste_ci), 1, 0),
         age_15_29_edu = ifelse(edad_ci>=15 & edad_ci<=29 & !is.na(asiste_ci), 1, 0),
         age_18_24_edu = ifelse(edad_ci>=18 & edad_ci<=24 & !is.na(asiste_ci), 1, 0)) %>% 
  select(age_25_mas, age_edu, asis, asis_neta, grupo_etario, age_15_24_edu, age_15_29_edu, age_18_24_edu, 
         anos_edu, asiste_ci,  region_BID_c, pais_c, ine01, estrato_ci, zona_c, relacion_ci, idh_ch, factor_ch, factor_ci, 
         idp_ci) 
}