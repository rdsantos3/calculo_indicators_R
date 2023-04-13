# Script para generar variables intermedias - LMK

# 1. Censos

if (tipo == "censos") {
  data_lmk <- data_filt %>% 
    mutate(pet = ifelse((edad_ci>=15 & edad_ci<65),1,0),
           pea = ifelse((condocup_ci == 1 | condocup_ci == 2 ) & pet==1,1,0),
           age_15_64_lmk = ifelse(edad_ci>=15 & edad_ci<65, 1, 0), 
           age_25_64_lmk = ifelse(edad_ci>=25 & edad_ci<65, 1, 0), 
           age_65_mas_lmk = ifelse(edad_ci>=65, 1, 0),
           patron = case_when(condocup_ci==1 & categopri_ci==1 ~ 1, 
                              condocup_ci==1 & categopri_ci!=1 ~ 0),
           asalariado = case_when(condocup_ci==1 & categopri_ci==3 ~ 1, 
                                  condocup_ci==1 & categopri_ci!=3 ~ 0),
           ctapropia = case_when(condocup_ci==1 & categopri_ci==2 ~ 1, 
                                 condocup_ci==1 & categopri_ci!=2 ~ 0),
           sinremuner = case_when(condocup_ci==1 & categopri_ci==4 ~ 1, 
                                  condocup_ci==1 & categopri_ci!=4 ~ 0)) %>% 
    select(pet:sinremuner, region_BID_c, pais_c, geolev1, estrato_ci, zona_c, relacion_ci, idh_ch, factor_ch, factor_ci, 
           idp_ci) 
  
}

# 2. Encuestas

if (tipo == "encuestas") {

data_lmk <- data_filt %>% 
  mutate(pet = ifelse((condocup_ci == 1 | condocup_ci == 2 | condocup_ci == 3),1,0),
         pea = ifelse((condocup_ci == 1 | condocup_ci == 2 ),1,0),
         age_15_64_lmk = ifelse(edad_ci>=15 & edad_ci<65, 1, 0), 
         age_25_64_lmk = ifelse(edad_ci>=25 & edad_ci<65, 1, 0), 
         age_65_mas_lmk = ifelse(edad_ci>=65, 1, 0),
         patron = case_when(condocup_ci==1 & categopri_ci==1 ~ 1, 
                            condocup_ci==1 & categopri_ci!=1 ~ 0),
         asalariado = case_when(condocup_ci==1 & categopri_ci==3 ~ 1, 
                                condocup_ci==1 & categopri_ci!=3 ~ 0),
         ctapropia = case_when(condocup_ci==1 & categopri_ci==2 ~ 1, 
                               condocup_ci==1 & categopri_ci!=2 ~ 0),
         sinremuner = case_when(condocup_ci==1 & categopri_ci==4 ~ 1, 
                                condocup_ci==1 & categopri_ci!=4 ~ 0),
         formal_aux = case_when(cotizando_ci ==1 ~1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="URY" & anio_c<=2000 ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL" ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI" & anio_c<2000 ~1,
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998 ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN" ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006 ~ 1,
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM" ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008 ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="COL" & anio_c<=1999~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="ECU"  ~ 1, 
                                afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BHS" ~ 1, 
                                TRUE ~ 0),
         formal_ci = case_when(formal_aux==1 & (condocup_ci==1 | condocup_ci==2) ~ 1, 
                               is.na(formal_ci) & (condocup_ci==1 | condocup_ci==2) ~ 0,
                               TRUE ~ formal_ci),
         pensiont_ci = case_when(pension_ci==1 | pensionsub_ci==1 ~ 1, 
                                 pension_ci==0 & pensionsub_ci==0 ~ 0, 
                                 is.na(pension_ci) & edad_ci >=65 ~ 0, 
                                  TRUE ~ NA_real_),
         aux_pensiont_ci=mean(pensiont_ci)) %>% 
  select(pet:aux_pensiont_ci, region_BID_c, pais_c, ine01, estrato_ci, zona_c, relacion_ci, idh_ch, factor_ch, factor_ci, 
         idp_ci)
}