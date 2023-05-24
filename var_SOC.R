##### Script para generar variables intermedias - SCL/SCL

# 1. Censos

if (tipo == "censos") {
  
  data_soc <- data_filt %>% 
    mutate(jefa_ci = case_when(jefe_ci==1 & sexo_ci==2 ~ 1, 
                               jefe_ci==1 & sexo_ci==1 ~ 0, 
                               TRUE ~ NA_real_),
           ylm_ci=as.double(ylm_ci), ynlm_ci=as.double(ynlm_ci),
           urbano_ci = case_when(zona_c == 1 ~ 1, 
                                 is.na(zona_c) ~NA_real_, 
                                 TRUE ~ 0), 
           pob_sfd = case_when(sexo_ci == 2 ~ 1, 
                               afroind_ci == 1 ~ 1, 
                               afroind_ci == 2 ~ 1, 
                               dis_ci == 1 ~ 1, 
                               TRUE ~ 0), # variable requested for SFD - GDI
           pob18_ci = case_when(edad_ci<=18 ~ 1, 
                                is.na(edad_ci) ~ NA_real_, 
                                TRUE ~ 0), 
           pob65_ci = case_when(edad_ci>=65 ~ 1, 
                                is.na(edad_ci) ~ NA_real_, 
                                TRUE ~ 0),
           single_member = miembros_ci == 1,
           is_woman = sexo_ci == 2) %>% 
    mutate(ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ynlm_ci), na.rm=TRUE)),
           yallsr18 = ifelse(edad_ci>=18, ytot_ci, NA)) %>%
    group_by(anio_c, pais_c, idh_ch) %>%
    mutate(ytot_ch = ifelse(single_member, sum(ytot_ci,na.rm=TRUE), NA),
           ytot_ch = pmax(0, ytot_ch),
           hhyallsr = ifelse(single_member, sum(yallsr18,na.rm=TRUE), NA),
           hhyallsr = pmax(0, hhyallsr),
           ywomen = sum(yallsr18[is_woman], na.rm = TRUE),
           ywomen = ifelse(is.na(yallsr18), NA, ywomen),
           hhywomen = ifelse(ywomen>0, ywomen/hhyallsr, 0),  
           jefa_ch = ifelse(jefe_ci==1, sum(jefa_ci),0)) %>%
    ungroup() %>%
    mutate(
      pc_ytot_ch = ifelse(nmiembros_ch > 0, ytot_ch/nmiembros_ch, NA),
      pc_ytot_ch = ifelse(pc_ytot_ch <= 0, NA, pc_ytot_ch),
      # International poverty
      poor = ifelse(pc_ytot_ch < lp5_ci, 1, ifelse(pc_ytot_ch >= lp5_ci & !is.na(pc_ytot_ch), 0, NA)), 
      poor31 = ifelse(pc_ytot_ch < lp31_ci, 1, ifelse(pc_ytot_ch >= lp31_ci & !is.na(pc_ytot_ch), 0, NA)), 
      area = case_when(
        zona_c == 1 ~ "urban", 
        zona_c == 0 ~ "rural", 
        TRUE ~ NA_character_
      ),
      sex = case_when(
        sexo_ci == 2 ~ "women",
        sexo_ci == 1 ~ "men", 
        TRUE ~ NA_character_
      ),
      hhfem_ch = ifelse(hhywomen >= .5, 1, ifelse(is.na(yallsr18), NA, 0))
    ) %>%
    select(jefa_ci:hhfem_ch, region_BID_c, pais_c, geolev1, estrato_ci, zona_c, relacion_ci, idh_ch, factor_ch, factor_ci, 
           idp_ci)
  
}

# 2. Encuestas

if (tipo == "encuestas") {
  
  data_soc <- data_filt %>%
    # create principal variables
    mutate(jefa_ci = case_when(jefe_ci==1 & sexo_ci==2 ~ 1, 
                               jefe_ci==1 & sexo_ci==1 ~ 0, 
                               TRUE ~ NA_real_),
           ylm_ci = as.double(ylm_ci), 
           ynlm_ci = as.double(ynlm_ci),
           urbano_ci = case_when(zona_c == 1 ~ 1, 
                                 is.na(zona_c) ~ NA_real_, 
                                 TRUE ~ 0), 
           pob_sfd = case_when(sexo_ci == 2 ~ 1, 
                               afroind_ci == 1 ~ 1, 
                               afroind_ci == 2 ~ 1, 
                               dis_ci == 1 ~ 1, 
                               TRUE ~ 0),
           pob18_ci = case_when(edad_ci<=18 ~ 1, 
                                is.na(edad_ci) ~ NA_real_, 
                                TRUE ~ 0),
           pob65_ci = case_when(edad_ci>=65 ~ 1, 
                                is.na(edad_ci) ~ NA_real_, 
                                TRUE ~ 0), 
           single_member = miembros_ci == 1,
           is_woman = sexo_ci == 2) %>%
    # Replace rowwise and ifelse operations with vectorized alternatives
    mutate(ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ynlm_ci), na.rm = TRUE)),
           yallsr18 = ifelse(edad_ci >= 18, ytot_ci, NA)) %>%
    # Group by anio_c, pais_c, and idh_ch
    group_by(anio_c, pais_c, idh_ch) %>%
    # Mutate to compute aggregated values
    mutate(
      ytot_ch = ifelse(single_member, sum(ytot_ci, na.rm = TRUE), NA),
      ytot_ch = pmax(0, ytot_ch),
      hhyallsr = ifelse(single_member, sum(yallsr18, na.rm = TRUE), NA),
      hhyallsr = pmax(0, hhyallsr),
      ywomen = sum(yallsr18[is_woman], na.rm = TRUE),
      ywomen = ifelse(is.na(yallsr18), NA, ywomen),
      hhywomen = ifelse(ywomen > 0, ywomen / hhyallsr, 0),
      jefa_ch = ifelse(jefe_ci == 1, sum(jefa_ci), 0)
    ) %>%
    ungroup() %>% 
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition 
      pc_ytot_ch = ifelse(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = ifelse(pc_ytot_ch <= 0, NA, pc_ytot_ch),
      # International poverty
      poor = ifelse(pc_ytot_ch < lp5_ci, 1, ifelse(pc_ytot_ch >= lp5_ci & !is.na(pc_ytot_ch), 0, NA)), 
      poor31 = ifelse(pc_ytot_ch < lp31_ci, 1, ifelse(pc_ytot_ch >= lp31_ci & !is.na(pc_ytot_ch), 0, NA)), 
      # Define area and sex based on zona_c and sexo_ci respectively
      area = case_when(
        zona_c == 1 ~ "urban", 
        zona_c == 0 ~ "rural", 
        TRUE ~ NA_character_
      ),
      sex = case_when(
        sexo_ci == 2 ~ "women",
        sexo_ci == 1 ~ "men", 
        TRUE ~ NA_character_
      ),
      # Calculate hhfem_ch
      hhfem_ch = ifelse(hhywomen >= .5, 1, ifelse(is.na(yallsr18), NA, 0))
    ) %>%
    # Select required columns
    select(
      jefa_ci:hhfem_ch, region_BID_c, pais_c, ine01, estrato_ci, zona_c, relacion_ci, idh_ch, factor_ch, factor_ci, idp_ci
    )
  
  
}