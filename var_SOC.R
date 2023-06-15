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
           ywomen = sum(yallsr18[sexo_ci == 2], na.rm = TRUE),
           hhywomen = max(ywomen, na.rm = TRUE),
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
  
  # creating a vector with initial column names
  initial_column_names <- names(data_filt)
  
  
  data_soc <- data_filt %>%
    # create principal variables
    mutate(jefa_ci = if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
           ylm_ci = as.double(ylm_ci), 
           ynlm_ci = as.double(ynlm_ci),
           pob_sfd = if_else(sexo_ci == 2 | afroind_ci == 1 | afroind_ci == 2 | dis_ci == 1, 1, 0),
           pob18_ci = as.numeric(edad_ci <= 18),
           pob65_ci = as.numeric(edad_ci >= 65),
           single_member = as.numeric(miembros_ci == 1)) %>%
    mutate(ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ynlm_ci), na.rm = TRUE)),
           yallsr18 = if_else(edad_ci >= 18, ytot_ci, NA_real_)) %>%
    group_by(anio_c, pais_c, idh_ch) %>%
    mutate(ytot_ch = if_else(single_member == 1, sum(ytot_ci, na.rm = TRUE), NA_real_),
           ytot_ch = pmax(0, ytot_ch),
           hhyallsr = if_else(single_member == 1, sum(yallsr18, na.rm = TRUE), NA_real_),
           hhyallsr = pmax(0, hhyallsr),
           ywomen = sum(yallsr18[sexo_ci == 2], na.rm = TRUE),
           hhywomen = max(ywomen, na.rm = TRUE),
           shareylmfem_ch = hhywomen / hhyallsr,
           jefa_ch = if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
           miembro6_ch = as.numeric(any(edad_ci < 6 & relacion_ci > 0 & relacion_ci <= 5)),
           miembro65_ch = as.numeric(any(edad_ci >= 65 & relacion_ci > 0 & relacion_ci <= 5)),
           miembro6y16_ch = as.numeric(any(edad_ci >=6 & edad_ci <=16  & relacion_ci > 0 & relacion_ci <= 5)),
           perceptor_ci = if_else(ytot_ci > 0, sum(miembros_ci, na.rm = TRUE), NA_real_),
           perceptor_ch = suppressWarnings(max(perceptor_ci, na.rm = TRUE))) %>%
    ungroup() %>% 
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition 
      pc_ytot_ch = ifelse(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = ifelse(pc_ytot_ch <= 0, NA, pc_ytot_ch),
      # extreme poverty 3.1
      poor31 = ifelse(!is.na(pc_ytot_ch) & pc_ytot_ch < lp31_ci, 1, 0),
      # poverty 5.0
      poor = ifelse(!is.na(pc_ytot_ch) & pc_ytot_ch < lp5_ci, 1, 0),
      # vulnerable 5 y 12.4 usd per capita por día
      vulnerable = ifelse(!is.na(pc_ytot_ch) & pc_ytot_ch >= lp5_ci & pc_ytot_ch < lp31_ci*4, 1, 0),
      # middle $12.4 y $62 per capita por día
      middle = ifelse(!is.na(pc_ytot_ch) & pc_ytot_ch >= lp31_ci*4 & pc_ytot_ch < lp31_ci*20, 1, 0),
      # rich more than 62 USD per capita por día
      rich = ifelse(!is.na(pc_ytot_ch) & pc_ytot_ch >= lp31_ci*20, 1, 0),
      # Define area and sex based on zona_c and sexo_ci respectively, 
      income_category = case_when(
        (pc_ytot_ch < lp31_ci ~ "extreme"),  # extreme poverty
        (pc_ytot_ch >= lp31_ci) & (pc_ytot_ch < lp5_ci) ~ "poverty",  # poverty
        (pc_ytot_ch >= lp5_ci) & (pc_ytot_ch < lp31_ci*4) ~ "vulnerable",  # vulnerable
        (pc_ytot_ch >= lp31_ci*4) & (pc_ytot_ch < lp31_ci*20) ~ "middle",  # middle class
        (pc_ytot_ch >= lp31_ci*20) ~ "rich", 
        TRUE ~ NA_character_),  # rich,
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
      hhfem_ch = ifelse(hhywomen >= .5, 1, ifelse(is.na(yallsr18), NA, 0)),
      # remesas
      indexrem = ifelse(jefe_ci == 1 & !is.na(remesas_ch) & remesas_ch > 0, 1, NA),
      ylmprixh = ylmpri_ci / (horaspri_ci * 4.34),
      #vivienda 
      hacinamiento_ch = nmiembros_ch / cuartos_ch,
      #demografia dependencia 
      depen_ch = nmiembros_ch / perceptor_ch
    ) %>% 
    group_by(idh_ch) %>%
    mutate(
      ytot_ch = sum(ytot_ci, na.rm = TRUE),
      pc_ytot_ch = ytot_ch / n(),
      suma1 = cumsum(pc_ytot_ch)
    ) %>%
    ungroup() %>%
    mutate(
      quintile = cut(suma1, breaks = quantile(suma1, probs = seq(0, 1, by = 0.2), na.rm = TRUE), labels = FALSE)
    )
  
  # then select only added variables and specific columns
  new_column_names <- setdiff(names(data_soc), initial_column_names)
  
  select_column_names <- c(new_column_names, 
                           "region_BID_c", "pais_c", "ine01","estrato_ci","area", "zona_c", "relacion_ci", 
                           "idh_ch", "factor_ch", "factor_ci", "idp_ci")
  
  data_soc <- select(data_soc, all_of(select_column_names))
  
  
}