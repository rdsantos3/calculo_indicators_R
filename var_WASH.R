##### Script para generar variables intermedias - SCL/SCL de Agua y Saneamiento

# Agua Tratada
data$aguatrat_ch_1 <- ifelse(data$aguatrat_ch == 1, 1, 0)

# Agua Consumo
data$aguafconsumo_ch_0 <- ifelse(data$aguafconsumo_ch == 0, 1, 0)
data$aguafconsumo_ch_1 <- ifelse(data$aguafconsumo_ch == 1, 1, 0)
data$aguafconsumo_ch_2 <- ifelse(data$aguafconsumo_ch == 2, 1, 0)
data$aguafconsumo_ch_3 <- ifelse(data$aguafconsumo_ch == 3, 1, 0)
data$aguafconsumo_ch_4 <- ifelse(data$aguafconsumo_ch == 4, 1, 0)
data$aguafconsumo_ch_5 <- ifelse(data$aguafconsumo_ch == 5, 1, 0)
data$aguafconsumo_ch_6 <- ifelse(data$aguafconsumo_ch == 6, 1, 0)
data$aguafconsumo_ch_7 <- ifelse(data$aguafconsumo_ch == 7, 1, 0)
data$aguafconsumo_ch_8 <- ifelse(data$aguafconsumo_ch == 8, 1, 0)
data$aguafconsumo_ch_9 <- ifelse(data$aguafconsumo_ch == 9, 1, 0)
data$aguafconsumo_ch_10 <- ifelse(data$aguafconsumo_ch == 10, 1, 0)
data$aguafconsumo_ch_11 <- 0
data$aguafconsumo_ch_12 <- 0
data$aguafconsumo_ch_13 <- 0
data$aguafconsumo_ch_11 <- ifelse(data$aguafconsumo_ch == 8 | data$aguafconsumo_ch == 9, 1, 0)
data$aguafconsumo_ch_12 <- ifelse(data$aguafconsumo_ch >= 1 & data$aguafconsumo_ch <= 7, 1, 0)
data$aguafconsumo_ch_13 <- ifelse((data$aguafconsumo_ch >= 1 & data$aguafconsumo_ch <= 7) & 
                                    (data$aguadisp1_ch == 1 | (data$aguadisp1_ch == 9 & 
                                                                 (data$aguadisp2_ch == 2 | data$aguadisp2_ch == 3))) &
                                    (data$aguadist == 1 | data$aguadist == 2), 1, 0)

# Agua Fuente
data$aguafuente_ch_1 <- ifelse(data$aguafuente_ch >= 1 & data$aguafuente_ch <= 7 & 
                                 (data$aguadisp1_ch == 1 | (data$aguadisp1_ch == 9 & 
                                                              (data$aguadisp2_ch == 2 | data$aguadisp2_ch == 3))) &
                                 (data$aguadist == 1 | data$aguadist == 2), 1, 0)
data$aguafuente_ch_2 <- ifelse(data$aguafuente_ch == 2, 1, 0)
data$aguafuente_ch_3 <- ifelse(data$aguafuente_ch == 3, 1, 0)
data$aguafuente_ch_4 <- ifelse(data$aguafuente_ch == 4, 1, 0)
data$aguafuente_ch_5 <- ifelse(data$aguafuente_ch == 5, 1, 0)
data$aguafuente_ch_6 <- ifelse(data$aguafuente_ch == 6, 1, 0)
data$aguafuente_ch_7 <- ifelse(data$aguafuente_ch == 7, 1, 0)
data$aguafuente_ch_8 <- ifelse(data$aguafuente_ch == 8, 1, 0)
data$aguafuente_ch_9 <- ifelse(data$aguafuente_ch == 9, 1, 0)
data$aguafuente_ch_10 <- ifelse(data$aguafuente_ch == 10, 1, 0)
data$aguafuente_ch_01 <- ifelse(data$aguafuente_ch == 1, 1, 0)

# Agua Red
data$aguared_ch_1 <- ifelse(data$aguared_ch == 1, 1, 0)

# Agua Mala
data$aguamala_ch_1 <- ifelse(data$aguamala_ch == 1, 1, 0)

# Agua Mejorada
data$aguamejorada_ch_1 <- ifelse(data$aguamejorada_ch == 1, 1, 0)

# Agua Disponible
data$aguadisp_ch_1 <- ifelse(data$aguadisp1_ch == 9 & data$aguadisp2_ch == 9, 1, 0)
data$aguadisp_ch_2 <- ifelse(data$aguadisp2_ch == 3, 1, 0)
data$aguadisp_ch_3 <- ifelse(data$aguadisp1_ch == 1 | (data$aguadisp1_ch == 9 & 
                                                         (data$aguadisp2_ch == 2 | data$aguadisp2_ch == 3)), 1, 0)

# Agua Mide
data$aguamide_ch_1 <- ifelse(data$aguamide_ch == 1, 1, 0)

# Sin Bano
data$sinbano_ch_0 <- 0
data$sinbano_ch_2 <- 0
data$sinbano_ch_3 <- 0
data$sinbano_ch_1 <- 0
data$sinbano_ch_0 <- ifelse(data$bano_ch == 0, 1, 0)
data$sinbano_ch_1 <- ifelse(data$sinbano_ch == 1, 1, 0)
data$sinbano_ch_2 <- ifelse(data$sinbano_ch == 2, 1, 0)
data$sinbano_ch_3 <- ifelse(data$sinbano_ch == 3, 1, 0)

# Bano Disponible
data$bano_ch_1 <- ifelse(data$bano_ch == 1, 1, 0)
data$bano_ch_2 <- ifelse(data$bano_ch == 2, 1, 0)
data$bano_ch_3 <- ifelse(data$bano_ch == 3, 1, 0)
data$bano_ch_4 <- ifelse(data$bano_ch == 4, 1, 0)
data$bano_ch_5 <- ifelse(data$bano_ch == 5, 1, 0)
data$bano_ch_6 <- ifelse(data$bano_ch == 6, 1, 0)
data$bano_ch_7 <- ifelse(data$bano_ch >= 1 & data$bano_ch <= 3, 1, 0)
data$bano_ch_8 <- ifelse(data$bano_ch >= 4 & data$bano_ch <= 5, 1, 0)

# Bano Exclusivo
data$banoex_ch_1 <- ifelse(data$banoex_ch == 1, 1, 0)
data$banoex_ch_9 <- ifelse(data$banoex_ch == 9, 1, 0)