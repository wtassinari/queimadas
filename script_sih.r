# INSTALANDO PACOTE
install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

# CARREGANDO PACOTES
library(microdatasus)
library(dplyr)


# DEFININDO UM VETOR DE VARIÁVEIS
vars_select = c("N_AIH", "ANO_CMPT", "MES_CMPT", "UF_ZI", "MUNIC_RES", "NASC", "IDADE", "SEXO", 
                "DIAG_PRINC", "QT_DIARIAS", "DT_INTER", "DT_SAIDA",  "VAL_SP", "VAL_TOT")

# Doenças respiratórias que podem ser ocasionadas por queimadas:

# J40 - Bronquite não especificada como aguda ou crônica
# J41 - Bronquite crônica simples e mucopurulenta
# J42 - Bronquite crônica não especificada
# J43 - Enfisema pulmonar
# J44 - Doença Pulmonar Obstrutiva Crônica (DPOC)
# J45 - Asma
# J46 - Estado de mal asmático
# J18 - Pneumonia não especificada
# J00-J06 - Infecções agudas das vias aéreas superiores (resfriados comuns, rinite aguda, sinusite)
# J20-J22 - Outras infecções agudas das vias aéreas inferiores (bronquiolites, bronquite aguda, infecção respiratória inferior aguda não especificada)

# DEFININDO AS CID DE INTERESSE
cids_select <- c("J40", "J41", "J42", "J43", "J44", "J45", "J46", "J18",
                 "J00", "J01", "J02", "J03", "J04", "J05", "J06",
                 "J20", "J21", "J22")

# Baixar os dados do SIH-RD para AM em 2024 (julho a dezembro)
# Baixando os dados do SIH para todo Brasil
sih_df <- fetch_datasus(year_start = 2024, year_end = 2024, 
                        month_start = 7, month_end = 12, 
                        uf = "AM", vars = vars_select,
                        information_system = "SIH-RD") %>%
  filter(DIAG_PRINC %in% cids_select)


library(tidyverse)
library(ggplot2)
library(gridExtra)

# Visualizando os dados
glimpse(sih_df)

# Transformando dados
sih_df$NASC <- as.Date(as.character(sih_df$NASC), format = "%Y%m%d")
sih_df$DT_INTER <- as.Date(as.character(sih_df$DT_INTER), format = "%Y%m%d")
sih_df$DT_SAIDA <- as.Date(as.character(sih_df$DT_SAIDA), format = "%Y%m%d")

# Transformar a N_AIH, que é numérica, em um formato de texto com 15 dígitos, preenchendo com zeros à esquerda quando necessário.
sih_df$N_AIH <- sprintf("%015.0f", as.numeric(sih_df$N_AIH)) 

# Criando features
sih_df <- sih_df %>%
  mutate(
    IDADE = as.numeric(format(DT_INTER, "%Y")) - as.numeric(format(NASC, "%Y"))
  )
