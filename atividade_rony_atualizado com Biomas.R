# Caracterização dos focos de queimadas entre os biomas brasileiros: estudo da série histórica  2003-2024
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Lendo os dados extraidos do TerraBrasilis
load("dados_inpe.RData")

# Formatando as datas do banco e retirando aqueles registros sem data
dadosBR <- dadosBR %>%
  mutate(Ano = year(ymd_hms(DataHora))) |>  # extrai o ano da coluna DataHora
  mutate(across(where(is.numeric), ~na_if(., -999)))

##################################################
## Agregando a Freqencia de focos de calor por ano
##################################################
library(dplyr)
library(lubridate)

dadosBR_ano <- dadosBR %>%
  mutate(Ano = year(as.Date(DataHora))) %>%
  filter(Ano < 2025) |>
  count(Ano, name = "QtdFocos")

ggplot(dadosBR_ano, aes(x = Ano, y = QtdFocos)) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(
    title = "Série Histórica Anual de Focos de Calor no Brasil",
    subtitle = "Contagem anual de focos detectados por satélites",
    x = "Ano",
    y = "Quantidade de Focos de Calor"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(dadosBR_ano$Ano), max(dadosBR_ano$Ano), 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

library(writexl)

write_xlsx(dadosBR_ano, path = "dadosBR_ano.xlsx")

##################################################
## Agregando a Freqencia de focos de calor por mes
##################################################

# Supondo que dadosBR$DataHora esteja no formato POSIXct ou Date
dadosBR_mes <- dadosBR %>%
  mutate(Mes = floor_date(as.Date(DataHora), "month")) %>%
  count(Mes, name = "QtdFocos")

# Criar uma sequência completa de meses no período de interesse
todos_meses <- data.frame(Mes = seq(min(dadosBR_mes$Mes), max(dadosBR_mes$Mes), by = "1 month"))

# Preencher com zero os meses ausentes
dadosBR_mes <- todos_meses %>%
  left_join(dadosBR_mes, by = "Mes") %>%
  replace_na(list(QtdFocos = 0))


ggplot(dadosBR_mes, aes(x = Mes, y = QtdFocos)) +
  geom_line(color = "darkred", linewidth = 1) +
  # geom_point(color = "black", size = 1.5) +
  labs(
    title = "Série Histórica de Focos de Calor no Brasil",
    subtitle = "Contagem mensal de focos detectados por satélites",
    x = "Ano",
    y = "Quantidade de Focos de Calor"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

#############################################################
## Agregando a Freqencia de focos de calor por mes e por bioma
##############################################################

dadosBR_bioma_mes <- dadosBR %>%
  mutate(Mes = floor_date(as.Date(DataHora), "month")) %>%
  count(Mes, Bioma, name = "QtdFocos") |>
  filter(!is.na(Bioma) & Bioma != " ")

dadosBR_bioma_mes <- dadosBR %>%
  mutate(Mes = floor_date(as.Date(DataHora), "month")) %>%
  count(Mes, Bioma, name = "QtdFocos") %>%
  filter(!is.na(Bioma) & Bioma != "" & QtdFocos > 1)

# Filtrar biomas válidos
biomas_validos <- dadosBR %>%
  filter(!is.na(Bioma) & Bioma != "") %>%
  distinct(Bioma) %>%
  pull(Bioma)

# Expandir somente com biomas válidos
# Se for algo tipo "2022-01", adiciona "-01" e transforma em Date
dadosBR_bioma_mes$Mes <- as.Date(paste0(dadosBR_bioma_mes$Mes, "-01"))

# Agora sim expande
todos_mes_bioma <- expand.grid(
  Mes = seq.Date(min(dadosBR_bioma_mes$Mes), max(dadosBR_bioma_mes$Mes), by = "month"),
  Bioma = biomas_validos
)


# Agora faz a junção e preenche
dadosBR_bioma_mes <- todos_mes_bioma %>%
  left_join(dadosBR_bioma_mes, by = c("Mes", "Bioma")) %>%
  replace_na(list(QtdFocos = 0))

dadosBR_bioma_mes <- dadosBR_bioma_mes %>%
  mutate(Mes = as.Date(Mes))

ggplot(dadosBR_bioma_mes, aes(x = Mes, y = QtdFocos)) +
  geom_line(color = "firebrick", linewidth = 0.8) +
  facet_wrap(~ Bioma) +  # escala padrão (fixa para todos)
  labs(
    title = "Série Histórica de Focos de Calor por Bioma",
    subtitle = "Comparação mensal com escala padronizada",
    x = "Ano",
    y = "Quantidade de Focos"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

#####################################################
# Convertendo os dados para um objeto de classe ts
##################################################
focos_ts <- ts(
  data = dadosBR_mes$QtdFocos, # variavel que indica o número de focos
  start = c(2003, 1), # início da série - Janeiro de 2003
  frequency = 12 # frequência da série: mensal
)

focos_ts

class(focos_ts)


# Fazendo o gráfico da série temporal
plot(focos_ts,
     ylab = "Focos de calor", # nomeando o eixo y
     xlab = "Tempo") # nomeando o eixo x

# Autocorrelacao
acf(focos_ts)

# teste estatístico de Ljung-Box p autocorrelacao
Box.test(focos_ts, lag = 20, type = "Ljung-Box")

#
summary(focos_ts)

# Tendencia
plot(focos_ts, main = "Focos de incêndio - Brasil - 2003-2025")
abline(reg = lm(formula = focos_ts ~ time(focos_ts)),
       col = "red")

# # Vamos utilizar um pacote chamado Kendall para calcular a tendência
library(Kendall)
# Visualizando a tendência
plot(focos_ts, main = "Focos de incêndio - Brasil - 2003-2025")
lines(lowess(time(focos_ts), focos_ts),
      lwd = 3,
      col = "red")

# Criando um boxplot para verificar a sazonalidade
boxplot(focos_ts ~ cycle(focos_ts),
        xlab = "Mês",
        ylab = "Focos de Queimadas")

# Utilizando a função monthplot() para visualizar a sazonalidade
monthplot(focos_ts)

# Decomposição da série usando a função decompose()
plot(decompose(focos_ts))

# Decomposição da série usando a função stl()
# Ao lado direito, a barra de escala indica a importância para
# composição da série de cada termo (quanto menor, mais relevante)
plot(stl(focos_ts, s.window = 12))

######################
# Bioma Amazonia
#####################

dadosBR_amazonia <- dadosBR_bioma_mes |>
                    filter(Bioma == "Amazônia")

focos_amaz_ts <- ts(
  data = dadosBR_amazonia$QtdFocos, # variavel que indica o número de focos
  start = c(2003, 1), # início da série - Janeiro de 2003
  frequency = 12 # frequência da série: mensal
)

plot(focos_amaz_ts,
     ylab = "Focos de calor", # nomeando o eixo y
     xlab = "Tempo") # nomeando o eixo x

acf(focos_amaz_ts)

Box.test(focos_amaz_ts, lag = 20, type = "Ljung-Box")

plot(focos_amaz_ts, main = "Focos de incêndio - Amazônia - 2003-2025")
abline(reg = lm(formula = focos_amaz_ts ~ time(focos_amaz_ts)),
       col = "red")

boxplot(focos_amaz_ts ~ cycle(focos_amaz_ts),
        xlab = "Mês",
        ylab = "Focos de Queimadas")

monthplot(focos_amz)

plot(decompose(focos_amaz_ts))
plot(stl(focos_amaz_ts, s.window = 12))

######################
# Bioma Pampas
#####################

dadosBR_pampa <- dadosBR_bioma_mes |>
  filter(Bioma == "Pampa")

focos_pampa_ts <- ts(
  data = dadosBR_pampa$QtdFocos, # variavel que indica o número de focos
  start = c(2003, 1), # início da série - Janeiro de 2003
  frequency = 12 # frequência da série: mensal
)

plot(focos_pampa_ts,
     ylab = "Focos de calor", # nomeando o eixo y
     xlab = "Tempo") # nomeando o eixo x

acf(focos_pampa_ts)

Box.test(focos_pampa_ts, lag = 20, type = "Ljung-Box")

plot(focos_pampa_ts, main = "Focos de incêndio - Pampa - 2003-2025")
abline(reg = lm(formula = focos_pampa_ts ~ time(focos_pampa_ts)),
       col = "red")

boxplot(focos_pampa_ts ~ cycle(focos_pampa_ts),
        xlab = "Mês",
        ylab = "Focos de Queimadas")

monthplot(focos_pampa_ts)

plot(decompose(focos_pampa_ts))
plot(stl(focos_pampa_ts, s.window = 12))


######################
# Bioma Caatinga 
#####################
dadosBR_caatinga <- dadosBR_bioma_mes |>
  filter(Bioma == "Caatinga")


focos_caatinga_ts <- ts(
  data = dadosBR_caatinga$QtdFocos, # variavel que indica o número de focos
  start = c(2003, 1), # início da série - Janeiro de 2003
  frequency = 12 # frequência da série: mensal
)

plot(focos_caatinga_ts, 
     ylab = "Focos de calor", # nomeando o eixo y
     xlab = "Tempo") # nomeando o eixo x 

acf(focos_caatinga_ts)

Box.test(focos_caatinga_ts, lag = 20, type = "Ljung-Box")

plot(focos_caatinga_ts, main = "Focos de incêndio - Caatinga - 2003-2025")
abline(reg = lm(formula = focos_caatinga_ts ~ time(focos_ts)),
       col = "red")


boxplot(focos_caatinga_ts ~ cycle(focos_caatinga_ts),
        xlab = "Mês",
        ylab = "Focos de Queimadas")

monthplot(focos_caatinga_ts)

plot(decompose(focos_caatinga_ts))
plot(stl(focos_caatinga_ts, s.window = 12))

######################
# Bioma Cerrado
#####################

dadosBR_cerrado <- dadosBR_bioma_mes |>
  filter(Bioma == "Cerrado")

focos_cerrado_ts <- ts(
  data = dadosBR_cerrado$QtdFocos, # variavel que indica o número de focos
  start = c(2003, 1), # início da série - Janeiro de 2003
  frequency = 12 # frequência da série: mensal
)

plot(focos_cerrado_ts, 
     ylab = "Focos de calor", # nomeando o eixo y
     xlab = "Tempo") # nomeando o eixo x 

acf(focos_cerrado_ts)
Box.test(focos_cerrado_ts, lag = 20, type = "Ljung-Box")

plot(focos_cerrado_ts, main = "Focos de incêndio - Cerrado - 2003-2025")

abline(reg = lm(formula = focos_cerrado_ts ~ time(focos_cerrado_ts)),
       col = "purple")

boxplot(focos_cerrado_ts ~ cycle(focos_cerrado_ts),
        xlab = "Mês",
        ylab = "Focos de Queimadas")

monthplot(focos_cerrado_ts)

plot(decompose(focos_cerrado_ts))
plot(stl(focos_caatinga_ts, s.window = 12))

######################
# Bioma Mata Atlântica
#####################

dadosBR_mata_atlantica <- dadosBR_bioma_mes |>
  filter(Bioma == "Mata Atlântica")

focos_Mata_Atlãntica_ts <- ts(
  data = dadosBR_mata_atlantica$QtdFocos, # variavel que indica o número de focos
  start = c(2003, 1), # início da série - Janeiro de 2003
  frequency = 12 # frequência da série: mensal
)

plot(focos_Mata_Atlãntica_ts, 
     ylab = "Focos de calor", # nomeando o eixo y
     xlab = "Tempo") # nomeando o eixo x 

acf(focos_Mata_Atlãntica_ts)
Box.test(focos_Mata_Atlãntica_ts, lag = 20, type = "Ljung-Box")

plot(focos_Mata_Atlãntica_ts, main = "Focos de incêndio - Mata Atlântica - 2003-2025")

abline(reg = lm(formula = focos_Mata_Atlãntica_ts ~ time(focos_Mata_Atlãntica_ts)),
       col = "purple")

boxplot(focos_Mata_Atlãntica_ts ~ cycle(focos_Mata_Atlãntica_ts),
        xlab = "Mês",
        ylab = "Focos de Queimadas")

monthplot(focos_Mata_Atlãntica_ts)

plot(decompose(focos_Mata_Atlãntica_ts))
plot(stl(focos_Mata_Atlãntica_ts, s.window = 12))

##############################################3

str(dadosBR_bioma_mes)

dadosBR_bioma_mes <- dadosBR_bioma_mes |>
                    mutate(Bioma = as.factor(Bioma))

table(dadosBR_bioma_mes$Bioma)


boxplot(dadosBR_bioma_mes$QtdFocos ~dadosBR_bioma_mes$Bioma)

anova <- aov(dadosBR_bioma_mes$QtdFocos ~dadosBR_bioma_mes$Bioma)
summary(anova)

TukeyHSD(anova)

hist(log(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Amazônia"]))

anova2 <- kruskal.test(dadosBR_bioma_mes$QtdFocos ~dadosBR_bioma_mes$Bioma)
anova2

summary(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Amazônia"])
summary(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Cerrado"])
summary(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Mata Atlântica"])
summary(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Caatinga"])
summary(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Pampa"])
summary(dadosBR_bioma_mes$QtdFocos[dadosBR_bioma_mes$Bioma=="Pantanal"])


##################################################################################3


