################### PROJEÇÃO para IGP-M #######################################
###############################################################################
###############################################################################

####################### carregando pacotes e base de dados ####################
###############################################################################

### ajustar diretório de trabalho
setwd("C:/Users/lanza/OneDrive/Desktop/Trabalhos/Trabalhos_avulsos/Series_temporais/Revisao_portfolio/Arquivos_finais_portfolio")


### instalando e carregando os pacotes necessários
# install.packages("readxl")
# install.packages("stats")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("lmtest")

library("readxl")
library("stats")
library("forecast")
library("tseries")
library("lmtest")
library('xts')

### importando os dados em xls

igpm_202305SerieHist = read_excel("C:/Users/lanza/OneDrive/Desktop/Trabalhos/Trabalhos_avulsos/Series_temporais/Series/igpm_202305SerieHist.xlsx", 
                                   sheet = "Série IGP-M")
View(igpm_202305SerieHist)

### selecionando as colunas importantes
serieIGPM = ts(igpm_202305SerieHist$`Variação (%)`, start = c(2017, 6), frequency = 12)
serieIGPM.xts = as.xts(serieIGPM) #para usar os gráticos 'xts'


#################### análises descritivas #DataExploration ####################
###############################################################################

### gráfico da série original  
plot(
  serieIGPM.xts,
  main="",
  xlab="Meses", 
  ylab="Valores (em %)" 
)
  #média e variância não parecem constante ao longo do período; não parece apresentar sazonalidade

### Estatísticas Descritivas da Série Original
summary(serieIGPM)
sd(serieIGPM)

### função de autocorrelação (acf) da Série Original
plot(acf(serieIGPM),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" #quatro 'lags' significantes
)

### função de autocorrelação parcial (pacf) da Série Original
plot(pacf(serieIGPM, lag=length(serieIGPM), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
)#Primeiro 'lag' significante



### checando a estacionariedade   #StationarityCheck
adf.test(serieIGPM, k=4)
  #não é estacionária

adf.test(serieIGPM, k=1)
  #k são os lags significantes nas acf e pacf
  #não é estacionária


### transformações na série original para tentar alcançar estacionariedade

dif1_igpm = diff(serieIGPM, lag = 1, differences = 1) #obtendo a primeira diferença, em pontos percentuais (p. p.)

dif1_igpm = c(0.26, dif1_igpm) ## 0,26 é a diferença em p. p. para a var. do igpm entre jun/2017 e mai/2017
dif1_igpm = ts(dif1_igpm, start = c(2017, 6), frequency = 12)
dif1_igpm.xts = as.xts(dif1_igpm)
  #corrigindo o tamanho do vetor e transformando-o em ts

# criando uma lista p guardar as séries original e transformada
#series = list(original = serieIGPM, dif1_igpm = dif1_igpm) 

#### análise descritiva da série transformada (primeira diferença)

plot(
  dif1_igpm.xts, 
  main="",
  xlab="Meses", 
  ylab="Valores (em p.p)" 
)
  #a média parece mais comportada, mas a variância não parece constante



## Estatísticas Descritivas da série transformada
summary(dif1_igpm)
sd(dif1_igpm)/mean(dif1_igpm)
sd(serieIGPM)/mean(serieIGPM)
  # sd diminuiu de 1.200772 pra 0.9711645

### função de autocorrelação (acf) da série transformada
plot(
  acf(dif1_igpm), 
  xlab = "Escala relacionada à periodicidade da série (1 unidade = 12 meses)",
  ylab = "ACF",
  main = "" 
)
#'lags' de n° 1 e 12 tiveram significância -> indica um SARIMA(0, 1, 1)(0, 0, 1)12?

### função de autocorrelação parcial (pacf) da série transformada
plot(
  pacf(dif1_igpm, lag=length(serieIGPM), plot = FALSE),
  xlab = "Escala relacionada à periodicidade da série (1 unidade = 12 meses)",
  ylab = "PACF",
  main = "" 
)

#'lags' de n° 1 e 12 tiveram significância -> indica um SARIMA(1, 1, 0)(1, 0, 0)12?
  ##referenciando o exemplo mostrado em https://otexts.com/fpp2/seasonal-arima.html, 
    #testar-se-á primeiramente somente um dos modelos SARIMA apontados acima

### checando a estacionariedade  #StationarityCheck
adf.test(dif1_igpm, k = 4)
adf.test(dif1_igpm, k = 1)
# com 'lag order = 1', considera-se a hipótese de estacionariedade



###################### escolhendo modelos ##################################
################################################################################


### série transformada
# começando com SARIMA(0, 1, 1) (0, 0, 1)12 (em razão da acf)

par(mar = c(3, 3, 2, 1))  # Ajustar as margens das imagens

library(stats)

# Ajustar o modelo SARIMA (0, 1, 1) (0, 0, 1) [12] 
SARIMA_igpm_1 <- Arima(
  dif1_igpm, 
  order=c(0, 1, 1), 
  seasonal=list(order=c(0, 0, 1), period=12)
)

summary(SARIMA_igpm_1)
summary(SARIMA_igpm_1$residuals)
sd(SARIMA_igpm_1$residuals)

# Resíduos do modelo SARIMA (0, 1, 1)(0, 0, 1) [12]

plot(
  as.xts(SARIMA_igpm_1$residuals), 
  main="",
  xlab="Meses", 
  ylab="Valores (em p.p)" 
)


## ACF e PACF do primeiro modelo ajustado

par(mar = c(5, 5, 3, 2))  # Ajustar as margens das imagens

plot(SARIMA_igpm_1$residuals,
     xlab = "Anos",
     ylab = "Resíduos",
     main = "" 
)

grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")


plot(acf(SARIMA_igpm_1$residuals),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" 
) # correlação para n-1

plot(pacf(SARIMA_igpm_1$residuals, lag=length(serieIGPM), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
)

## # correlação para n-1, n-5 e n-7


# Ajustar o modelo SARIMA (1, 1, 1) (0, 0, 1) [12] 
SARIMA_igpm_2 <- Arima(
  dif1_igpm, 
  order=c(1, 1, 1), 
  seasonal=list(order=c(0, 0, 1), period=12)
)

summary(SARIMA_igpm_2)
summary(SARIMA_igpm_2$residuals)
sd(SARIMA_igpm_2$residuals)

# Ajustar o modelo SARIMA (5, 1, 1) (0, 0, 1) [12] 
SARIMA_igpm_3 <- Arima(
  dif1_igpm, 
  order=c(5, 1, 1), 
  seasonal=list(order=c(0, 0, 1), period=12)
)

summary(SARIMA_igpm_3)
summary(SARIMA_igpm_3$residuals)
sd(SARIMA_igpm_3$residuals)

# Ajustar o modelo SARIMA (7, 1, 1) (0, 0, 1) [12] 
SARIMA_igpm_4 <- Arima(
  dif1_igpm, 
  order=c(7, 1, 1), 
  seasonal=list(order=c(0, 0, 1), period=12)
)

summary(SARIMA_igpm_4)
summary(SARIMA_igpm_4$residuals)
sd(SARIMA_igpm_4$residuals)


# melhor foi o segundo modelo, cujos resíduos serão plotados

par(mar = c(5, 5, 3, 2))  # Ajustar as margens das imagens

plot(SARIMA_igpm_2$residuals,
     xlab = "Anos",
     ylab = "Resíduos",
     main = "" 
)

grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")


plot(acf(SARIMA_igpm_2$residuals),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" 
) # sem autocorrelação significante

plot(pacf(SARIMA_igpm_2$residuals, lag=length(serieIGPM), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
) # autocorrelação significante no sétimo 'lag' 


## plotando os modelo SARIMA (7, 1, 1) (0, 0, 1) [12]


par(mar = c(5, 5, 3, 2))  # Ajustar as margens das imagens

plot(SARIMA_igpm_4$residuals,
     xlab = "Anos",
     ylab = "Resíduos",
     main = "" 
)

grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray")

plot(acf(SARIMA_igpm_4$residuals),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" 
) # sem autocorrelação significante 

plot(pacf(SARIMA_igpm_4$residuals, lag=length(serieIGPM), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
) # # sem autocorrelação significante

# checkresiduals

checkresiduals(SARIMA_igpm_2)
checkresiduals(SARIMA_igpm_4)

# resíduos x valores ajustados
plot.default(
  x = SARIMA_igpm_4$fitted, 
  y = SARIMA_igpm_4$residuals, 
  pch = 20, 
  xlab = "Valores ajustados", 
  ylab = "Resíduos", 
  main = ""
)
grid(
  nx = NULL, 
  ny = NULL, 
  lty = "dotted", 
  col = "gray"
)


## análise gráfica dos resíduos dos modelos 2 e 4

plot(
  as.xts(SARIMA_igpm_4$residuals), 
  main="",
  xlab="Meses", ylab="Valores (em p.p)" 
)

lines(
  as.xts(SARIMA_igpm_2$residuals), 
  col = "red"
)

# comparando as somas dos quadrados dos resíduos (RSS)

sum(SARIMA_igpm_4$residuals^2)
sum(SARIMA_igpm_2$residuals^2)

sum(SARIMA_igpm_4$residuals^2) > sum(SARIMA_igpm_2$residuals^2)
  #modelo 4 tem menor RSS

## modelo 4 escolhido 


############### PROJETANDO COM O MODELO ESCOLHIDO ##################
####################################################################

projecao_2309 = forecast(SARIMA_igpm_4, h = 4, level = 95) # até 09/2023

# OBSERVAÇÃO: os valores projetados são as diferenças de primeira ordem, 
    # então deve-se voltar à série original

# criando uma lista para guardar a projeção e seus intervalos
resultados_IGPM = list(
  projecao_media = c(dif1_igpm, projecao_2309$mean),
  projecao_superior = c(dif1_igpm, projecao_2309$upper),
  projecao_inferior = c(dif1_igpm, projecao_2309$lower)
)

# loop só para os valores projetados
for (i in seq(
  from = length(resultados_IGPM$projecao_media), 
  to = length(resultados_IGPM$projecao_media) - 2, 
  by = -1)
) 
{ # o valor original para jun/2017 será computado depois
  resultados_IGPM$projecao_media[i] = resultados_IGPM$projecao_media[i] + resultados_IGPM$projecao_media[i-1]
  resultados_IGPM$projecao_superior[i] = resultados_IGPM$projecao_superior[i] + resultados_IGPM$projecao_media[i-1]
  resultados_IGPM$projecao_inferior[i] = resultados_IGPM$projecao_inferior[i] + resultados_IGPM$projecao_media[i-1]
}

# loop só para os valores da série transformada
for (i in seq(from = length(resultados_IGPM$projecao_media) - 3, to = 2, by = -1)) { # o valor original para jun/2017 será computado depois
  resultados_IGPM$projecao_media[i] = resultados_IGPM$projecao_media[i] + serieIGPM[i-1]
  resultados_IGPM$projecao_superior[i] = resultados_IGPM$projecao_superior[i] + serieIGPM[i-1]
  resultados_IGPM$projecao_inferior[i] = resultados_IGPM$projecao_inferior[i] + serieIGPM[i-1]
}

# restauração "manual" do valor da série original para jun/2017

resultados_IGPM$projecao_media[1] = resultados_IGPM$projecao_media[1] + (-0.93)
resultados_IGPM$projecao_superior[1] = resultados_IGPM$projecao_superior[1] + (-0.93)
resultados_IGPM$projecao_inferior[1] = resultados_IGPM$projecao_inferior[1] + (-0.93)

################################# encontrando os valores acumulados em doze meses

#transformando cada valor em fator 
resultados_IGPM$projecao_media = (resultados_IGPM$projecao_media/100 + 1) 
      #divide-se por 100 em razão dos valores representarem percentuais
resultados_IGPM$projecao_superior = (resultados_IGPM$projecao_superior/100 + 1)
resultados_IGPM$projecao_inferior = (resultados_IGPM$projecao_inferior/100 + 1)


for (i in seq(from = length(resultados_IGPM$projecao_media), to =  12, by = -1)) {
  resultados_IGPM$projecao_media[i] = cumprod(resultados_IGPM$projecao_media[i:(i-11)])[12] #alocar ao longo do vetor serieIGPM_proj 
      #o último valor obtido do produto dos fatores (cujo nº de multiplicandos = 12)
  
  resultados_IGPM$projecao_superior[i] = cumprod(resultados_IGPM$projecao_superior[i:(i-11)])[12]
  resultados_IGPM$projecao_inferior[i] = cumprod(resultados_IGPM$projecao_inferior[i:(i-11)])[12]
} 

for (i in seq(1, 11, by = 1)) {
  resultados_IGPM$projecao_media[i] = NA
  resultados_IGPM$projecao_superior[i] = NA
  resultados_IGPM$projecao_inferior[i] = NA
}                              

# subtraindo um
resultados_IGPM$projecao_media = (resultados_IGPM$projecao_media - 1)*100
resultados_IGPM$projecao_superior = (resultados_IGPM$projecao_superior - 1)*100
resultados_IGPM$projecao_inferior = (resultados_IGPM$projecao_inferior - 1)*100

#  transformando em série temporal 
resultados_IGPM$projecao_media = ts(resultados_IGPM$projecao_media, start = c(2017, 6), frequency = 12)
resultados_IGPM$projecao_superior = ts(resultados_IGPM$projecao_superior, start = c(2017, 6), frequency = 12)
resultados_IGPM$projecao_inferior = ts(resultados_IGPM$projecao_inferior, start = c(2017, 6), frequency = 12)


### gráfico até 09/2023

par(mar = c(4, 4, 2, 1))

plot(resultados_IGPM$projecao_media, 
     main = "", 
     xlab = "Years",
     ylab = "IGP-M 12-month variation (%)",
     xlim = c(2022, 2023.7), 
     ylim = c(-15, 20)
)

# Add the forecasted values to the plot
lines(resultados_IGPM$projecao_media, col = "blue", lty = 2)  

# Adicionando intervalos de predição de 95% de confiança
lines(resultados_IGPM$projecao_superior, col = "red", lty = 2)   
lines(resultados_IGPM$projecao_inferior, col = "red", lty = 2)   

grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')


resultados_IGPM$projecao_media[73:76]
resultados_IGPM$projecao_superior[73:76]
resultados_IGPM$projecao_inferior[73:76]



##### fim #######
