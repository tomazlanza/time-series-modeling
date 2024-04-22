################### PROJEÇÃO para IPCA #######################################
###############################################################################
###############################################################################

####################### carregando pacotes e base de dados ####################
###############################################################################

### ajustar diretório de trabalho
setwd("C:/Users/lanza/OneDrive/Desktop/Trabalhos/Trabalhos_avulsos/Series_temporais/Revisao_portfolio/Arquivos_finais_portfolio")

### instalando e carregando os pacotes necessários
#install.packages("readxl")
#install.packages("stats")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("lmtest")

library("readxl")
library("stats")
library("forecast")
library("tseries")
library("lmtest")
library('xts')

### importando os dados em xls
library(readxl)
ipca_202305SerieHist <- read_excel(
  "C:/Users/lanza/OneDrive/Desktop/Trabalhos/Trabalhos_avulsos/Series_temporais/Series/ipca_202305SerieHist.xls", 
  sheet = "Série IPCA", 
  range = "B2:C75"
)
View(ipca_202305SerieHist)

ipca_202305SerieHist = ipca_202305SerieHist[-73,] #removendo uma linha de NAs

### selecionando as colunas importantes
serieIPCA = ts(
  ipca_202305SerieHist$`Variação mensal (%)`, 
  start = c(2017, 6), 
  frequency = 12
)

serieIPCA.xts = as.xts(serieIPCA) #para usar os gráficos 'xts'


#################### análises descritivas #DataExploration ####################
###############################################################################

### gráfico da série original  
plot(
  serieIPCA.xts,
  main="",
  xlab="Meses", 
  ylab="Variação mensal (em %)" 
)
#média e variância não parecem constante ao longo do período; não parece apresentar sazonalidade

### Estatísticas Descritivas da Série Original
summary(serieIPCA)
sd(serieIPCA)

### função de autocorrelação (acf) da Série Original
plot(acf(serieIPCA),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" #dois 'lags' significantes
)

### função de autocorrelação parcial (pacf) da Série Original
plot(pacf(serieIPCA, lag=length(serieIPCA), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
)#nenhum 'lag' significante

### checando a estacionariedade   #StationarityCheck
adf.test(serieIPCA, k=4)
#não é estacionária

adf.test(serieIPCA, k=2)
#k são os lags significantes nas acf e pacf
#é estacionária


###################### escolhendo modelos para a série ####################
################################################################################

### série original
# MA(1)

par(mar = c(3, 3, 2, 1))  # Ajustar as margens das imagens

MA_IPCA_1 = Arima(
  serieIPCA, 
  order=c(0, 0, 1), 
)

MA_IPCA_1$coef #precisa-se olhar e comentar os coeficientes

summary(MA_IPCA_1)
summary(MA_IPCA_1$residuals)
sd(MA_IPCA_1$residuals)

# Resíduos de MA(1)
plot(
  x = as.xts(MA_IPCA_1$residuals), 
  main = "",
  xlab = "Meses",
  ylab = "Valores (em p.p)"
)

# gráficos ACF e PACF para MA(1)

par(mar = c(5, 5, 3, 2))  # Ajustar as margens das imagens

plot(acf(MA_IPCA_1$residuals),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" 
) # correlação para n-2  

plot(pacf(MA_IPCA_1$residuals, lag=length(serieIPCA), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
) # correlação parcial para n-2 

# resíduos x valores ajustados
plot.default(
  x = MA_IPCA_1$fitted, 
  y = MA_IPCA_1$residuals, 
  pch = 20, 
  xlab = "Valores ajustados", 
  ylab = "Resíduos", 
  main = "Resíduos X Valores ajustados"
)
grid(
  nx = NULL, 
  ny = NULL, 
  lty = "dotted", 
  col = "gray"
)

checkresiduals(MA_IPCA_1)



# MA(2) (em razão da acf da série do IPCA original e dos ACF e PACF dos resíduos do MA(1))

par(mar = c(3, 3, 2, 1))  # Ajustar as margens das imagens

MA_IPCA_2 = Arima(
  serieIPCA, 
  order=c(0, 0, 2), 
)

MA_IPCA_2$coef #precisa-se olhar e comentar os coeficientes

summary(MA_IPCA_2)
summary(MA_IPCA_2$residuals)
sd(MA_IPCA_2$residuals)

# Resíduos de MA(2)
plot(
  x = as.xts(MA_IPCA_2$residuals), 
  main = "",
  xlab = "Meses",
  ylab = "Valores (em p.p)"
)

# gráficos ACF e PACF para MA(2)

plot(acf(MA_IPCA_2$residuals),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "ACF",
     main = "" 
) # sem autocorrelação 

plot(pacf(MA_IPCA_2$residuals, lag=length(serieIPCA), plot = FALSE),
     xlab = "Escala relacionada à periodicidade da série (1 = 12 meses)",
     ylab = "PACF",
     main = ""
) # sem autocorrelação parcial


# resíduos x valores ajustados

par(mar = c(5, 5, 3, 2))

plot.default(
  x = MA_IPCA_2$fitted, 
  y = MA_IPCA_2$residuals, 
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

checkresiduals(MA_IPCA_2)


#########################################################################

## análise gráfica dos resíduos

plot(
  as.xts(MA_IPCA_2$residuals), 
  main="",
  xlab="Meses", ylab="Valores (em p.p)" 
)

lines(
  as.xts(MA_IPCA_1$residuals), 
  col = "red"
)

# comparando as somas dos quadrados dos resíduos (RSS)

sum(MA_IPCA_2$residuals^2) > sum(MA_IPCA_1$residuals^2)
sum(MA_IPCA_2$residuals^2)
sum(MA_IPCA_1$residuals^2)

#modelo 4 tem menor RSS

AIC(MA_IPCA_1) > AIC(MA_IPCA_2)
BIC(MA_IPCA_1) > BIC(MA_IPCA_2)

## modelo 2 escolhido 

MA_IPCA_2$residuals


############### PROJETANDO IPCA COM O MODELO ESCOLHIDO ##################
####################################################################

projecao_IPCA_2309 = forecast(MA_IPCA_2, h = 4, level = 95) # até 09/2023

# criando uma lista para guardar a projeção e seus intervalos
resultados_IPCA = list(
  projecao_media_ipca = c(serieIPCA, projecao_IPCA_2309$mean),
  projecao_superior_ipca = c(serieIPCA, projecao_IPCA_2309$upper),
  projecao_inferior_ipca = c(serieIPCA, projecao_IPCA_2309$lower)
)

#  transformando em série temporal 
resultados_IPCA$projecao_media_ipca = ts(resultados_IPCA$projecao_media_ipca, start = c(2017, 6), frequency = 12)
resultados_IPCA$projecao_superior_ipca = ts(resultados_IPCA$projecao_superior_ipca, start = c(2017, 6), frequency = 12)
resultados_IPCA$projecao_inferior_ipca = ts(resultados_IPCA$projecao_inferior_ipca, start = c(2017, 6), frequency = 12)


# algo deu errado daqui pra baixo

################################# encontrando os valores acumulados em doze meses

#transformando cada valor em fator 
resultados_IPCA$projecao_media_ipca = (resultados_IPCA$projecao_media_ipca/100 + 1) 
#divide-se por 100 em razão dos valores representarem percentuais
resultados_IPCA$projecao_superior_ipca = (resultados_IPCA$projecao_superior_ipca/100 + 1)
resultados_IPCA$projecao_inferior_ipca = (resultados_IPCA$projecao_inferior_ipca/100 + 1)


for (i in seq(from = length(resultados_IPCA$projecao_media_ipca), to =  12, by = -1)) {
  resultados_IPCA$projecao_media_ipca[i] = cumprod(resultados_IPCA$projecao_media_ipca[i:(i-11)])[12] #alocar ao longo do vetor serieIGPM_proj 
  #o último valor obtido do produto dos fatores (cujo nº de multiplicandos = 12)
  
  resultados_IPCA$projecao_superior_ipca[i] = cumprod(resultados_IPCA$projecao_superior_ipca[i:(i-11)])[12]
  resultados_IPCA$projecao_inferior_ipca[i] = cumprod(resultados_IPCA$projecao_inferior_ipca[i:(i-11)])[12]
} 

for (i in seq(1, 11, by = 1)) {
  resultados_IPCA$projecao_media_ipca[i] = NA
  resultados_IPCA$projecao_superior_ipca[i] = NA
  resultados_IPCA$projecao_inferior_ipca[i] = NA
}                              

# subtraindo um
resultados_IPCA$projecao_media_ipca = (resultados_IPCA$projecao_media_ipca - 1)*100
resultados_IPCA$projecao_superior_ipca = (resultados_IPCA$projecao_superior_ipca - 1)*100
resultados_IPCA$projecao_inferior_ipca = (resultados_IPCA$projecao_inferior_ipca - 1)*100

#  transformando em série temporal 
resultados_IPCA$projecao_media_ipca = ts(resultados_IPCA$projecao_media_ipca, start = c(2017, 6), frequency = 12)
resultados_IPCA$projecao_superior_ipca = ts(resultados_IPCA$projecao_superior_ipca, start = c(2017, 6), frequency = 12)
resultados_IPCA$projecao_inferior_ipca = ts(resultados_IPCA$projecao_inferior_ipca, start = c(2017, 6), frequency = 12)



### gráfico até 09/2023

par(mar = c(4, 4, 2, 1))

plot(resultados_IPCA$projecao_media_ipca, 
     main = "", 
     xlab = "Years",
     ylab = "IPCA 12-month variation (%)",
     xlim = c(2022, 2023.7), 
     ylim = c(0, 15)
)

# Add the forecasted values to the plot
lines(resultados_IPCA$projecao_media_ipca, col = "blue", lty = 2)  

# Adicionando intervalos de predição de 95% de confiança
lines(resultados_IPCA$projecao_superior_ipca, col = "red", lty = 2)   
lines(resultados_IPCA$projecao_inferior_ipca, col = "red", lty = 2)   

grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')

resultados_IPCA$projecao_media_ipca[73:76]
resultados_IPCA$projecao_superior_ipca[73:76]
resultados_IPCA$projecao_inferior_ipca[73:76]

############## fim #################

