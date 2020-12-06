rm(list=ls())
#Bibliotecas necessarias
pacman::p_load(pacman, dplyr, tidyr, tidyverse, haven, lubridate, 
               psych, skimr, corrplot, ggplot2, e1071)

#Lendo arquivos em formato SAS
covid19 <- read_sas("~/Dados/Covid-19/covid19.sas7bdat", NULL)
covid_serie <- read_sas("~/Dados/Covid-19/covid19_serie.sas7bdat", NULL)

#Filtrando os arquivos para: Brasil e DF
covid_DF <- covid_serie[covid_serie$estado == "DF" & !is.na(covid_serie$estado),]
covid_Br <- covid19[covid19$regiao == "Brasil" & !is.na(covid19$estado),]


#Arrumando Erro de notacao cientifica nos graficos
options("scipen"=100, "digits"=4)


########################## MES ############################
###################### BRASIL ##########################

#Analise Exploratoria rapida Brasil
skim(covid_Br)
summary(covid_Br)

#(Conferindo se o filtro para o Brasil possui as mesmas analises caso usasse a base toda)
skim(covid19)
skim(covid_serie)

########################### DISTRITO FEDERAL ##############################
#Analise Exploratoria rapida Distrito Federal
skim(covid_DF)
summary(covid_DF)

########################### BRASIL ##############################
summary(covid_Br$casosNovos)
covid_Br$data[covid_Br$casosNovos == 69074]
covid_Br$data[covid_Br$obitosNovos == 1595]
View(covid_Br[,c(1,2,3,4,5,6,7,9,17)])


#Conjunto de graficos Brasil
#Histograma BRASIL CASOS
hist(covid_Br$casosNovos, col="#777799", 
     main = "Histograma casos novos", xlab = "Qtd. casos novos",
     ylab = "Frequência de casos novos")

#indice de assimetria e curtose
skewness(covid_Br$casosNovos)
kurtosis(covid_Br$casosNovos)


#LinePlot BRASIL CASOS 
par(mfrow = c(1, 1))
plot(covid_Br$data, covid_Br$casosNovos, 
     main = "Evolução de casos novos de COVID-19 no Brasil por mês",
     ylab = "Qtd. casos novos", xlab = "Período", 
     type = "l", col = "blue", panel.first = grid())

plot(covid_Br$data, covid_Br$casosAcumulado, 
     main = "Evolução de casos acumulados de COVID-19 no Brasil por mês",
     ylab = "Qtd. casos acumulados", xlab = "Período", 
     type = "l", col = "red", panel.first = grid())


#Linha tendencia suave BRASIL CASOS 
plot(covid_Br$casosNovos ~ covid_Br$data,
     data = covid_Br,
     xlab = "Período",
     ylab = "Qtd. casos novos",
     main = "Evolução de casos novos de COVID-19 no Brasil por mês")
with(covid_Br, {
  lines(lowess(x = covid_Br$data, y = covid_Br$casosNovos),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})

plot(covid_Br$casosAcumulado ~ covid_Br$data,
     data = covid_Br,
     xlab = "Período",
     ylab = "Qtd. casos acumulados",
     main = "Evolução de casos acumulados de COVID-19 no Brasil por mês")
with(covid_Br, {
  lines(lowess(x = covid_Br$data, y = covid_Br$casosAcumulado),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})
par(mfrow = c(1, 1))

#LinePlot BRASIL OBITOS 
skewness(covid_Br$obitosNovos)
kurtosis(covid_Br$obitosNovos)

hist(covid_Br$obitosNovos, col = "#ff0050", ylab = "Frequência de obitos novos", 
     xlab = "Número de óbitos novos", main = "Histograma obitos novos")


plot(covid_Br$data, covid_Br$obitosNovos, 
     main = "Evolução de óbitos novos de COVID-19 no Brasil por mês",
     ylab = "Qtd. óbitos novos", xlab = "Período", 
     type = "l", col = "blue", panel.first = grid())

plot(covid_Br$data, covid_Br$obitosAcumulado, 
     main = "Evolução de óbitos acumulados de COVID-19 no Brasil por mês",
     ylab = "Qtd. óbitos acumulados", xlab = "Período", 
     type = "l", col = "red", panel.first = grid())
par(mfrow=c(1, 1))

#Linha tendencia suave BRASIL OBITOS 
plot(covid_Br$obitosNovos ~ covid_Br$data,
     data = covid_Br,
     xlab = "Período",
     ylab = "Qtd. óbitos novos",
     main = "Evolução de óbitos novos de COVID-19 no Brasil por mês")
with(covid_Br, {
  lines(lowess(x = covid_Br$data, y = covid_Br$obitosNovos),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})

plot(covid_Br$obitosAcumulado ~ covid_Br$data,
     data = covid_Br,
     xlab = "Período",
     ylab = "Qtd. óbitos acumulados",
     main = "Evolução de óbitos acumulados de COVID-19 no Brasil por mês")
with(covid_Br, {
  lines(lowess(x = covid_Br$data, y = covid_Br$obitosAcumulado),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})
par(mfrow = c(1, 1))

#Em acompanhamento BR
skim(covid_Br$emAcompanhamentoNovos)
summary(covid_Br$emAcompanhamentoNovos)
plot(covid_Br$emAcompanhamentoNovos ~ covid_Br$data, 
     main = "Evolução de pacientes em acompanhamentono Brasil por mês", 
     ylab = "Qtd. em acompanhamento", xlab = "Período",
     type = "l", col = "blue", panel.first = grid())

#Recuperado BR
skim(covid_Br$Recuperadosnovos)
plot(covid_Br$Recuperadosnovos ~ covid_Br$data, 
     main = "Evolução de pacientes recuperados Brasil por mês", 
     ylab = "Qtd. recuperados", xlab = "Período",
     type = "l", col = "blue", panel.first = grid())


########################### DF ##############################
#Conjunto de graficos DF
View(covid_DF[,c(1,2,3,4,5,6,7,9, 16)])
summary(covid_DF$obitosNovos)
summary(covid_DF$casosNovos)
covid_DF$data[covid_DF$casosNovos == 3171]
covid_DF$data[covid_DF$obitosNovos == 79]

#Indice de assimetria e curtose
skewness(covid_DF$casosNovos)
kurtosis(covid_DF$casosNovos)

skewness(covid_DF$obitosNovos)
kurtosis(covid_DF$obitosNovos)

#Histogramas DF
hist(covid_DF$casosNovos, col = "#59a19f", 
     main = "Histograma casos novos", xlab = "Qtd. casos novos",
     ylab = "Frequência de casos novos")

hist(covid_DF$obitosNovos, col = "#e7a900", 
     main = "Histograma óbitos novos", xlab = "Qtd. óbitos novos",
     ylab = "Frequência de óbitos novos")

#LinePlot DF CASOS
plot(covid_DF$data, covid_DF$casosNovos, 
     main = "Evolução de casos novos de COVID-19 no DF por mês",
     ylab = "Qtd. casos novos", xlab = "Período", 
     type = "l", col = "blue", panel.first = grid())

plot(covid_DF$data, covid_DF$casosAcumulado, 
     main = "Evolucao de casos acumulados de COVID-19 no DF por mês",
     ylab = "Qtd. casos acumulados", xlab = "Período", 
     type = "l", col = "red", panel.first = grid())
par(mfrow=c(1, 1))

#Linha tendencia suave DF CASOS
plot(covid_DF$casosNovos ~ covid_DF$data,
     data = covid_DF,
     xlab = "Período",
     ylab = "Qtd. casos novos",
     main = "Evolução de casos novos de COVID-19 no DF por mês")
with(covid_DF, {
  lines(lowess(x = covid_DF$data, y = covid_DF$casosNovos),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})

plot(covid_DF$casosAcumulado ~ covid_DF$data,
     data = covid_DF,
     xlab = "Período",
     ylab = "Qtd. casos acumulados",
     main = "Evolução de casos acumulados de COVID-19 no DF por mês")
with(covid_DF, {
  lines(lowess(x = covid_DF$data, y = covid_DF$casosAcumulado),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})
par(mfrow = c(1, 1))


#LinePlot DF OBITOS
skewness(covid_DF$obitosNovos)
kurtosis(covid_DF$obitosNovos)

plot(covid_DF$data, covid_DF$obitosNovos, 
     main = "Evolução de óbitos novos de COVID-19 no DF por mês",
     ylab = "Qtd. óbitos novos", xlab = "Período", 
     type = "l", col = "blue", panel.first = grid())

plot(covid_DF$data, covid_DF$obitosAcumulado, 
     main = "Evolução de óbitos acumulados de COVID-19 no DF por mês",
     ylab = "Qtd. óbitos acumulados", xlab = "Período", 
     type = "l", col = "red", panel.first = grid())
par(mfrow=c(1, 1))

#Linha tendencia suave DF OBITOS 
par(mfrow = c(2, 1))
plot(covid_DF$obitosNovos ~ covid_DF$data,
     data = covid_DF,
     xlab = "Período",
     ylab = "Qtd. óbitos novos",
     main = "Evolução de óbitos novos de COVID-19 no DF por mês")
with(covid_DF, {
  lines(lowess(x = covid_DF$data, y = covid_DF$obitosNovos),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})

plot(covid_DF$obitosAcumulado ~ covid_DF$data,
     data = covid_DF,
     xlab = "Período",
     ylab = "Qtd. óbitos acumulados",
     main = "Evolução de óbitos acumulados de COVID-19 no DF por mês")
with(covid_DF, {
  lines(lowess(x = covid_DF$data, y = covid_DF$obitosAcumulado),
        lwd = 2,
        col = "#ff0050", panel.first = grid())})
par(mfrow = c(1, 1))



##########################################################
#Boxplot Casos novos meses Brasil
tapply(covid_Br$casosNovos, month(covid_Br$data), summary)
describeBy(covid_Br$casosNovos, month(covid_Br$data))

boxplot(covid_Br$casosNovos ~ month(covid_Br$data),
        main="Casos novos por mês no Brasil",
        xlab="Mês",
        ylab="Quantidade de casos novos",
        col="#777799",
)

#Boxplot Obitos novos meses Brasil
tapply(covid_Br$obitosNovos, month(covid_Br$data), summary)
describeBy(covid_Br$obitosNovos, month(covid_Br$data))

boxplot(covid_Br$obitosNovos ~ month(covid_Br$data),
        main="Óbitos novos por mês no Brasil",
        xlab="Mês",
        ylab="Quantidade de casos novos",
        col="#880055",
)

#Boxplot Casos novos meses DF
tapply(covid_DF$casosNovos, month(covid_DF$data), summary)
describeBy(covid_DF$casosNovos, month(covid_DF$data))

boxplot(covid_DF$casosNovos ~ month(covid_DF$data),
        main="Casos novos por mês no Distrito Federal",
        xlab="Mês",
        ylab="Quantidade de casos novos",
        col="#59a19f",
        border="#050627",
)

#Boxplot Obitos novos meses DF
tapply(covid_DF$obitosNovos, month(covid_DF$data), summary)
describeBy(covid_DF$obitosNovos, month(covid_DF$data))

boxplot(covid_DF$obitosNovos ~ month(covid_DF$data),
        main="óbitos novos por mês no Distrito Federal",
        xlab="Mês",
        ylab="Quantidade de casos novos",
        col="#e7a900",
        border="#050627",
)

########## Regressoes e correlacoes ##########
#Regressao BR
plot(covid_Br$casosNovos ~ covid_Br$obitosNovos, xlab = "Qtd. óbitos novos", 
     ylab = "Qtd. casos novos", main = "Correlação casos novos por óbitos novos")
abline(lm(covid_Br$casosNovos ~ covid_Br$obitosNovos), col = "red", lwd = 3)
text(paste("Correlação:", round(cor(covid_Br$casosNovos, covid_Br$obitosNovos), 2)), 
     x = 400, y = 60000)

#Regressao DF
plot(covid_DF$casosNovos ~ covid_DF$obitosNovos, xlab = "Qtd. óbitos novos", 
     ylab = "Qtd. casos novos", main = "Correlação casos novos por óbitos novos")
abline(lm(covid_DF$casosNovos ~ covid_DF$obitosNovos), col = "red", lwd = 3)
text(paste("Correlação:", round(cor(covid_DF$casosNovos, covid_DF$obitosNovos), 2)), 
     x = 10, y = 3000)

#Correlacao BR
coree <- covid_Br[,c(12,14)]
N<-cor(coree)
head(round(N,2))
png("correlacao_numeroBR.PNG")
corrplot(N, method= "number")
corrplot

#Correlacao DF
coree1 <- covid_DF[,c(11,13)]
M<-cor(coree1)
head(round(M,2))
png("correlacao_numeroDF.PNG")
corrplot(M, method= "number")
corrplot

############################REGIAO######################################
ggplot(covid_serie, aes(y = casosNovos, x = regiao, fill = regiao)) +
  geom_bar(stat = "identity", width = .75) +
  ggtitle("Qtd. casos novos por região") +
  labs(x = "Região", y = "Qtd. casos novos")

ggplot(covid_serie, aes(y = casosNovos, x = estado, fill = regiao)) +
  geom_bar(stat = "identity", width = .75) +
  ggtitle("Qtd. casos novos por estado") +
  labs(x = "Estado", y = "Qtd. casos novos")


ggplot(covid_serie, aes(y = obitosNovos, x = regiao, fill = regiao)) +
  geom_bar(stat = "identity", width = .75) +
  ggtitle("Qtd. óbitos novos por região") +
  labs(x = "Região", y = "Qtd. óbitos novos")

ggplot(covid_serie, aes(y = obitosNovos, x = estado, fill = regiao)) +
  geom_bar(stat = "identity", width = .75) +
  ggtitle("Qtd. óbitos novos por estado") +
  labs(x = "Estado", y = "Qtd. óbitos novos")



#######################################
#Distribuicao dados casos novos Brasil
den <- density(covid_Br$casosNovos)
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Qtd. casos novos",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "Distribuição dados casos novos no Brasil",
     sub = paste("Quantidade total:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(covid_Br$casosNovos)


##########Distribuicao dados obitos novos Brasil
den <- density(covid_Br$obitosNovos)
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Qtd. óbitos novos",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "Distribuição dados óbitos novos no Brasil",
     sub = paste("Quantidade total:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(covid_Br$obitosNovos)


#######################################
#Distribuicao dados casos novos DF
den <- density(covid_DF$casosNovos)
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Qtd. casos novos",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "Distribuição dados casos novos no DF",
     sub = paste("Quantidade total:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(covid_DF$casosNovos)

#Distribuicao dados obitos novos DF
den <- density(covid_DF$obitosNovos)
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Qtd. óbitos novos",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "Distribuição dados óbitos novos no Brasil",
     sub = paste("Quantidade total:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(covid_DF$obitosNovos)

