library(readxl)
library(dplyr)


#Leitura Dados GA
#Os dados vem do relatorio manual do GA de um período escolhido
cartoes <- read_excel("C:/Users/X209285/Desktop/Vitrine/Campanhas/Ferias/cartoes.xlsx")

head(cartoes)

#Tratamento dos dados

dados <- dplyr::select(cartoes, Data, "Visualizações de páginas únicas")
nlinhas <- nrow(dados)-1
dados <- dados[1:nlinhas,]

#Acaba que não usamos as datas agora
dados$Data <- as.Date(dados$Data, format = "%Y%m%d")
#ordenando por data
#dados <- dados[order(dados$Data , decreasing = FALSE ),]

#Criamos os dados do período pré campanha para nosso modelo
pre.periodo <- data.frame(dados$`Visualizações de páginas únicas`[1:37])
pre.periodo$`Visualizações de páginas únicas` <- as.double(pre.periodo$`Visualizações de páginas únicas`)
### ok ###
#Transformamos em trime-series
nts <- ts(pre.periodo, frequency = 7)
plot(nts)

#Fazemos o modelo mais simples possível, porém devemos usar o alpha e gama.
#Pois o smooth é bom
fit <- HoltWinters(nts)
plot(fit)

## Cria o Modelo ##
library(forecast)

#Fazemos o forecast
modelo <- forecast(HoltWinters(nts), 23)
plot (forecast(HoltWinters(nts), 23))

# Criamos a matriz para ser usada no Causal Impact

#O mesmo que preperiodo
nv_dados <- data.frame(dados$`Visualizações de páginas únicas`)

#nao funcionando ts
# nv_dados <- data.frame(ts(modelo$model$xhat))
#plot(ts(modelo$model$xhat))
#plot(modelo$mean)


#o mesmo que modelo
dados.forecast <- forecast(HoltWinters(nts),16)

nv_for <- data.frame(dados.forecast$fitted)

#Monta os dados do modelo + predicao em x1
x1 <- append(fit$fitted[,1], modelo$mean)
plot(fit$fitted[,1])

plot(ts(x1))

y <- dados$`Visualizações de páginas únicas`
plot(ts(y))
#Consolida as duas ts
#dados_final <- data.matrix(y,x1)
dados_final <- data.frame(y,x1);
dados_final <- data.matrix(dados_final)

### Causal Impact ###
library(CausalImpact)

pre.period <- c(1, 37)
post.period <- c(38, 53)

impact <- CausalImpact(dados_final, pre.period, post.period)
#impact <- CausalImpact(dados2, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

plot(impact)

### mais ###

nts <- ts(dados, frequency = 7)
plot(nts)
fit
fit <- HoltWinters(nts, beta = F, gamma = F)

plot(fit)


library(forecast)
auto.arima(nts)
#Os Dados Estão ok
#Agora falta: 
#1. Criar um modelo de forecast para o período antes do inicio da campanha
#2. predict para o período pos inicio de campanha e cbind com os dados
#3. Usar o causalimpact para mensurar e plotar
#4. Podemos exportar o modelo antes do início da campanha e subir no Live

#WorkFlow: 1.Estrutura os dados antes da campanha> 2.Cria modelo preditivo forecast> 3. exporta dados para live >4. roda resultado somado no Live
#          A+: gera os dados dia a dia>exporta> importa Live> plota o Impacto no live



#data <- zoo(dados$`Visualizações de páginas únicas`)
#data <- cbind(as.Date(dados$Data), data)

###############Teste Funcionalidade####################
###################Dados Falsos########################

pre.period <- c(1, 36)
post.period <- c(37, 53)

dados3 <- data.frame(
  y = dados$`Visualizações de páginas únicas` * 1.2, 
  x1 = dados$`Visualizações de páginas únicas`
);
dados3$y[37:53] <- dados3$y[37:53] + 200

#dados3 <- data.matrix(dados3)

#dados2 <- dplyr::select(cartoes, "Visualizações de páginas únicas")

impact <- CausalImpact(dados3, pre.period, post.period)
#impact <- CausalImpact(dados2, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

plot(impact)