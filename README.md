# Intro

Esse markdown tem como objetico apresentar um estudo de caso para o uso da ferramenta CausalImpact [Pacote do Google (para mais informações)](https://google.github.io/CausalImpact/CausalImpact.html) feito em um determinado produto em um grande instituição financeira. A idéia é validar a usabilidade da ferramenta (R) com a ferramenta do GooglAnalytics para cenários onde não for possível uso de teste A/B tradicional e mais importante, podendo fazer uso como indicador de performance.

## Sobre o modelo

Para a modelagem do pós-período, usamos como treinamento o período de 35 dias de dados temporais retirados da ferramenta GoogleAnalytics que se referem a página de meta (o arquivo encontra-se no repositorio/files/analitics.xlsx e foi alterado para preservar a fonte), isto é onde inicia o fluxo de contratação do produto de interesse. A função forecast HoltWinters foi escolhida para nosso artigo e usamos de tratamento orientado a objeto para retirar o modelo (linha tracejada pré-período, como veremos) e também a predição pós-período que teria acontecido, como controle do modelo no uso da ferramenta CausalImpact. Assim o modelo para inferência, temos grande ganho no uso de Modelo de espaço de estado de suavização exponencial.

## Sobre o fluxo

Para a maioria dos produtos da instituição, há vários meios de captação de clientes. Porém para os produtos digitais, há varias campanhas ou meios até inicio de funil. Todos os meios (seja pela homepage, pagina do produto, adwords, instagram ou qualquer outra mídia acaba na página analisada).  

Em tempos em tempos há uma alavancagem por meio de promoção por campanhas com homepage dedicada. E é nesse ponto que vamos explorar a página de início de funil, examinando o aumento de fluxo e estimar de maneira sólida a melhora dado o início dessa campanha. 

# Modelagem

O relatório do vindo do googleanalytics geralmente é exportado como .csv ou .xlsx, nesse caso temos um arquivo xlsx 

```{r}

library(readxl)
library(dplyr)


#Leitura Dados GA
#Os dados vem do relatorio manual do GA de um período escolhido
campanha <- read_excel("files/analytics.xlsx")

head(campanha)
```

```{r }
#Tratamento dos dados

dados <- dplyr::select(campanha, Data, "Visualizações de páginas únicas")
nlinhas <- nrow(dados)-1
dados <- dados[1:nlinhas,]
```

Para esse modelo, é importante termos o pré-periodo que são os dados até o início da campanha.
``` {r }

#Criamos os dados do período pré campanha para nosso modelo
pre.periodo <- data.frame(dados$`Visualizações de páginas únicas`[1:37])

``` 

Assim podemos ver como é o comportamento dos clientes na página de início de funil. Notamos aqui um problema de frequência na segunda semana. Esse problema específico se deu a um erro de configuração durante o período.
``` {r }
#Transformamos em time-series
nts <- ts(pre.periodo, frequency = 7)
plot(nts)
```

Assim, usamos o pacote holtwinters para modelar nossa pequena série, vemos aqui o fit tem grande influencia pelo período da segunda semana.
``` {r }
#Fazemos o modelo mais simples possível, porém devemos usar o alpha e gama.
#Pois o smooth é bom
fit <- HoltWinters(nts)
plot(fit)
```

Assim, podemos usar o pacote forecast e usar os parâmetros de nosso modelo holtwinters. Selecionamos o tamanho do período da campanha, que durou 23 dias.
``` {r}
## Cria o Modelo ##
library(forecast)

#Fazemos o forecast
modelo <- forecast(HoltWinters(nts), 23)
plot (forecast(HoltWinters(nts), 23))
```


Então usamos os dados de forecast para completar nossa série temporal.
``` {r}
#o mesmo que modelo
dados.forecast <- forecast(HoltWinters(nts),16)

plot(fit$fitted[,1])
```

Unimos os dados do analytics com o forecast:
``` {r}
#Monta os dados do modelo + predicao em x1
x1 <- append(fit$fitted[,1], modelo$mean)


plot(ts(x1))
```

E também temos os dados reais do período. Visivelmente notamos o aumento na página de início de funil de vendas. Em seguida transformamos em matriz, pois o modelo exige esse formato.
```{r}
y <- dados$`Visualizações de páginas únicas`
plot(ts(y))
#Consolida as duas ts
dados_final <- data.frame(y,x1);
dados_final <- data.matrix(dados_final)
```

Desse modo temos os dados prontos para verificar o efeito causal da campanha.
``` {r warning=FALSE, message=FALSE}
### Causal Impact ###
library(CausalImpact)

pre.period <- c(1, 37)
post.period <- c(38, 53)

impact <- CausalImpact(dados_final, pre.period, post.period)


plot(impact)
```

Como vemos acima, o nosso modelo (tracejado) tem menor erro nas datas próximas ao início da campanha, pelo fato de estar mais afastado ao período de erro de medição.

De maneira mais explícita, o pacote nos permite ver as métrica e ainda exporta um relatório robusto sobre os resultados. Onde o que importa a nós é o efeito relativo.

```{r}
summary(impact)
```

Como mencionado:
```{r}
summary(impact, "report")
```

# Conclusão

Vemos que o report fornecido complementa a análise significativamente e traz as com rigor o resultado. Complementamos ainda com os resultados divulgados oficialmente para o produto que estudamos, resultantes da página da campanha publicitária. Onde foram designados exatamente 406 leads para o início do fluxo de contratação. 

Como o nosso modelo não possui dados de lead da página da campanha, podemos fazer a análise comparativa como validação e nesse ponto essa abordagem foi muito boa usando apenas a média do forecast. 

Você percebe que temos um grande número de variáveis criadas nesse artigo. Não é necessário, mas o recurso foi utilizado para ficar mais didático; 

