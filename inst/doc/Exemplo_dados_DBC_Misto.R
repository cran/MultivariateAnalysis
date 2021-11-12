## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.DBC.Misto")
head(Dados.DBC.Misto)

## -----------------------------------------------------------------------------
Res=MANOVA(Dados.DBC.Misto[,1:5],Modelo=2)
Res

## -----------------------------------------------------------------------------
DadosMed=Res$Med
DistMaha=Distancia(DadosMed,Metodo = 7,Cov = Res$CovarianciaResidual)
DistMaha

## -----------------------------------------------------------------------------
resumo=SummaryDistancia(DistMaha)
resumo

## -----------------------------------------------------------------------------
Dadosquali=Dados.DBC.Misto[,6:11]

#Excluindo os valores NA
id=is.na(Dadosquali$CorFolha)==FALSE
Dadosquali2=Dadosquali[id,]
#Colocando o nome dos tratamentos na matriz
rownames(Dadosquali2)=Dados.DBC.Misto[id,1]


Distquali=Distancia(Dadosquali2,Metodo = 10)
round(Distquali$Distancia,3)

## -----------------------------------------------------------------------------

#Criando list com as matrizes
dissimilaridades=list(DistMaha,Distquali)
n=c(ncol(DadosMed),ncol(Dadosquali2))
#Calculando a media ponderada
DistMisto=MediaDistancia(dissimilaridades,n)
DistMisto


## -----------------------------------------------------------------------------
#Dendrograma com o metodo UPGMA
Dendrograma(DistMaha,Metodo=3,Titulo="Dados quantiativos")

## -----------------------------------------------------------------------------
Dendrograma(Distquali,Metodo=3, Titulo="Dados qualitativos")

## -----------------------------------------------------------------------------
Dendrograma(DistMisto,Metodo=3,Titulo= "Qualitativos + Quantiativos")

## -----------------------------------------------------------------------------
Tocher(DistMisto)

## -----------------------------------------------------------------------------
CorrelacaoMantel(DistMaha,DistMisto)

## -----------------------------------------------------------------------------
CorrelacaoMantel(DistMaha,Distquali)

## -----------------------------------------------------------------------------
CorrelacaoMantel(Distquali,DistMisto)

