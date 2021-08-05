## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.Fat2.DBC")
head(Dados.Fat2.DBC)

## -----------------------------------------------------------------------------
Res=MANOVA(Dados.Fat2.DBC,Modelo=5)
Res

## -----------------------------------------------------------------------------
#Carregando a média dos tratamentos
DadosMed=Res$Med
head(DadosMed)
Dist=Distancia(DadosMed,Metodo = 7,Cov = Res$CovarianciaResidual)


## -----------------------------------------------------------------------------
resumo=SummaryDistancia(Dist)
resumo

## -----------------------------------------------------------------------------
#Dendograma com o metodo UPGMA
Dendo=Dendograma(Dist,Metodo=3)

## -----------------------------------------------------------------------------
To=Tocher(Dist)
To$Tocher

## -----------------------------------------------------------------------------
CP=ComponentesPrincipais(DadosMed,padronizar = TRUE)

## -----------------------------------------------------------------------------
VC=VariaveisCanonicas(Dados.Fat2.DBC,Modelo = 5,Fator = "A:B")

## -----------------------------------------------------------------------------
VC=VariaveisCanonicas(Dados.Fat2.DBC,Modelo = 5,Fator = "A")
VC=VariaveisCanonicas(Dados.Fat2.DBC,Modelo = 5,Fator = "B")

