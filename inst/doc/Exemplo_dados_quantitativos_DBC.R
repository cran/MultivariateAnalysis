## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.DBC")
head(Dados.DBC)

## -----------------------------------------------------------------------------
Res=MANOVA(Dados.DBC,Modelo=2)
Res

## -----------------------------------------------------------------------------
#colocando nome nos individuos
DadosMed=Res$Med
Dist=Distancia(DadosMed,Metodo = 7,Cov = Res$CovarianciaResidual)
round(Dist$Distancia,3)

## -----------------------------------------------------------------------------
resumo=SummaryDistancia(Dist)
resumo

## -----------------------------------------------------------------------------
#Dendrograma com o metodo UPGMA
Dendrograma(Dist,Metodo=3)

## -----------------------------------------------------------------------------
#Dendrograma com o metodo UPGMA
Tocher(Dist)

## -----------------------------------------------------------------------------
ComponentesPrincipais(DadosMed,padronizar = TRUE)

## -----------------------------------------------------------------------------
VariaveisCanonicas(Dados.DBC,Modelo = 2,)

