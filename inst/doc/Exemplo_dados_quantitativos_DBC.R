## ---- include = FALSE---------------------------------------------------------
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
round(Dist,3)

## -----------------------------------------------------------------------------
resumo=SummaryDistancia(Dist)
resumo

## -----------------------------------------------------------------------------
#Dendograma com o metodo UPGMA
Dendograma(Dist,Metodo=3)

## -----------------------------------------------------------------------------
#Dendograma com o metodo UPGMA
Tocher(Dist)

## -----------------------------------------------------------------------------
ComponentesPrincipais(DadosMed,padronizar = TRUE)

## -----------------------------------------------------------------------------
VariaveisCanonicas(Dados.DBC,Modelo = 2,)

