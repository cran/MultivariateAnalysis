## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.DIC")
Dados.DIC

## -----------------------------------------------------------------------------
Res=MANOVA(Dados.DIC,Modelo=1)
Res

## -----------------------------------------------------------------------------
#colocando nome nos individuos
DadosMed=Res$Med
Dist=Distancia(DadosMed,Metodo = 7,Cov = Res$CovarianciaResidual)
Dist

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
VC=VariaveisCanonicas(Dados.DIC,Modelo = 1)

