## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.MED")
Dados.MED

## -----------------------------------------------------------------------------
#colocando nome nos individuos
rownames(Dados.MED)=paste0("T",1:nrow(Dados.MED))
Dist=Distancia(Dados.MED,Metodo = 5)
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

COp=CoordenadasPrincipais(Dist,main = "")
CoordenadasPrincipais(Dist)
ComponentesPrincipais(Dados.MED)

