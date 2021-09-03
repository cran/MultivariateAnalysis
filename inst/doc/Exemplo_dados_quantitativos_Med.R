## ---- include = FALSE---------------------------------------------------------
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

COp=CoordenadasPrincipais(Dist,main = "")
CoordenadasPrincipais(Dist,Dados = Dados.MED,Padronizar = TRUE,main="")

## -----------------------------------------------------------------------------
ComponentesPrincipais(Dados.MED)

