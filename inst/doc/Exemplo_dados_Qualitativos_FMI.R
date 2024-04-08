## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.FMI.Quali")
head(Dados.FMI.Quali)

## -----------------------------------------------------------------------------
Fator=Dados.FMI.Quali$Tratamento
DadosQuali=Dados.FMI.Quali[,6:10]
Dados2=ApplyDissimilaridade(Dados = DadosQuali,Factor = Fator)
(head(Dados2))

## -----------------------------------------------------------------------------
#distancia euclidiana padronizada
Dist=Distancia(Dados2,Metodo = 4)

## -----------------------------------------------------------------------------
resumo=SummaryDistancia(Dist)
resumo

## -----------------------------------------------------------------------------
#Dendrograma com o metodo UPGMA
Dendo=Dendrograma(Dist,Metodo=3)
Dendo$SigCorrelCofenetica
Dendo$MojenaCorte

## -----------------------------------------------------------------------------
#Dendrograma com o metodo UPGMA
To=Tocher(Dist)
To$Tocher
To$DistanciaIntraInterCluster
To$CorrelacaoCofenetica

## -----------------------------------------------------------------------------

CO=CoordenadasPrincipais(Dist)

