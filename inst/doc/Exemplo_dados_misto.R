## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MultivariateAnalysis)

## -----------------------------------------------------------------------------
data("Dados.Misto")
Dados.Misto

## -----------------------------------------------------------------------------
#colocando nome nos individuos
rownames(Dados.Misto)=paste0("T",1:nrow(Dados.Misto))
Gower1=Distancia(Dados.Misto,Metodo = 21)
round(Gower1$Distancia,3)

## -----------------------------------------------------------------------------
Gower2=Distancia(Dados.Misto,Metodo = 22)
round(Gower2$Distancia,3)

## -----------------------------------------------------------------------------
#Indice de jacard
DistBin=Distancia(Dados.Misto[,c(1:5)],Metodo = 12)
#Indice de discordancia
DistCat=Distancia(Dados.Misto[,c(8,9,11)],Metodo = 10)
#Distancia euclidiana padronizada
DadosQanti=Distancia(Dados.Misto[,c(6,7,10)],Metodo = 5)

#Criando list com as matrizes
dissimilaridades=list(DistBin,DistCat,DadosQanti)

#Calculando a media ponderada
Metodo3=MediaDistancia(dissimilaridades,n=c(5,3,3))
Metodo3


## -----------------------------------------------------------------------------
DadosQuanti=Dados.Misto[,c(6,7,10)]
DadosQuanti
Mat=Quant2Quali(DadosQuanti,nclasses = 4)
Mat

#Substituido nos dados os valores quatitativos por qualitativos
Dados.Misto2=Dados.Misto
Dados.Misto2[,c(6,7,10)]=Mat
Dados.Misto2

## -----------------------------------------------------------------------------
Metodo4=Distancia(Dados.Misto2,Metodo = 10)
Metodo4

## -----------------------------------------------------------------------------
Dendograma(Gower1,Metodo=3,Titulo="Gower1")

## -----------------------------------------------------------------------------
Dendograma(Gower2,Metodo=3,Titulo="Gower2")

## -----------------------------------------------------------------------------
Dendograma(Metodo3,Metodo=3,Titulo="Metodo3")

## -----------------------------------------------------------------------------
Dendograma(Metodo4,Metodo=4,Titulo="Metodo4")

## -----------------------------------------------------------------------------
mat=cbind(Gower1=Gower1$Distancia,Gower2=Gower2$Distancia,Metodo3=Metodo3,Metodo4=Metodo4$Distancia)
Cor=cor(mat)
Cor

## -----------------------------------------------------------------------------
ComponentesPrincipais.Misto(Dados.Misto,plot = "individuos")

## -----------------------------------------------------------------------------
CPM=ComponentesPrincipais.Misto(Dados.Misto,plot = "nivel")

## -----------------------------------------------------------------------------
CPM=ComponentesPrincipais.Misto(Dados.Misto,plot = "correlacao")

## -----------------------------------------------------------------------------
CPM=ComponentesPrincipais.Misto(Dados.Misto,plot = "pesos")

