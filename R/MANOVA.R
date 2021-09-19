#'Analise de variancia multivariada
#'
#' @description Esta funcao retorna o resultado da analise de variancia
#'  multivariada (MANOVA).
#' @usage MANOVA(Dados,Modelo)
#' @param Dados    Matriz contendo os dados para execucao da MANOVA. Para cada
#'  modelo o conjunto de dados precisa estar organizado de uma forma apropriada:
#'  \itemize{ \item Modelos 1 e 2: As duas primeiras colunas devem conter a
#'  identificacao dos tratamentos e repeticoes/blocos, e as demais os valores
#'  observanos nas variaveis respostas. \item Modelo 3: As tres primeiras
#'  colunas devem conter as informacoes dos tratamentos, linhas e colunas, e
#'  posteriormente, os valores da variavel resposta. \item Modelos 4 e 5: as
#'  primeiras colunas precisam ter a informacao do fator A, fator B,
#'  repeticao/bloco, e posteriormente, as variaveis respostas. }
#' @param Modelo    Valor numerico indicando o delineamento: \itemize{ \item 1 =
#'  Delineamento inteiramente casualizado (DIC) \item 2 = Delineamento em blocos
#'  casualizados (DBC) \item 3 = Delineamento em quadrado latino (DQL) \item 4 =
#'  Esquema fatorial em DIC \item 5 = Esquema fatorial em DBC }
#' @return A funcao retorna a MANOVA, a matriz de (co)variancia residual e o
#'  numero dos graus de liberdade do residuo.
#' @seealso \code{\link{lm}}, \code{\link{manova}}
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.  (ISBN: 8572691510)
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p. (ISBN 13:978 8581270630)
#'
#'  HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978 0138132637)
#' @examples
#' \donttest{
#' #Delineamento inteiramente casualizado (DIC)
#' data("Dados.DIC")
#' MANOVA(Dados.DIC,1)
#'
#' #Delineamento em blocos casualizados (DBC)
#' data(Dados.DBC)
#' MANOVA(Dados.DBC,2)
#'
#' #Delineamento em quadrado latino (DQL)
#' data(Dados.DQL)
#' MANOVA(Dados.DQL,3)
#'
#' #Esquema fatorial em DIC
#' data(Dados.Fat2.DIC)
#' MANOVA(Dados.Fat2.DIC,4)
#'
#' #Esquema fatorial em DBC
#' data(Dados.Fat2.DBC)
#' MANOVA(Dados.Fat2.DBC,5)
#' }
#' @export
#' @exportS3Method print MANOVA

MANOVA=function(Dados,Modelo=2){
D=data.frame(Dados)
modelos=list(
  Y~Trat,
  Y~Trat+Bloco,
  Y~Trat+Linha+Coluna,
  Y~FatorA*FatorB,
  Y~FatorA*FatorB+Bloco)

#Dic
if(Modelo==1){
  Trat= as.factor(D[,1])
  Y=as.matrix(D[,-(1:2)])
}
#DBC
if(Modelo==2){
  Trat= as.factor(D[,1])
  Bloco= as.factor(D[,2])
  Y=as.matrix(D[,-(1:2)])
}
#DQL
if(Modelo==3){
  Trat= as.factor(D[,1])
  Linha= as.factor(D[,2])
  Coluna= as.factor(D[,3])
  Y=as.matrix(D[,-(1:3)])
}

#Fat2Dic
if(Modelo==4){
  FatorA= as.factor(D[,1])
  FatorB= as.factor(D[,2])
  #Bloco= as.factor(D[,3])
  Y=as.matrix(D[,-(1:3)])
}

#Fat2Dbc
if(Modelo==5){
  FatorA= as.factor(D[,1])
  FatorB= as.factor(D[,2])
  Bloco= as.factor(D[,3])
  Y=as.matrix(D[,-(1:3)])
}

ajuste=manova(modelos[[Modelo]])

MANOVAS=list(Teste_Pillai=summary(ajuste,"Pillai")$stats,
     Teste_Wilks=summary(ajuste,"Wilks")$stats,
     Teste_HotellingL=summary(ajuste,"Hotelling-Lawley")$stats,
     Teste_Roy=summary(ajuste,"Roy")$stats)
GLR=ajuste$df.residual
Cov=summary(ajuste)$SS$Residuals/GLR


if(Modelo<4){
  Med=apply(Y,2,function(x) tapply(x,Trat,mean))
}

if(Modelo>3){
  Med=apply(Y,2,function(x) tapply(x,FatorA:FatorB,mean))
}
Resultado=list(Manova=MANOVAS,CovarianciaResidual=Cov,GLres=GLR,Med=Med)
class(Resultado)="MANOVA"
 return(Resultado)}


print.MANOVA=function(x,...){
  cat("__________________________________________________________________________","\n")
  cat("MANOVA com o teste Pillai","\n")
  print(x$Manova$Teste_Pillai)
  cat(" ","\n")

  cat("MANOVA com o teste Wilks","\n")
  print(x$Manova$Teste_Wilks)
  cat(" ","\n")
  cat("MANOVA com o teste Hotelling","\n")
  print(x$Manova$Teste_HotellingL)
  cat(" ","\n")
  cat("MANOVA com o teste Roy","\n")
  print(x$Manova$Teste_Roy)
  cat(" ","\n")
  cat("As medias dos tratamentos podem ser acessados com o $Med","\n")
  cat("Os Graus de liberdade do residuo podem ser acessados com o $GLres","\n")
  cat("A matriz de (co)variancias residuais pode ser acessada com o $CovarianciaResidual","\n")

  cat("__________________________________________________________________________","\n")



  }
