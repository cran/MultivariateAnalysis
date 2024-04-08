#'Comparacoes multiplas multivariadas
#'
#' @description Esta funcao retorna o resultado da comparacao multiplas (dois a dois)
#' com abordagem multivariada.
#' @name PairComp
#' @usage PairComp(MANOVA,adjust="bonferroni",test="Pillai",Sig=0.05)
#' @param MANOVA    Resultado da funcao MANOVA
#' @param adjust    Ajuste da significancia para o teste de comparacao multipla.
#' Pode ser: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#'   "fdr" ou "none".
#' @param test Teste considerado na MANOVA. Pode ser:"Pillai", "Wilks",
#'  "Hotelling-Lawley" ou "Roy"
#' @param Sig Significancia a ser considerado. Default e 0.05.
#' @return A funcao as comparações multiplas para os tratamentos.
#' @seealso \code{\link{lm}}, \code{\link{manova}}
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.  (ISBN: 8572691510)
#'
#' Da Silva, A.R.; Malafaia, G.; Menezes, I.P.P. (2017) biotools: an R function
#' to predict spatial gene diversity via an individual-based approach. Genetics
#' and Molecular Research, 16: gmr16029655.
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p. (ISBN 13:978 8581270630)
#'
#' Krzanowski, W. J. (1988) Principles of Multivariate Analysis. A User's Perspective. Oxford.
#'
#' HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978 0138132637)
#' @examples
#' \donttest{
#' #Delineamento inteiramente casualizado (DIC)
#' data("Dados.DIC")
#' Res=MANOVA(Dados.DIC,1)
#' PairComp(Res)
#'
#' #Delineamento em blocos casualizados (DBC)
#' data(Dados.DBC)
#' Res=MANOVA(Dados.DBC,2)
#' PairComp(Res)
#'
#' #Delineamento em quadrado latino (DQL)
#' data(Dados.DQL)
#' Res=MANOVA(Dados.DQL,3)
#' PairComp(Res)
#'
#' #Esquema fatorial duplo em DIC
#' data(Dados.Fat2.DIC)
#' Res=MANOVA(Dados.Fat2.DIC,4)
#' PairComp(Res)
#'
#' #Esquema fatorial duplo em DBC
#' data(Dados.Fat2.DBC)
#' Res=MANOVA(Dados.Fat2.DBC,5)
#' PairComp(Res)
#'
#' #' #Esquema fatorial triplo em DIC
#' data(Dados.Fat3.DIC)
#' Res=MANOVA(Dados.Fat3.DIC,6)
#' PairComp(Res)
#'
#' #Esquema fatorial triplo em DBC
#' data(Dados.Fat3.DBC)
#' Res=MANOVA(Dados.Fat3.DBC,7)
#' PairComp(Res)
#' }
#'
#' @importFrom biotools mvpaircomp
#' @export
#' @exportS3Method print MANOVA

PairComp=function(MANOVA,adjust="bonferroni",test="Pillai",Sig=0.05){

res=MANOVA

D=res$Data
Modelo=res$Modelo

#Dic
if(Modelo==1){
  Trat= as.factor(D[,1])
  Y=as.matrix(D[,-(1:2)])

  M <- lm(Y ~ Trat)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCM=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)
  Result=TCM
  }
#DBC
if(Modelo==2){
  Trat= as.factor(D[,1])
  Bloco= as.factor(D[,2])
  Y=as.matrix(D[,-(1:2)])

  M <- lm(Y ~ Trat+Bloco)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCM=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)
  Result=TCM
}
#DQL
if(Modelo==3){
  Trat= as.factor(D[,1])
  Linha= as.factor(D[,2])
  Coluna= as.factor(D[,3])
  Y=as.matrix(D[,-(1:3)])

  M <- lm(Y ~ Trat+Linha,Coluna)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCM=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)
  Result=TCM
}

#Fat2Dic
if(Modelo==4){
  FatorA= as.factor(D[,1])
  FatorB= as.factor(D[,2])
  #Bloco= as.factor(D[,3])
  Y=as.matrix(D[,-(1:3)])

  M <- lm(Y ~ FatorA*FatorB)
  res2=mvpaircomp(M, factor1 = "FatorA",test = test, adjust = adjust)
  TCMa=CompMed(n=length(unique(FatorA)),Pvalor=res2$st[,5,1],Sig=Sig)

  res2=mvpaircomp(M, factor1 = "FatorB",test = test, adjust = adjust)
  TCMb=CompMed(n=length(unique(FatorB)),Pvalor=res2$st[,5,1],Sig=Sig)

  Trat=FatorA:FatorB
  M <- lm(Y ~ Trat)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCMab=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)
  Result=list(FA=TCMa,FB=TCMb,FAB=TCMab)
  }

#Fat2Dbc
if(Modelo==5){
  FatorA= as.factor(D[,1])
  FatorB= as.factor(D[,2])
  Bloco= as.factor(D[,3])
  Y=as.matrix(D[,-(1:3)])


  M <- lm(Y ~ FatorA*FatorB+Bloco)
  res2=mvpaircomp(M, factor1 = "FatorA",test = test, adjust = adjust)
  TCMa=CompMed(n=length(unique(FatorA)),Pvalor=res2$st[,5,1],Sig=Sig)

  res2=mvpaircomp(M, factor1 = "FatorB",test = test, adjust = adjust)
  TCMb=CompMed(n=length(unique(FatorB)),Pvalor=res2$st[,5,1],Sig=Sig)

  Trat=FatorA:FatorB
  M <- lm(Y ~ Trat+Bloco)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCMab=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)
  Result=list(FA=TCMa,FB=TCMb,FAB=TCMab)
  }

#Fat3Dic
if(Modelo==6){
  FatorA= as.factor(D[,1])
  FatorB= as.factor(D[,2])
  FatorC= as.factor(D[,3])
  Y=as.matrix(D[,-(1:4)])



  M <- lm(Y ~ FatorA*FatorB*FatorC)
  res2=mvpaircomp(M, factor1 = "FatorA",test = test, adjust = adjust)
  TCMa=CompMed(n=length(unique(FatorA)),Pvalor=res2$st[,5,1],Sig=Sig)

  res2=mvpaircomp(M, factor1 = "FatorB",test = test, adjust = adjust)
  TCMb=CompMed(n=length(unique(FatorB)),Pvalor=res2$st[,5,1],Sig=Sig)

  res2=mvpaircomp(M, factor1 = "FatorC",test = test, adjust = adjust)
  TCMc=CompMed(n=length(unique(FatorB)),Pvalor=res2$st[,5,1],Sig=Sig)

  Trat=FatorA:FatorB:FatorC
  M <- lm(Y ~ Trat)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCMab=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)
  Result=list(FA=TCMa,FB=TCMb,FC=TCMc,FABC=TCMab)

  }

#Fat3Dbc
if(Modelo==7){
  FatorA= as.factor(D[,1])
  FatorB= as.factor(D[,2])
  FatorC= as.factor(D[,3])
  Bloco= as.factor(D[,4])
  Y=as.matrix(D[,-(1:4)])


  M <- lm(Y ~ FatorA*FatorB*FatorC+Bloco)
  res2=mvpaircomp(M, factor1 = "FatorA",test = test, adjust = adjust)
  TCMa=CompMed(n=length(unique(FatorA)),Pvalor=res2$st[,5,1],Sig=Sig)

  res2=mvpaircomp(M, factor1 = "FatorB",test = test, adjust = adjust)
  TCMb=CompMed(n=length(unique(FatorB)),Pvalor=res2$st[,5,1],Sig=Sig)

  res2=mvpaircomp(M, factor1 = "FatorC",test = test, adjust = adjust)
  TCMc=CompMed(n=length(unique(FatorB)),Pvalor=res2$st[,5,1],Sig=Sig)

  Trat=FatorA:FatorB:FatorC
  M <- lm(Y ~ Trat+Bloco)
  res2=mvpaircomp(M, factor1 = "Trat",test = test, adjust = adjust)
  TCMab=CompMed(n=length(unique(Trat)),Pvalor=res2$st[,5,1],Sig=Sig)

  Result=list(FA=TCMa,FB=TCMb,FC=TCMc,FABC=TCMab)

}

return(Result)
}
