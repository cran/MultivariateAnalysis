#' Resumo das informacoes de cada tratamento em funcao da dissimilaridade
#'
#' @description Esta funcao apresenta informacoes que resumem a matriz de dissimilaridade
#' @usage SummaryDistancia(Dist,ndec=2)
#' @param Dist Matriz de dissimilaridade
#' @param ndec Valor numerico indicando o numero de casas decimais.
#' @return Retorna informacoes importantes sobre cada tratamento em relacao aos
#' demais como distancia media, menor distancia, maior distancia, tratamento
#' mais proximo, tratamento mais distante etc.
#' @seealso /code{/link{dist}/}
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
#'  data(Dados.FMI.Quali)
#' DadosQuali=ApplyDissimilaridade(Dados.FMI.Quali[,6:10],Dados.FMI.Quali[,2])
#' Dist=Distancia(DadosQuali,1)
#' SummaryDistancia(Dist)
#' @export






SummaryDistancia=function(Dist,ndec=2){
  Dist2=as.matrix(Dist)
  diag(Dist2)=NA
  Medio=round(apply(Dist2,1,mean,na.rm=T),ndec)
  Minimo=round(apply(Dist2,1,min,na.rm=T),ndec)
  Maximo=round(apply(Dist2,1,max,na.rm=T),ndec)
  Minimo2=apply(Dist2,1,min,na.rm=T)
  Maximo2=apply(Dist2,1,max,na.rm=T)
  sd=round(apply(Dist2,1,sd,na.rm=T),ndec)

  MaisDistante=MaisProximo=NULL
  for(i in 1:nrow(Dist2)){
    id=Dist2[,i]==Minimo2[i]
    MaisProximo=c(MaisProximo,na.omit(colnames(Dist2)[id])[1])
    id=Dist2[,i]==Maximo2[i]
    MaisDistante=c(MaisDistante,na.omit(colnames(Dist2)[id])[1])
  }

  data.frame(Medio=Medio, Minimo=Minimo,Maximo=Maximo,sd=sd,MaisProximo=MaisProximo,MaisDistante=MaisDistante)
}
