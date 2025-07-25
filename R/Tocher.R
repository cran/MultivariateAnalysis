#' Agrupamento Tocher
#'
#' @description Esta funcao faz o agrupamento pelo metodo Tocher.
#' @name Tocher
#' @usage Tocher(Dist,
#' Metodo="original",
#' nperm=999,
#' Plot=1,
#' xlab="Dissimilaridade",
#' ylab="Distancia fenetica",
#' bty="n")
#' @param Dist Objeto contendo a matriz de dissimilaridade
#' @param Metodo Um character indicando o algoritimo de agrupamento.
#'  Ha duas possibidades: "original" (default) ou "sequential".
#'   O ultimo foi proposto por Vasconcelos et al. (2007), tambem chamando de
#'    metodo   Tocher modificado.
#' @param nperm Numero de permutacoes para identificar a signficancia pelo
#' metodo de Mantel
#' @param Plot  Numero indicando qual grafico devera ser plotado:
#'  \itemize{
#'      \item"1": Grafico com as distancias intra e intercluster
#'      \item"2": Grafico com as dispersao da distancia cofenetica em funcao
#'       dos valores de dissimilaridade.
#' }
#'
#' @param xlab nome do eixo x do grafico. Deve ser utilizado quando o Plot=2.
#' @param ylab nome do eixo y do grafico. Deve ser utilizado quando o Plot=2.
#' @param bty deve receber un character indicando o tipo de borda desejado no
#'   grafico. Deve ser utilizado quando o Plot=2.
#'
#'
#'    \itemize{
#'      \item"o": Todas as bordas
#'      \item"n": Sem bordas
#'      \item"7": Acima e a direita
#'      \item"L": Abaixo + esquerda (Default)
#'      \item"C": Acima + Direita + Abaixo
#'      \item"U": Direita + Abaixo + Direita
#'      }

#' @return A funcao retorna resultados do teste Tocher.
#' @references
#' PlayList "Curso de Analise Multivariada":
#' https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' Cruz, C.D.; Ferreira, F.M.; Pessoni, L.A. (2011) Biometria aplicada ao estudo
#' da diversidade genetica. Visconde do Rio Branco: Suprema.
#'
#' Rao, R.C. (1952) Advanced statistical methods in biometric research.
#' New York: John Wiley & Sons.
#'
#' Sharma, J.R. (2006) Statistical and biometrical techniques in plant breeding.
#' Delhi: New Age International.

#' Silva, A.R. & Dias, C.T.S. (2013) A cophenetic correlation coefficient for
#' Tocher's method. Pesquisa Agropecuaria Brasileira, 48:589-596.
#'
#' Vasconcelos, E.S.; Cruz, C.D.; Bhering, L.L.; Resende Junior, M.F.R. (2007) Alternative
#'    methodology for the cluster analysis. Pesquisa Agropecuaria Brasileira, 42:1421-1428.
#'
#' @examples
#' \donttest{
#' data("Dados.MED")
#' Dist=Distancia(Dados.MED,Metodo=6)
#' Tocher(Dist)
#'}
#' @importFrom biotools tocher
#' @importFrom graphics abline
#' @importFrom stats cophenetic lm
#' @export
#' @exportS3Method print Tocher
#'
#'
#'
Tocher=function(Dist,Metodo="original",nperm=999, Plot=1,xlab="Dissimilaridade",ylab="Distancia fenetica",bty="n") {
  #    rownames(Dados.MED)=paste0("T",1:10)
  #   Dist=Distancia(Dados.MED,5)0=
  Dist0=Dist
  if(is(Dist)=="Distancia"){Dist=Dist$Distancia}


  D=as.dist(Dist)

  #Passo 0



  x=tocher(D,algorithm = Metodo)

  cof=cophenetic(x)



  DistanciaIntraInterCluster=x$distClust

 # Mantel=biotools::mantelTest(D,cof,graph = FALSE,nperm = nperm)

  Mantel=CorrelacaoMantel(D,cof,nperm = nperm,alternativa = "maior")

  if(Plot==2){
    sig=ifelse(Mantel$p.value>0.05,"ns",ifelse((Mantel$p.value<0.05)&(Mantel$p.value>0.01),"*",ifelse(Mantel$p.value<0.01,"**", "")))
    plot(D,cof,col="blue",bty=bty,main=paste("r=", round(Mantel$correlation,4),sig),xlab=xlab,ylab=ylab)
    abline(lm(cof~D),col=2)
  }



  if(Plot==1){
    D=DistanciaIntraInterCluster
    CP=CoordenadasPrincipais(as.dist(D),plot = FALSE)
    xy=CP$vectors
    if(ncol(xy)>1){dif=(max(xy[,2])-min(xy[,2]))*.04}
    if(ncol(xy)==1){xy=cbind(xy,0.0)
    dif=.06}
    xy=xy[,1:2]


    xy2=xy3=NULL
    for(i in 1:(nrow(xy)-1)){
      for(j in (i+1):nrow(xy)){
        v=rbind(xy[i,],xy[j,])
        xy2=rbind(xy2,v)
        xy3=rbind(xy3,c(colMeans(v),D[i,j]))
      }
    }

    par(mar=c(0,0,1,0)+0.1)
    plot(1.5*xy,col=0,axes = F,ylab="",xlab="",main="Distancia intra e intercluster")
    lines(xy2[,1],xy2[,2],col="blue",lwd=8)
    points(xy[,1],xy[,2],cex=8,pch=16,col="white")
    points(xy[,1],xy[,2],cex=8)

    text(xy[,1],xy[,2]+dif,paste0("C",1:nrow(xy)))
    XXX=(round(diag(D)+0.001,2))
    XXX[XXX==0]="0.00"
    text(xy[,1],xy[,2]-dif,XXX)
    #points(xy3[,1],xy3[,2],cex=6,pch=15,col="white")
    text(xy3[,1],xy3[,2],round(xy3[,3],2))
par(mar=c(5, 4, 4, 2) + 0.1)

  }



classe=x$class
names(classe)=colnames(as.matrix(Dist))
  out=list(#call = match.call(),

    Tocher=x$clusters,
    Classe=classe,
    DistanciaCofenetica=cof,
    DistanciaIntraInterCluster=DistanciaIntraInterCluster,
    CorrelacaoCofenetica=Mantel)
  class(out) <- "Tocher"


  if(is(Dist0)=="Distancia"){
    classe=x$class
    names(classe)=colnames(as.matrix(Dist))

    out=list(#call = match.call(),
      Tocher=x$clusters,
      Classe=classe,
      DistanciaCofenetica=cof,
      DistanciaIntraInterCluster=DistanciaIntraInterCluster,
      CorrelacaoCofenetica=Mantel,
      Distancia=Dist0)
    class(out) <- "Tocher"

  }









return(out)

}


#' @export
print.Tocher=  function (x, ...){
  cat("_________________________________________________________________________","\n")
  cat("Agrupamento Tocher","\n")

  for(i in 1:length(x$Tocher)){
    nomes=unlist(x$Tocher[[i]])
    cat(paste0 ("Cluster",i,":"),"\n")
    cat(nomes,"\n")
    cat("\n")
  }


  cat("Distancia intra e intercluster:","\n")
  Cluster=x$DistanciaIntraInterCluster
  colnames(Cluster)=rownames(Cluster)=paste0("Cluster",1:ncol(Cluster))
  print(Cluster)
  cat("\n")
  cat("\n")

  cat("Correlacao Cofenetica:",x$CorrelacaoCofenetica$correlation,"\n")
  cat("pvalor:",x$CorrelacaoCofenetica$p.value,"baseado no teste Mantel","\n")
  cat("Hipotese alternativa: A correlacao e maior que 0","\n")
  cat("_________________________________________________________________________","\n")
  invisible(x)
}


