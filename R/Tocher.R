#' Agrupamento Tocher
#'
#' @description Esta funcao faz o agrupamento pelo metodo Tocher
#' @usage Tocher(Dist,Metodo="original",nperm=9999, corPlot=TRUE,
#' xlab="Dissimilaridade",ylab="Distancia fenetica",bty="n")
#' @param Dist Objeto contendo a matriz de dissimilaridade
#' @param Metodo Um character indicando o algoritimo de agrupamento.
#'  Ha duas possibidades: "original" (default) ou "sequential".
#'   O ultimo foi proposto por Vasconcelos et al. (2007), tambem chamando de metodo
#'   Tocher modificado.
#' @param nperm Numero de permutacoes para identificar a signficancia pelo metodo de Mantel
#' @param corPlot  Valor logico (TRUE ou FALSE) indicando se aparecera o grafico de
#' correlacao entre as matriz cofenetica e de dissimilaridade
#' @param xlab nome do eixo x do grafico
#' @param ylab nome do eixo y do grafico
#' @param bty deve receber un character indicando o tipo de borda desejado no
#'   grafico.
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
#' @importFrom biotools tocher  mantelTest
#' @importFrom graphics abline
#' @importFrom stats cophenetic lm
#' @export
#'

Tocher=function(Dist,Metodo="original",nperm=9999, corPlot=TRUE,xlab="Dissimilaridade",ylab="Distancia fenetica",bty="n") {
  D=dist(Dist)


x=tocher(D,algorithm = Metodo)
cof=cophenetic(x)



DistanciaIntraInterCluster=x$distClust

Mantel=mantelTest(D,cof,graph = FALSE,nperm = nperm)

if(corPlot==TRUE){
sig=ifelse(Mantel$p.value>0.05,"ns",ifelse((Mantel$p.value<0.05)&(Mantel$p.value>0.01),"*",ifelse(Mantel$p.value<0.01,"**", "")))
plot(D,cof,col="blue",bty=bty,main=paste("r=", round(Mantel$correlation,4),sig),xlab=xlab,ylab=ylab)
abline(lm(cof~D),col=2)
}

out=list(#call = match.call(),
            Tocher=x,
            DistanciaCofenetica=cof,
            DistanciaIntraInterCluster=DistanciaIntraInterCluster,
            CorrelacaoCofenetica=Mantel)
class(out) <- "Tocher"




return(out)

}


#
# print.Tocher=  function (x, ...){
#   cat("_________________________________________________________________________","\n")
#   #cat("Agrupamento Tocher","\n")
#   print(x$Tocher)
#   cat("Distancia intra e intercluster:","\n")
#   print(x$DistanciaIntraInterCluster)
#   cat("\n")
#   cat("\n")
#   cat("Correlacao Cofenetica (teste Mantel):","\n")
#   cat("Correlacao Cofenetica:",x$CorrelacaoCofenetica$correlation,"\n")
#   cat("pvalor:",x$CorrelacaoCofenetica$p.value,"baseado no teste Mantel","\n")
#   cat("Hipotese alternativa: A correlacao e maior que 0","\n")
#
#   #print(x$CorrelacaoCofenetica)
#   cat("_________________________________________________________________________","\n")
#   invisible(x)
# }
#
#
