#' Dendograma
#'
#' @description Esta funcao retorna a figura do dendograma, distancias feneticas
#'   e correlacao cofenetica.
#' @usage Dendograma(Dissimilaridade,Metodo=3,nperm=999)
#' @param Dissimilaridade    Matriz contendo a estimativa das distancias entre
#'   tratamentos.
#' @param Metodo Valor numerico indicando o metodo a ser utilizado:
#'   \itemize{
#'  \item  1 = Ligacao simples (Metodo do vizinho mais proximo).
#'  \item  2 = Ligacao completa (Metodo do vizinho distante).
#'   \item  3 = Ligacao media entre grupo (UPGMA).
#'   \item  4 = Metodo de Ward.
#'   \item  5 = Metodo de ward (d2).
#'   \item  6= Metodo da mediana (WPGMC).
#'   \item  7= Metodo do centroide (UPGMC).
#'   \item  8 = Metodo mcquitty (WPGMA).
#'   }
#' @param nperm Numero de permutacoes do teste mantel para testar a significancia
#' pelo teste Mantel.
#' @return A funcao retorna o dendograma, distancias feneticas e correlacao
#'   cofenetica.
#'
#' @seealso \code{\link{hclust}}, \code{\link{dist}}
#' @references
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
#' data(Dados.MED)
#' Dist=Distancia(Dados.MED,2)
#' Dendograma(Dist,2)
#' @export
Dendograma=function(Dissimilaridade,Metodo=3,nperm=999)

{
############################################################
Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )
Dissimilaridade=as.dist(Dissimilaridade)

Arvore=hclust(Dissimilaridade,Met[Metodo])
plot(Arvore,hang=-1)

DistanciaFenetica=cophenetic(Arvore)
CorrelacaoCofenetica=cor(Dissimilaridade,DistanciaFenetica)

Mantel=mantelTest(Dissimilaridade,DistanciaFenetica,nperm = nperm,graph  = F)
fusao=Arvore$height
MojenaCorte=c(`k=1.25`=mean(fusao)+sd(fusao)*1.25,`k=2`=mean(fusao)+sd(fusao)*2)

resultado=list(#call = match.call(),
               DistanciaFenetica=DistanciaFenetica,CorrelacaoCofenetica=CorrelacaoCofenetica,SigCorrelCofenetica=Mantel,MojenaCorte=MojenaCorte,Ordem=Arvore$order)
# class(resultado)="Dendograma"
#
# print.Dendograma = function (x, ...){
#   cat("_________________________________________________________________________","\n")
#   cat("Estimativa de correlacao cofenetica:","\n")
#   print(x$CorrelacaoCofenetica)
#   cat("Significancia da correlacao cofenetica pelo teste Mantel","\n")
#   cat("pvalor:",x$SigCorrelCofenetica$p.value,"baseado no teste Mantel","\n")
#   cat("Hipotese alternativa: A correlacao e maior que 0","\n")
#   cat("\n")
#   cat("Ponto de corte pelo metodo Mojena","\n")
#   print(x$MojenaCorte)
# }


#print.Dendograma(resultado)
return(resultado)
  }

