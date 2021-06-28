#' Media ponderada entre as matrizes de dissimilaridade
#'
#' @description Esta funcao faz a padronizacao da matriz de dissimilaridade
#' a fim de retirar a escala, nesta etapa, os valores das matrizes sao calculados,
#' a fim de variar entre 0 e 1. Posteriormente, e feita a media ponderada entre
#' essas matrizes em funcao do numero de variaveis consideradas na estimativa
#' de cada uma dessas dissimilaridade. Essa funcao e importante quando se trabalha
#' com dados mistos.
#' @usage MediaDistancia(Dist1,Dist2,n1=NULL,n2=NULL)
#' @param Dist1 Matriz contendo as medidas dissimilaridade da primeira matriz.
#' @param Dist2 Matriz contendo as medidas dissimilaridade da segunda matriz.
#' @param n1 Numero de variaveis utilizadas para a obtencao das medidas
#' dissimilaridade da primeira matriz.
#' @param n2 Numero de variaveis utilizadas para a obtencao das medidas
#' dissimilaridade da segunda matriz.
#' @return Retorna a media ponderada de duas matrizes de dissimilaridade.
#' @seealso \code{\link{dist}}
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
#' DadosQuanti=matrix(rnorm(100,100,5),ncol=4)
#' DadosQuali=matrix(round(runif(200,1,5),0),ncol=8)
#' rownames(DadosQuanti)=rownames(DadosQuali)=paste("T",1:25,sep="_")
#' DistQuant=Distancia(DadosQuanti,4)
#' DistQuali=Distancia(DadosQuali,10)
#' Dist=MediaDistancia(DistQuant, DistQuali,ncol(DadosQuanti),ncol(DadosQuali))
#' Dist
#' Dendograma(Dist,3)
#' @export





MediaDistancia=function(Dist1,Dist2,n1=NULL,n2=NULL){
  DistQuali=Dist1
  DistQuanti=Dist2
  DistQualiN=Normatiza(DistQuali,DistQuali,LimiteInferior = 0,LimiteSuperior = 1,Metodo = 2)
  DistQuantiN=Normatiza(DistQuanti,DistQuanti,LimiteInferior = 0,LimiteSuperior = 1,Metodo = 2)

  DadosMix=(n1*DistQualiN+n2*DistQuantiN)/(n1+n2)
  return(DadosMix)
}
