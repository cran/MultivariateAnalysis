#' Media ponderada entre as matrizes de dissimilaridade
#'
#' @description Esta funcao faz a padronizacao da matriz de dissimilaridade
#' a fim de retirar a escala, nesta etapa, os valores das matrizes sao calculados,
#' a fim de variar entre 0 e 1. Posteriormente, e feita a media ponderada entre
#' essas matrizes em funcao do numero de variaveis consideradas na estimativa
#' de cada uma dessas dissimilaridade. Essa funcao e importante quando se trabalha
#' com dados mistos.
#' @usage MediaDistancia(Distancias,n,Normatizar=TRUE)
#' @param Distancias Objeto do tipo list contendo as matrizes de dissimilaridade.
#' @param n Vetor com o numero de variaveis consideradas na estimacao de cada medida
#' de dissimilaridade.
#' @param Normatizar Valor Logico. Se TRUE a matriz de dissimilaridade sera normatizada
#' para que os valores variem entre 0 e 1.
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
#'
#' #Obentendo as dados de exemplo
#' DadosQuanti=matrix(rnorm(100,100,5),ncol=4)
#' DadosQuali=matrix(round(runif(200,1,5),0),ncol=8)
#' DadosBin=matrix(round(rbinom(400 ,1,0.5),0),ncol=16)
#' rownames(DadosQuanti)=rownames(DadosQuali)=rownames(DadosBin)=paste("T",1:25,sep="_")
#'
#' #Obentendo as matrizes de dissimilaridade
#' DistQuant=Distancia(DadosQuanti,4)
#' DistQuali=Distancia(DadosQuali,10)
#' Distbin=Distancia(DadosBin,12)
#'
#' #Criando os argumentos
#' Distancias=list(DistQuant,DistQuali,Distbin)
#' n=c(ncol(DadosQuanti),ncol(DadosQuali),ncol(DadosBin))
#'
#' #obentedo a media ponderada das matrizes
#' Dist=MediaDistancia(Distancias,n,Normatizar = TRUE)
#' @export





MediaDistancia=function(Distancias,n,Normatizar=TRUE){



  for(i in 1:length(Distancias)){
    Distancias[[i]]=as.matrix(Distancias[[i]])

  }



  if(Normatizar==TRUE){
    for(i in 1:length(Distancias)){
      Distancias[[i]]=(Normatiza(Distancias[[i]],Metodo = 2))

    }
  }

  for(i in 1:length(Distancias)){
    Distancias[[i]]=Distancias[[i]]*n[i]
  }

  Dist=Distancias[[i]]*0
  for(i in 1:length(Distancias)){
    Dist=Dist+Distancias[[i]]/sum(n)
  }
  as.dist(Dist)
}
