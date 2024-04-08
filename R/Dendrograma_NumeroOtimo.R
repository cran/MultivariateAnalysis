#' Determinacao do numero otimo de clusters no dendrograma
#'
#' @description Esta funcao retorna informacoes que auxilia na determinacao do numero de clusters
#' a serem considerados no dendrograma.
#' @name Dendrograma_NumeroOtimo
#'
#' @usage Dendrograma_NumeroOtimo(Dissimilaridade,Metodo=3)
#' @param Dissimilaridade  Objeto criado pela funcao `Distancia`
#' @param Metodo Metodo Valor numerico indicando o metodo a ser utilizado:
#'   \itemize{
#'  \item  1 = Ligacao simples (Metodo do vizinho mais proximo).
#'  \item  2 = Ligacao completa (Metodo do vizinho distante).
#'   \item  3 = Ligacao media entre grupo (UPGMA).
#'   \item  4 = Metodo de Ward.
#'   \item  5 = Metodo de Ward (d2).
#'   \item  6 = Metodo da mediana (WPGMC).
#'   \item  7 = Metodo do centroide (UPGMC).
#'   \item  8 = Metodo mcquitty (WPGMA).
#'   }
#'
#' @return A funcao retorna o numero otimo de Clusters.
#' @seealso \code{\link{Kmeans}}, \code{\link{Tocher}} , \code{\link{Dendrograma}}
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
#'

#' @examples
#' data(Dados.MED)
#' rownames(Dados.MED)=paste0("Treatment ",1:10)
#' Dist=Distancia(Dados.MED,Metodo=3)
#' Dendrograma_NumeroOtimo(Dissimilaridade = Dist,Metodo = 3)
#'
#' data("Dados.CAT")
#' Diss=Distancia(Dados = Dados.CAT,Metodo=10)
#' Dendrograma_NumeroOtimo(Dissimilaridade = Diss,Metodo = 5)
#' @export


Dendrograma_NumeroOtimo=function(Dissimilaridade,Metodo=3){
  Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )
  diss=Dissimilaridade
  if(is(diss)=="Distancia"){diss=diss$Distancia}

  Arvore=hclust(diss,Met[Metodo])
  fusao=Arvore$height
  MojenaCorte=c(`k=1.25`=mean(fusao)+sd(fusao)*1.25,`k=2`=mean(fusao)+sd(fusao)*2)





sink(nullfile <- tempfile(), type = "output")
nc1=NbClust(diss=diss,distance =NULL,
             max.nc=ncol(as.matrix(diss))-1,
             method = Met[Metodo], index = "cindex")
sink()
file.remove(nullfile)

sink(nullfile <- tempfile(), type = "output")
nc2=NbClust(diss=diss,distance =NULL,
        max.nc=ncol(as.matrix(diss))-2,
        method = Met[Metodo], index = "frey")
sink()
file.remove(nullfile)


sink(nullfile <- tempfile(), type = "output")
nc3=NbClust(diss=diss,distance =NULL,
        max.nc=ncol(as.matrix(diss))-2,
        method = Met[Metodo], index = "mcclain")
sink()
file.remove(nullfile)


sink(nullfile <- tempfile(), type = "output")
nc4=NbClust(diss=diss,distance =NULL, max.nc=ncol(as.matrix(diss))-1,
        method = Met[Metodo], index = "dunn")
sink()
file.remove(nullfile)




Indices=data.frame(rbind(
  Cindex=nc1$All.index,
Frey=c(nc2$All.index,""),
Mcclain=c(nc3$All.index,""),
Dunn=nc4$All.index))
colnames(Indices)=1:ncol(Indices)



gm1=cutree(Arvore,h = MojenaCorte[1])
gm2=cutree(Arvore,h = MojenaCorte[2])

Number_clusters=data.frame(rbind(
c(Number_clusters=length(unique(gm1)),Value_Index= MojenaCorte[1] ),
c(Number_clusters=length(unique(gm2)),Value_Index= MojenaCorte[2] ),
nc1$Best.nc,
nc2$Best.nc,
nc3$Best.nc,
nc4$Best.nc))
rownames(Number_clusters )=c("Mojena1","Mojena2","Cindex","Frey","Mcclain","Dunn")
colnames(Number_clusters )=c("Number_clusters" ,"Value_Index")

Best.partition=data.frame(rbind(
  gm1,
  gm2,
  nc1$Best.partition,
  nc2$Best.partition,
  nc3$Best.partition,
  nc4$Best.partition))
rownames(Best.partition )=c("Mojena1","Mojena2","Cindex","Frey","Mcclain","Dunn")


  Resultado=list()
  Resultado$Index=Indices
  Resultado$Number_clusters=Number_clusters
  Resultado$Best.partition=t(Best.partition)

  return(Resultado)

  }



