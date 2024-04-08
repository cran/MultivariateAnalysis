#' Dendrograma
#' @aliases Dendrograma
#' @description Esta funcao retorna a figura do Dendrograma, distancias feneticas
#'   e correlacao cofenetica.
#' @name Dendrograma
#' @usage Dendrograma(Dissimilaridade,
#'                   Metodo=3,
#'                   layout=2,
#'                   nperm=999,
#'                   Titulo="",
#'                   corte="Mojena1")
#' @param Dissimilaridade    Matriz contendo a estimativa das distancias entre
#'   tratamentos.
#' @param Metodo Valor numerico indicando o metodo a ser utilizado:
#'   \itemize{
#'   \item  1 = Ligacao simples (Metodo do vizinho mais proximo).
#'   \item  2 = Ligacao completa (Metodo do vizinho distante).
#'   \item  3 = Ligacao media entre grupo (UPGMA).
#'   \item  4 = Metodo de Ward.
#'   \item  5 = Metodo de Ward (d2).
#'   \item  6 = Metodo da mediana (WPGMC).
#'   \item  7 = Metodo do centroide (UPGMC).
#'   \item  8 = Metodo mcquitty (WPGMA).
#'   }
#' @param layout Deve ser um numero variando de 1 a 10. Para cada numero teremos
#' um layout diferente.
#' @param nperm Numero de permutacoes do teste mantel para testar a significancia
#' pelo teste Mantel.
#' @param Titulo Texto com o titulo a ser apresentado no grafico
#' @param corte Indica a metodologia considerada para estabelecer a linha de
#' corte no Dendrograma:
#' \itemize{
#'   \item FALSE = Indica que o ponto de corte nao sera apresentado.
#'   \item Mojena1 = Coeficiente de mojena considerando o K=1.25.
#'   \item Mojena2 = Coeficiente de mojena considerando o K=2.00.
#'   \item Cindex = Considera o metodo Cindex para determinar o numero de clusters.
#'   \item Frey = Considera o metodo Frey para determinar o numero de clusters.
#'   \item Mcclain = Considera o metodo Frey para determinar o numero de clusters.
#'   \item Dunn = Considera o metodo Dunn para determinar o numero de clusters.
#'   \item Numeric= Valor numerico indicando onde se deseja plotar a linhade corte.
#' }
#' @return A funcao retorna o Dendrograma, distancias feneticas e correlacao
#'   cofenetica.
#'
#' @seealso \code{\link{hclust}}, \code{\link{dist}}
#'
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.
#'     (ISBN: 8572691510)
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p.
#'  (ISBN 13:978 8581270630)
#'
#'  HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978 0138132637)
#'
#'   MOJENA, R. Hierarquical grouping method and stopping rules: an evaluation.
#'    Computer Journal, v.20, p.359-363, 1977.

#' @examples
#'  \donttest{
#' data(Dados.MED)
#' rownames(Dados.MED)=paste0("Treatment ",1:10)
#' Dist=Distancia(Dados.MED,2)
#' Dendrograma(Dist,Metodo = 2)
#' Dendrograma(Dist,2,layout =1,corte = "Mojena2")
#' Dendrograma(Dist,2,layout =2,corte="Cindex")
#' Dendrograma(Dist,2,layout =3,corte="Frey")
#' Dendrograma(Dist,2,layout =4,corte="Mcclain")
#' Dendrograma(Dist,2,layout =5,corte="Dunn")
#' Dendrograma(Dist,2,layout =6)
#' Dendrograma(Dist,2,layout =7,corte=3)
#' Dendrograma(Dist,2,layout =8)
#' Dendrograma(Dist,2,layout =9)
#' Dendrograma(Dist,2,layout =10,corte=2)
#' }
#'
#' @export
#' @exportS3Method print Dendrograma
Dendrograma=function(Dissimilaridade,
                    Metodo=3,
                    layout=2,
                    nperm=999,
                    Titulo="",
                    corte="Mojena1")

{

  Dissimilaridade0=Dissimilaridade
  if(is(Dissimilaridade)=="Distancia"){Dissimilaridade=Dissimilaridade$Distancia}

############################################################
Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )
Dissimilaridade=as.dist(Dissimilaridade)

if(isFALSE((corte==F)|
           (corte=="Mojena1")|
           (corte=="Mojena2")|
           (corte=="Cindex")|
           (corte=="Frey")|
           (corte=="Mcclain")|
           (corte=="Dunn")|
           is.numeric(corte))){
  warning("O objeto 'corte' deve conter um valor numerico, um criterio ou FALSE")
}

Arvore=hclust(Dissimilaridade,Met[Metodo])

DistanciaFenetica=cophenetic(Arvore)
CorrelacaoCofenetica=cor(Dissimilaridade,DistanciaFenetica)


Mantel=CorrelacaoMantel(Dissimilaridade,DistanciaFenetica,nperm = nperm,Plot   = FALSE)
fusao=Arvore$height
MojenaCorte=c(`k=1.25`=mean(fusao)+sd(fusao)*1.25,`k=2`=mean(fusao)+sd(fusao)*2)
if(is.numeric(corte)){
    nc=list();nc$Best.partition=cutree(Arvore,h=corte)
}


if(corte=="Mojena1"){
  corte=nc=MojenaCorte[1]
  #lines(c(0,length(fusao+2)),c(corte,corte),col=2,lty=2)

  nc=list();nc$Best.partition=cutree(Arvore,h=corte)
}
if(corte=="Mojena2"){
  corte=nc=MojenaCorte[2]
  #lines(c(0,length(fusao+2)),c(corte,corte),col=2,lty=2)
  nc=list();nc$Best.partition=cutree(Arvore,h=corte)
}

if(corte=="Cindex"){
  sink(nullfile <- tempfile(), type = "output")
  nc=try(NbClust(diss=Dissimilaridade,distance =NULL,
          max.nc=ncol(as.matrix(Dissimilaridade))-1,
          method = Met[Metodo], index = "cindex"),silent=T)
  if(is(nc)[1]=="try-error"){nc=list();nc$Best.partition=rep(1,ncol(as.matrix(Dissimilaridade)))}

  sink()
  file.remove(nullfile)

  ngrupos=length(unique(nc$Best.partition))
  minDist=min(Dissimilaridade)
  maxDist=max(Dissimilaridade)
  SeqDist=seq(minDist,maxDist,l=100)
  ngrupos2=iii=0
  while(ngrupos != ngrupos2){
    iii=iii+1
    ngrupos2=length(unique(cutree(Arvore,h =SeqDist[iii] )))
    corte=SeqDist[iii]
  }

  #lines(c(0,length(fusao+2)),c(corte,corte),col=2,lty=2)
}

if(corte=="Frey"){
  sink(nullfile <- tempfile(), type = "output")
  nc=try(NbClust(diss=Dissimilaridade,distance =NULL,
          max.nc=ncol(as.matrix(Dissimilaridade))-2,
          method = Met[Metodo], index = "frey"),silent = T)
  sink()
  if(is(nc)[1]=="try-error"){nc=list();nc$Best.partition=rep(1,ncol(as.matrix(Dissimilaridade)))}
  file.remove(nullfile)
  ngrupos=length(unique(nc$Best.partition))
  minDist=min(Dissimilaridade)
  maxDist=max(Dissimilaridade)
  SeqDist=seq(minDist,maxDist,l=100)
  ngrupos2=iii=0
  while(ngrupos != ngrupos2){
    iii=iii+1
    ngrupos2=length(unique(cutree(Arvore,h =SeqDist[iii] )))
    corte=SeqDist[iii]
  }

  #lines(c(0,length(fusao+2)),c(corte,corte),col=2,lty=2)
}

if(corte=="Mcclain"){
  sink(nullfile <- tempfile(), type = "output")
 nc= try(NbClust(diss=Dissimilaridade,distance =NULL,
          max.nc=ncol(as.matrix(Dissimilaridade))-2,
          method = Met[Metodo], index = "mcclain"),silent = T)
 if(is(nc)[1]=="try-error"){nc=list();nc$Best.partition=rep(1,ncol(as.matrix(Dissimilaridade)))}

 sink()
 file.remove(nullfile)
 ngrupos=length(unique(nc$Best.partition))
 minDist=min(Dissimilaridade)
 maxDist=max(Dissimilaridade)
 SeqDist=seq(minDist,maxDist,l=100)
 ngrupos2=iii=0
 while(ngrupos != ngrupos2){
   iii=iii+1
   ngrupos2=length(unique(cutree(Arvore,h =SeqDist[iii] )))
   corte=SeqDist[iii]
 }


  #lines(c(0,length(fusao+2)),c(corte,corte),col=2,lty=2)
}

if(corte=="Dunn"){
  sink(nullfile <- tempfile(), type = "output")
 nc= try(NbClust(diss=Dissimilaridade,distance =NULL, max.nc=ncol(as.matrix(Dissimilaridade))-1,
          method = Met[Metodo], index = "dunn"),silent=T)
 if(is(nc)[1]=="try-error"){nc=list();nc$Best.partition=rep(1,ncol(as.matrix(Dissimilaridade)))}

 sink()
 file.remove(nullfile)
 ngrupos=length(unique(nc$Best.partition))
 minDist=min(Dissimilaridade)
 maxDist=max(Dissimilaridade)
 SeqDist=seq(minDist,maxDist,l=100)
 ngrupos2=iii=0
 while(ngrupos != ngrupos2){
   iii=iii+1
   ngrupos2=length(unique(cutree(Arvore,h =SeqDist[iii] )))
   corte=SeqDist[iii]
 }

  #lines(c(0,length(fusao+2)),c(corte,corte),col=2,lty=2)
}




if(layout==1){
plot(Arvore,hang=-1,main=Titulo,xlab="",sub ="" )


if(is.numeric(corte)){
  lines(c(0,length(fusao)+2),c(corte,corte),col=2,lty=2)
}
}

if(layout==2){
  p=ggdendrogram(Arvore,theme_dendro =FALSE,rotate = TRUE)+
    labs(x="Dendrogram", y = "Distance")+
    theme(panel.background = element_rect(fill = "white"))
  if(is.numeric(corte)){
    p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
  }

  print(p)
}

if(layout==3){
 p= ggdendrogram(Arvore,theme_dendro =FALSE,rotate = FALSE)+
   labs(x="Dendrogram", y = "Distance")+
   theme(panel.background = element_rect(fill = "white"))
 if(is.numeric(corte)){
   p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
 }
 print(p)
}

x=0
y=corte
xend= length(fusao)+2
yend=corte
label=NULL

if(layout==4){
  dend_data <- dendro_data(Arvore, type = "rectangle")
  label=dend_data$labels$label
 p= ggplot(segment(dend_data)) +
   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
   coord_flip() +
   scale_y_reverse(expand = c(0.2, 0)) +
    theme_dendro() +
   geom_text(data = dend_data$labels, aes(x, y, label = label),
              hjust = 0, angle = 0, size = 3)+
   labs(x="Dendrogram", y = "Distance")
 if(is.numeric(corte)){
   p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
 }
 print(p)
}

if(layout==5){
  dend_data <- dendro_data(Arvore, type = "triangle")
  label=dend_data$labels$label
  p=ggplot(segment(dend_data)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    coord_flip() +
    scale_y_reverse(expand = c(0.2, 0)) +
    theme_dendro() +
    geom_text(data = dend_data$labels, aes(x, y, label = label),
              hjust = 0, angle = 0, size = 3)+
    labs(x="Dendrogram", y = "Distance")
  if(is.numeric(corte)){
    p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
  }
  print(p)
}

if(layout==6){
  dend_data <- dendro_data(Arvore, type = "triangle")
  label=dend_data$labels$label
  p=ggplot(segment(dend_data)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    coord_flip() +
    scale_y_reverse(expand = c(0.2, 0)) +
   # theme_dendro() +
    theme(axis.ticks.y=element_blank(),
          axis.text.y = element_blank())+
    geom_text(data = dend_data$labels, aes(x, y, label = label),
              hjust = 0, angle = 0, size = 3)+
    labs(x="Dendrogram", y = "Distance") +
    theme(panel.background = element_rect(fill = "white"))

  if(is.numeric(corte)){
    p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
  }
  print(p)
}

if(layout==7){

  if(!is.numeric(corte)){
    corte=max(Dissimilaridade)+10
  }
  dend_data <-dendro_data_k(Arvore,h = corte)
  p=plot_ggdendro(dend_data,
                direction   = "lr",
                expand.y    = 0.2)+labs(x="Dendrogram", y = "Distance")+

    theme(panel.background = element_rect(fill = "white"))
  if(is.numeric(corte)){
    p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
  }
  print(p)
  }

if(layout==8){
  if(!is.numeric(corte)){
    corte=max(Dissimilaridade)+10
  }
  dend_data <-dendro_data_k(Arvore,h = corte)
  p=plot_ggdendro(dend_data,
                direction   = "tb",
                expand.y    = 0)+labs(x="Dendrogram", y = "Distance")+
    theme(panel.background = element_rect(fill = "white"))
  if(is.numeric(corte)){
    p=p+geom_hline(yintercept = corte, linetype = "dashed", color = "red")
  }
  print(p)
}

if(layout==9){

  if(!is.numeric(corte)){
    corte=max(Dissimilaridade)+10
  }
  dend_data <-dendro_data_k(Arvore,h = corte)

  p = plot_ggdendro(dend_data,
                    fan         = TRUE,
                    #scale.color = cols+1,
                    label.size  = 4,
                    nudge.label = 0.02,
                    expand.y    = 0.4)+
    theme_void() +
    theme(panel.background = element_rect(fill = "white") )


  print(p)


}

if(layout==10){

  if(!is.numeric(corte)){
    corte=max(Dissimilaridade)+10
  }

  dend_data <-dendro_data_k(Arvore,h = corte)

  p = plot_ggdendro(dend_data,
                     fan         = TRUE,
                     #scale.color = cols+1,
                     label.size  = 4,
                     nudge.label = 0.02,
                     expand.y    = 0.4)+
    theme_void() +
    theme(panel.background = element_rect(fill = "black"))


  print(p)


}

  resultado=list(#call = match.call()
  Distancia=Dissimilaridade0,
  MetodoDendo=Metodo,
  PontosFusao=fusao,
  DistanciaFenetica=DistanciaFenetica,
  CorrelacaoCofenetica=CorrelacaoCofenetica,
  SigCorrelCofenetica=Mantel,
  CiterioCorte=corte,
  Classe=nc$Best.partition,
  Ordem=Arvore$order


  )
  class(resultado)="Dendrograma"


#print.Dendrograma(resultado)
return(resultado)
  }



print.Dendrograma = function (x, ...){
  cat("_________________________________________________________________________","\n")
  cat("Estimativa de correlacao cofenetica:","\n")
  print(x$CorrelacaoCofenetica)
  cat("Significancia da correlacao cofenetica pelo teste Mantel","\n")
  cat("pvalor:",x$SigCorrelCofenetica$p.value,"\n")
  cat("Hipotese alternativa: A correlacao e maior que 0","\n")
  cat("\n")
  cat("Criterio de Corte","\n")
  print(x$CiterioCorte)
  cat("\n")
  cat("Agrupamentos","\n")
  print(data.frame(Cluster=x$Classe))

  cat("_________________________________________________________________________","\n")

}
