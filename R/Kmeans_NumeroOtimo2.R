#' Numero otimo de clusters pelo metodo do cotovelo
#'
#' @description Esta funcao retorna o numero otimo de clusters para o metodo kmeans
#'  considerando dieferentes criterios.
#'  @name Kmeans_NumeroOtimo2

#' @usage Kmeans_NumeroOtimo2(Dados,design=1,nboot=100, Metodo=1,iter.max = 100,
#' NumMax=NULL,TituloX="Numero de clusters",TituloY=NULL,Theme=theme_classic())

#' @param Dados Matriz contendo os dados para execucao da analise. Para cada
#'  modelo o conjunto de dados precisa estar organizado de uma forma apropriada:
#'  \itemize{ \item Design 1: Deve ter apenas os dados numericos da pesquisa.
#'   Na primeira linha não deve ter o nome dos individuos/tratamentos.
#'    \item Design 2 e 3: As duas primeiras colunas devem conter a
#'  identificacao dos tratamentos e repeticoes/blocos, e as demais os valores
#'  observanos nas variaveis respostas. \item Modelo 4: As tres primeiras
#'  colunas devem conter as informacoes dos tratamentos, linhas e colunas, e
#'  posteriormente, os valores da variavel resposta. \item Modelos 5 e 6: as
#'  primeiras colunas precisam ter a informacao do fator A, fator B,
#'  repeticao/bloco, e posteriormente, as variaveis respostas.}
#' @param design    Valor numerico indicando o delineamento:
#' \itemize{
#' \item 1 =  Experimento sem repeticoes.
#' \item 2 =  Delineamento inteiramente casualizado (DIC) .
#'  \item 3 = Delineamento em blocos casualizados (DBC).
#'  \item 4 = Delineamento em quadrado latino (DQL).
#'   \item 5 =Esquema fatorial duplo em DIC.
#'   \item 6 = Esquema fatorial duplo em DBC.}
#' @param nboot numero de reamostragens desejadas para o metodo bootstrap.
#' @param Metodo Criterio utilizado para a estimacao do numero otimo de clusters.
#' Pode-se utilizar as seguintes opcoes:
#' \itemize{
#' \item  1 = Metodo baseado na porcentagem de explicacao
#' \item 2 = Metodo baseado na soma dos quadrados intra-cluster
#'  \item 3 =  = Metodo baseado na soma dos quadrados inter-cluster
#' }
#'
#' @param iter.max numero de reamostragens desejadas para o metodo bootstrap.
#' @param NumMax Numero maximo de clustes a ser considerado (Obs: Deve ser no minimo 2).
#' @param TituloX Titulo desejado para o eixo x.
#' @param TituloY Titulo desejado para o eixo y.
#' @param Theme Tema utilizado para o graficos do `ggplot2` (Ex.:theme_gray(),
#' theme_bw(), theme_linedraw(), theme_light(), theme_dark(), theme_minimal(),
#' theme_classic(), theme_void(), theme_test()).
#' @return A funcao retorna o numero otimo de clusters a ser considerado no metodo kmeans.
#' @seealso \code{\link{Kmeans}}, \code{\link{Kmeans_NumeroOtimo2}} , \code{\link{ContribuicaoRelativa}}
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
#' @export
#' @examples
#'  \donttest{

#' #Sem repeticoes
#' data("Dados.MED")
#' Dados=Dados.MED
#' rownames(Dados)=paste("Genotipo",1:10,sep="_")
#' Kmeans_NumeroOtimo2(Dados,design=1,nboot=100,iter.max = 100,NumMax=NULL)
#'
#' #Experimento em DIC
#' data("Dados.DIC")
#' Dados=Dados.DIC
#' Kmeans_NumeroOtimo2(Dados,design=2,Metodo = 2,nboot=100,iter.max = 100,NumMax=NULL)
#'
#' #Experimento em DBC
#' data("Dados.DBC")
#' Dados=Dados.DBC
#' Kmeans_NumeroOtimo2(Dados,design=3,nboot=100,iter.max = 100,NumMax=NULL)
#'
#' #Experimento em DQL
#' data("Dados.DQL")
#' Dados=Dados.DQL
#' Kmeans_NumeroOtimo2(Dados,design=4,nboot=100,iter.max = 100,NumMax=NULL)
#'
#' #Experimento em fatorial no DIC
#' data("Dados.Fat2.DIC")
#' Dados=Dados.Fat2.DIC
#' Kmeans_NumeroOtimo2(Dados,design=5,Metodo = 1,nboot=100,iter.max = 100,NumMax=NULL)
#' Kmeans_NumeroOtimo2(Dados,design=5,Metodo =2,nboot=100,iter.max = 100,NumMax=NULL)
#' Kmeans_NumeroOtimo2(Dados,design=5,Metodo =3,nboot=100,iter.max = 100,NumMax=NULL)
#'
#' #Experimento em fatorial no DBC
#' data("Dados.Fat2.DBC")
#' Dados=Dados.Fat2.DBC
#' Kmeans_NumeroOtimo2(Dados,design=5,Metodo = 1,nboot=100,iter.max = 100,NumMax=NULL)
#' Kmeans_NumeroOtimo2(Dados,design=5,Metodo =2,nboot=100,iter.max = 100,NumMax=NULL)
#' Kmeans_NumeroOtimo2(Dados,design=5,Metodo =3,nboot=100,iter.max = 100,NumMax=NULL)
#' }




Kmeans_NumeroOtimo2=function(Dados,design=1,nboot=100, Metodo=1,iter.max = 100,NumMax=NULL,
                    TituloX="Numero de clusters",TituloY=NULL,Theme=theme_classic()){


#Media
if(design==1){Dados=Dados}
#DIC
if(design==2){
  Dmed=aggregate(Dados[,-c(1,2)],by = list(Dados[,1]),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dados=Dmed[,-1]}
#DBC
if(design==3){
  Dmed=aggregate(Dados[,-c(1,2)],by = list(Dados[,1]),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dados=Dmed[,-1]
}
#DQL
if(design==4){
  Dmed=aggregate(Dados[,-c(1,2,3)],by = list(Dados[,1]),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dados=Dmed[,-1]}
#Fat dic
if(design==5){
  Trat=paste(Dados[,1],Dados[,2],sep="_")
  Dmed=aggregate(Dados[,-c(1,2,3)],by = list(Trat),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dados=Dmed[,-1]}
#Fat dbc
if(design==6){
  Trat=paste(Dados[,1],Dados[,2],sep="_")
  Dmed=aggregate(Dados[,-c(1,2,3)],by = list(Trat),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dados=Dmed[,-1]}


  if(is.null(NumMax)){NumMax=(nrow(Dados)-1)}
  ncluster2=Exp2=totss2=tot.withinss2=betweenss2=NULL
  for(i in 1:nboot){
  ncluster=Exp=totss=tot.withinss=betweenss=NULL
  for(i in 2:NumMax){
  km= kmeans(x =Dados,centers = i,iter.max = iter.max)

  ncluster=c(ncluster,length(unique(km$cluster)))
  Exp=c(Exp,100*km$betweenss/  km$totss)
  totss=c(totss,km$totss)
  tot.withinss=c(tot.withinss,km$tot.withinss)
  betweenss=c(betweenss,km$betweenss)

  }
    ncluster2=rbind(ncluster2,ncluster)
    Exp2=rbind(Exp2,Exp)
    totss2=rbind(totss2,totss)
    tot.withinss2=rbind(tot.withinss2,tot.withinss)
    betweenss2=rbind(betweenss2,betweenss)

  }


  if(Metodo==1){
    if(is.null(TituloY)){TituloY="Explication (%)"}
    Exp3=apply(Exp2, 2,mean)
  }
  if(Metodo==2){
    if(is.null(TituloY)){TituloY="Soma dos quadrados intra-cluster"}
    Exp3=apply(tot.withinss2, 2,mean)
  }
  if(Metodo==3){
    if(is.null(TituloY)){TituloY="Soma dos quadrados inter-cluster"}
    Exp3=apply(betweenss2, 2,mean)
  }





    x1=2
    y1 =Exp3[1]
    x2= NumMax
    y2 = Exp3[length(Exp3)]

    distances = NULL
    for (i in 1:length(Exp3)){
    x0 = i+1
    y0 = Exp3[i]
    numerator = abs((y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1)
    denominator = sqrt((y2 - y1)**2 + (x2 - x1)^2)
    distances=c(distances,numerator/denominator)
    }
    ng=(2:NumMax)[distances==max(distances)]

x=y=""

    dad=data.frame(x=2:NumMax,
                   y=Exp3)
   p= ggplot(dad, aes(x = x, y = y)) +
      geom_point() +
      labs(x = TituloX,
           y = TituloY)+
     geom_line(color = "blue")+
     ggplot2::geom_vline(xintercept =ng, linetype = "dashed", color = "red")+
     Theme


  print(p)
  Res=NULL
  Res$Explication=Exp2
  Res$tot.withinss=tot.withinss2
  Res$betweenss=betweenss2
  Res$ClusterNumber=ng[length(ng)]

  return(Res)
}
