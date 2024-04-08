#' Agrupamento K-means
#'
#' @description Esta funcao retorna clusters pelo metodo kmeans a partir de uma
#' matriz com dados quantitativos.
#' @name Kmeans

#' @usage Kmeans(Dados,design=1,nclusters=4,iter.max = 10,nstart = 1,algorithm = "Hartigan-Wong")
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
#' @param nclusters numero desejado de cluster.
#' @param iter.max numero de iteracoes permitidas.
#' @param nstart numero de conjuntos aleatorios a serem escolhidos.
#' @param algorithm    Metodo desejado para o agrupamento kmeans:
#' \itemize{
#' \item “Hartigan-Wong”
#' \item “Lloyd”
#'  \item “Forgy”
#'  \item “MacQueen”
#'   }
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

#' #Dados sem repeticao
#'  data("Dados.MED")
#'  Dados=Dados.MED
#'  rownames(Dados)=paste("Genotipo",1:10,sep="_")
#'  Kmeans_NumeroOtimo(Dados,design=1,nboot=100,method="silhouette",NumMax=NULL)
#'  Kmeans(Dados,design=1,nclusters=3,iter.max = 10,nstart = 1,
#'         algorithm = "Hartigan-Wong")
#'
#' #Dados de experimento em dic
#' data("Dados.DIC")
#' Kmeans_NumeroOtimo(Dados=Dados.DIC,design=2,nboot=100,method="silhouette",NumMax=NULL)
#' Kmeans(Dados=Dados.DIC,design=2,nclusters=2,iter.max = 20,nstart = 1,
#'        algorithm = "Hartigan-Wong")
#'
#' #Dados de experimento em dbc
#' data("Dados.DBC")
#' Kmeans_NumeroOtimo(Dados=Dados.DBC,design=3,nboot=100,method="silhouette",NumMax=NULL)
#' Kmeans(Dados=Dados.DBC,design=3,nclusters=2,iter.max = 20,nstart = 1,
#'        algorithm = "Hartigan-Wong")
#'
#' #Dados de experimento em DQL
#' data("Dados.DQL")
#' Kmeans_NumeroOtimo(Dados=Dados.DQL,design=4,nboot=100,method="silhouette",NumMax=NULL)
#' Kmeans(Dados=Dados.DQL,design=4,nclusters=2,iter.max = 20,nstart = 1,
#'        algorithm = "Hartigan-Wong")
#'
#' #Dados de experimento em Esquema fatorial em DIC
#' data("Dados.Fat2.DIC")
#' Kmeans_NumeroOtimo(Dados=Dados.Fat2.DIC,design=5,nboot=100,method="silhouette",NumMax=NULL)
#' Kmeans(Dados=Dados.Fat2.DIC,design=5,nclusters=2,iter.max = 20,nstart = 1,
#'        algorithm = "Hartigan-Wong")
#'
#' #Dados de experimento em Esquema fatorial em DBC
#' data("Dados.Fat2.DBC")
#' Kmeans_NumeroOtimo(Dados=Dados.Fat2.DBC,design=6,nboot=100,method="silhouette",NumMax=NULL)
#' Kmeans(Dados=Dados.Fat2.DBC,design=6,nclusters=2,iter.max = 20,nstart = 1,
#'        algorithm = "Hartigan-Wong")
#' }


Kmeans=function(Dados,design=1,nclusters=4,iter.max = 10,nstart = 1,
       algorithm = "Hartigan-Wong"){

  ss <- function(x) sum(scale(x, scale = FALSE)^2)

RES=list()


######################################################
#Sem delineamento
if(design==1){
model=kmeans(x=Dados,centers = nclusters,iter.max =iter.max ,algorithm =algorithm )



RES$model=model

Predito=data.frame(Trat=rownames(Dados),Cluster=model$cluster)
rownames(Predito)=NULL

RES$predict=Predito
RES$Classe=model$cluster
an=AnovaCluster(Cluster=RES$predict[,2],Dados=Dados,design=design)

Explication=sprintf("%0.01f%%",100*unlist(model["betweenss"])/unlist(model["totss"]))
an2=AnovaCluster(Cluster=RES$predict[,2],Dados=scale(Dados,center = T,scale = F),design=design)
sqtrat=sqtot=NULL
for(a in 1:ncol(Dados)){
  sqtrat=c(sqtrat,an2$Anova[[a]][1,2])
  sqtot=c(sqtot,an2$Anova[[a]][1,2]+an2$Anova[[a]][2,2])
}
Explication=sprintf("%0.01f%%",100*sum(sqtrat)/sum(sqtot))




RES$Anova=an$Anova
#RES$Manova=an$MANOVA
RES$Explication=Explication
exp=100*(sqtrat)/(sqtot)
CR=100*exp/sum(exp)

names(CR)=colnames(Dados)
RES$RelativeContribution=CR

}

#########################################################
# DIC
if(design==2){
  Dmed=aggregate(Dados[,-c(1,2)],by = list(Dados[,1]),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dmed=Dmed[,-1]
  model=kmeans(x=Dmed,centers = nclusters,iter.max =iter.max ,algorithm =algorithm )

  TratCluster=model$cluster[Dados[,1]]
  DadosPad=scale(Dados[,-c(1,2)],scale = T)

  RES$model=model

  Predito=data.frame(Trat=rownames(Dmed),Cluster=model$cluster)
  rownames(Predito)=NULL

  RES$predict=Predito
  RES$Classe=model$cluster
  an=AnovaCluster(Cluster=model$cluster,Dados=Dados,design=design)
  an2=AnovaCluster(Cluster=model$cluster,Dados=cbind(Dados[,1:2],scale(Dados[,-c(1,2)],center = T,scale = F)),design=design)
  sqtrat=sqentre=NULL
  for(a in 1:(ncol(Dados)-2)){
    sqtrat=c(sqtrat,as.numeric(an2$Anova[[a]][1,2]))
    n=nrow(an2$Anova[[a]])-1
    sqentre=c(sqentre,as.numeric(an2$Anova[[a]][n,2]))
  }
  Explication=sprintf("%0.01f%%",100*sum(sqentre)/sum(sqtrat))





  RES$Anova=an$Anova
  #RES$Manova=an$MANOVA
  RES$Explication=Explication
  exp=100*(sqentre)/(sqtrat)
  CR=100*exp/sum(exp)
  names(CR)=colnames(Dados)[-c(1:2)]
  RES$RelativeContribution=CR

}

###########################################################
# DBC
if(design==3){
  Dmed=aggregate(Dados[,-c(1,2)],by = list(Dados[,1]),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dmed=Dmed[,-1]
  model=kmeans(x=Dmed,centers = nclusters,iter.max =iter.max ,algorithm =algorithm )

  TratCluster=model$cluster[Dados[,1]]
  DadosPad=scale(Dados[,-c(1,2)],scale = T)

  RES$model=model

  Predito=data.frame(Trat=rownames(Dmed),Cluster=model$cluster)
  rownames(Predito)=NULL

  RES$predict=Predito
  RES$Classe=model$cluster
  an=AnovaCluster(Cluster=model$cluster,Dados=Dados,design=design)
  an2=AnovaCluster(Cluster=model$cluster,Dados=cbind(Dados[,1:2],scale(Dados[,-c(1,2)],center = T,scale = F)),design=design)
  sqtrat=sqentre=NULL
  for(a in 1:(ncol(Dados)-2)){
    sqtrat=c(sqtrat,as.numeric(an2$Anova[[a]][1,2]))
    n=nrow(an2$Anova[[a]])-2
    sqentre=c(sqentre,as.numeric(an2$Anova[[a]][n,2]))
  }
  Explication=sprintf("%0.01f%%",100*sum(sqentre)/sum(sqtrat))





  RES$Anova=an$Anova
  #RES$Manova=an$MANOVA
  RES$Explication=Explication
  exp=100*(sqentre)/(sqtrat)
  CR=100*exp/sum(exp)
  names(CR)=colnames(Dados)[-c(1:2)]
  RES$RelativeContribution=CR

}

###########################################################
# DQL
if(design==4){
  Dmed=aggregate(Dados[,-c(1,2,3)],by = list(Dados[,1]),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dmed=Dmed[,-1]
  model=kmeans(x=Dmed,centers = nclusters,iter.max =iter.max ,algorithm =algorithm )

  TratCluster=model$cluster[Dados[,1]]
  DadosPad=scale(Dados[,-c(1,2)],scale = T)

  RES$model=model

  Predito=data.frame(Trat=rownames(Dmed),Cluster=model$cluster)
  rownames(Predito)=NULL

  RES$predict=Predito
  RES$Classe=model$cluster
  an=AnovaCluster(Cluster=model$cluster,Dados=Dados,design=design)
  an2=AnovaCluster(Cluster=model$cluster,Dados=cbind(Dados[,1:3],scale(Dados[,-c(1,2,3)],center = T,scale = F)),design=design)
  sqtrat=sqentre=NULL
  for(a in 1:(ncol(Dados)-3)){
    sqtrat=c(sqtrat,as.numeric(an2$Anova[[a]][1,2]))
    n=nrow(an2$Anova[[a]])-3
    sqentre=c(sqentre,as.numeric(an2$Anova[[a]][n,2]))
  }
  Explication=sprintf("%0.01f%%",100*sum(sqentre)/sum(sqtrat))





  RES$Anova=an$Anova
  #RES$Manova=an$MANOVA
  RES$Explication=Explication
  exp=100*(sqentre)/(sqtrat)
  CR=100*exp/sum(exp)
  names(CR)=colnames(Dados)[-c(1:3)]
  RES$RelativeContribution=CR

}

###########################################################
# Fatorial em DIC
if(design==5){
  Trat=paste(Dados[,1],Dados[,2],sep=":")
  Dmed=aggregate(Dados[,-c(1,2,3)],by = list(Trat),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dmed=Dmed[,-1]
  model=kmeans(x=Dmed,centers = nclusters,iter.max =iter.max ,algorithm =algorithm )

  TratCluster=model$cluster[Trat]
  DadosPad=scale(Dados[,-c(1,2)],scale = T)

  RES$model=model

  Predito=data.frame(Trat=rownames(Dmed),Cluster=model$cluster)
  rownames(Predito)=NULL

  RES$predict=Predito
  RES$Classe=model$cluster
  Dados2=cbind(Trat,Dados[,-c(1:2)])
  an=AnovaCluster(Cluster=TratCluster,Dados=Dados2,design=2)
  an2=AnovaCluster(Cluster=TratCluster,Dados=cbind(Dados2[,1:2],scale(Dados2[,-c(1,2)],center = T,scale = F)),design=2)
  sqtrat=sqentre=NULL
  for(a in 1:(ncol(Dados2)-2)){
    sqtrat=c(sqtrat,as.numeric(an2$Anova[[a]][1,2]))
    n=nrow(an2$Anova[[a]])-1
    sqentre=c(sqentre,as.numeric(an2$Anova[[a]][n,2]))
  }
  Explication=sprintf("%0.01f%%",100*sum(sqentre)/sum(sqtrat))





  RES$Anova=an$Anova
  #RES$Manova=an$MANOVA
  RES$Explication=Explication
  exp=100*(sqentre)/(sqtrat)
  CR=100*exp/sum(exp)
  names(CR)=colnames(Dados2)[-c(1:2)]
  RES$RelativeContribution=CR

}

###########################################################
# Fatorial em DBC
if(design==6){
  Trat=paste(Dados[,1],Dados[,2],sep=":")
  Dmed=aggregate(Dados[,-c(1,2,3)],by = list(Trat),mean,na.rm=TRUE)
  rownames(Dmed)=Dmed[,1]
  Dmed=Dmed[,-1]
  model=kmeans(x=Dmed,centers = nclusters,iter.max =iter.max ,algorithm =algorithm )

  TratCluster=model$cluster[Trat]
  DadosPad=scale(Dados[,-c(1,2)],scale = T)

  RES$model=model

  Predito=data.frame(Trat=rownames(Dmed),Cluster=model$cluster)
  rownames(Predito)=NULL

  RES$predict=Predito
  RES$Classe=model$cluster
  Dados2=cbind(Trat,Dados[,-c(1:2)])
  an=AnovaCluster(Cluster=TratCluster,Dados=Dados2,design=3)
  an2=AnovaCluster(Cluster=TratCluster,Dados=cbind(Dados2[,1:2],scale(Dados2[,-c(1,2)],center = T,scale = F)),design=3)
  sqtrat=sqentre=NULL
  for(a in 1:(ncol(Dados2)-2)){
    sqtrat=c(sqtrat,as.numeric(an2$Anova[[a]][1,2]))
    n=nrow(an2$Anova[[a]])-2
    sqentre=c(sqentre,as.numeric(an2$Anova[[a]][n,2]))
  }
  Explication=sprintf("%0.01f%%",100*sum(sqentre)/sum(sqtrat))





  RES$Anova=an$Anova
  #RES$Manova=an$MANOVA
  RES$Explication=Explication
  exp=100*(sqentre)/(sqtrat)
  CR=100*exp/sum(exp)
  names(CR)=colnames(Dados2)[-c(1:2)]
  RES$RelativeContribution=CR


}

class(RES)="Kmeans"
p=ContribuicaoRelativa(RES,layout = 5)
#print(p)
return(RES)
}




