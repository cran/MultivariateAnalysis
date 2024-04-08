#'Analise de variancia considerando clusters
#'
#' @description Esta funcao retorna o resultado da analise de variancia
#'  considerando clusters formados por diferentes metodos como dendrograma, kmeans,
#'  Tocher, etc.
#' @name AnovaCluster
#' @usage AnovaCluster(Cluster,Dados=Dados,design=design,test="Pillai")
#' @param Cluster  Vetor contendo os grupos que cada individuo/tratamento pertence. Veja os exemplos.
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
#' @param test    Nome do teste que se deseja utilizar na manova ("Pillai", "Wilks", "Hotelling-Lawley" ou "Roy").
#' @return A funcao retorna a ANOVA, para todas as variaveis considerando os clustes criados.
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
#' @export



#' @examples
#' \donttest{
#' #######################################################
#' #######################################################
#' #Dados sem repeticoes considerando o Kmeans
#' data(Dados.MED)
#' Km=Kmeans(Dados = Dados.MED,design = 1,nclusters = 4)
#' AnovaCluster(Cluster = Km$Classe,Dados=Dados.MED,design = 1)
#'
#' #Dados sem repeticoes considerando o Tocher
#' dist=Distancia(Dados = Dados.MED,Metodo = 3)
#' TO=Tocher(Dist = dist)
#' TO
#' AnovaCluster(Cluster = TO$Classe,Dados=Dados.MED,design = 1)
#'
#' #Dados sem repeticoes considerando o Dendrograma
#' Dendro=Dendrograma(Dissimilaridade = dist,corte="Frey")
#' Dendro
#' AnovaCluster(Cluster = Dendro$Classe,Dados=Dados.MED,design = 1)
#' #######################################################################
#' #######################################################################
#' #DIC considerando o Kmeans
#' data(Dados.DIC)
#' N=Kmeans_NumeroOtimo2(Dados = Dados.DIC,design = 2)
#' Km=Kmeans(Dados = Dados.DIC,design = 3,nclusters = N$ClusterNumber)
#' Km$predict
#' AnovaCluster(Cluster = Km$Classe,Dados=Dados.DIC,design = 2)
#'
#' #DIC considerando o Tocher
#' m=MANOVA(Dados.DIC,Modelo=1)
#' dist=Distancia(Dados = m$Med,Metodo = 3)
#' TO=Tocher(Dist = dist)
#' TO
#' AnovaCluster(Cluster = TO$Classe,Dados=Dados.DIC,design = 2)
#'
#' #DIC considerando o Dendrograma
#' Dendro=Dendrograma(Dissimilaridade = dist,corte="Frey")
#' Dendro
#' AnovaCluster(Cluster = Dendro$Classe,Dados=Dados.DIC,design = 2)
#'
#' #######################################################################
#' #######################################################################
#' #DBC considerando o Kmeans
#' data(Dados.DBC)
#' n=Kmeans_NumeroOtimo2(Dados = Dados.DBC,design = 3)
#' Km=Kmeans(Dados = Dados.DBC,design = 3,nclusters = n$ClusterNumber)
#' Km$predict
#' AnovaCluster(Cluster = Km$Classe,Dados=Dados.DBC,design = 3)
#'
#' #DBC considerando o Tocher
#' m=MANOVA(Dados.DBC,Modelo=2)
#' dist=Distancia(Dados = m$Med,Metodo = 3)
#' TO=Tocher(Dist = dist)
#' dist
#' TO
#' AnovaCluster(Cluster = TO$Classe,Dados=Dados.DBC,design = 3)
#'
#' #DBC considerando o Dendrograma
#' Dendro=Dendrograma(Dissimilaridade = dist,corte="Mcclain")
#' Dendro$Classe
#' AnovaCluster(Cluster = Dendro$Classe,Dados=Dados.DBC,design = 3)
#'
#'
#' #######################################################################
#' #######################################################################
#' #DQL considerando o Kmeans
#' data(Dados.DQL)
#' n=Kmeans_NumeroOtimo2(Dados = Dados.DQL,design = 4)
#' Km=Kmeans(Dados = Dados.DQL,design = 4,nclusters = n$ClusterNumber)
#' Km$predict
#' AnovaCluster(Cluster = Km$Classe,Dados=Dados.DQL,design = 4)
#'
#' #DQL considerando o Tocher
#' m=MANOVA(Dados.DQL,Modelo=2)
#' dist=Distancia(Dados = m$Med,Metodo = 3)
#' dist
#' TO=Tocher(Dist = dist)
#' TO
#' AnovaCluster(Cluster = TO$Classe,Dados=Dados.DQL,design = 4)
#'
#' #DQL considerando o Dendrograma
#' Dendro=Dendrograma(Dissimilaridade = dist,corte="Dunn")
#' Dendro$Classe
#' AnovaCluster(Cluster = Dendro$Classe,Dados=Dados.DQL,design = 4)
#'
#'
#'
#' #######################################################################
#' #######################################################################
#' #Fat duplo em dic considerando o Kmeans
#' data("Dados.Fat2.DIC")
#' n=Kmeans_NumeroOtimo2(Dados = Dados.Fat2.DIC,design = 5)
#' Km=Kmeans(Dados = Dados.Fat2.DIC,design = 5,nclusters = n$ClusterNumber)
#' Km$predict
#' AnovaCluster(Cluster = Km$Classe,Dados=Dados.Fat2.DIC,design = 5)
#'
#' #Fat2.DIC considerando o Tocher
#' m=MANOVA(Dados.Fat2.DIC,Modelo=4)
#' dist=Distancia(Dados = m$Med,Metodo = 3)
#' TO=Tocher(Dist = dist)
#' TO$Classe
#' AnovaCluster(Cluster = TO$Classe,Dados=Dados.Fat2.DIC,design = 5)
#'
#' #Fat2.DIC considerando o Dendrograma
#' Dendro=Dendrograma(Dissimilaridade = dist,corte="Dunn")
#' Dendro$Classe
#' AnovaCluster(Cluster = Dendro$Classe,Dados=Dados.Fat2.DIC,design = 5)
#'
#'
#'
#' #######################################################################
#' #######################################################################
#' #Fat duplo em dbc considerando o Kmeans
#' data("Dados.Fat2.DBC")
#' n=Kmeans_NumeroOtimo2(Dados = Dados.Fat2.DBC,design = 6)
#' Km=Kmeans(Dados = Dados.Fat2.DBC,design = 5,nclusters = n$ClusterNumber)
#' Km$predict
#' AnovaCluster(Cluster = Km$Classe,Dados=Dados.Fat2.DBC,design = 5)
#'
#' #Fat2.DBC considerando o Tocher
#' m=MANOVA(Dados.Fat2.DBC,Modelo=5)
#' dist=Distancia(Dados = m$Med,Metodo = 3)
#' TO=Tocher(Dist = dist)
#' TO$Classe
#' AnovaCluster(Cluster = TO$Classe,Dados=Dados.Fat2.DBC,design = 5)
#'
#' #Fat2.DBC considerando o Dendrograma
#' Dendro=Dendrograma(Dissimilaridade = dist,corte="Cindex")
#' Dendro$Classe
#' AnovaCluster(Cluster = Dendro$Classe,Dados=Dados.Fat2.DBC,design = 6)
#'
#' }





AnovaCluster=function(Cluster,Dados=Dados,design=design,test="Pillai"){
#  Cluster=Between_Clusters=as.factor(Cluster)
 if(design==1){Cluster=Between_Clusters=as.factor(Cluster)}
if((design==2)|(design==3)|(design==4)){

    Cluster=Between_Clusters=as.factor( Cluster[Dados[,1]])}
  if((design==5)|(design==6)){Trat=paste(Dados[,1],Dados[,2],sep=":");Cluster=Between_Clusters=as.factor( Cluster[Trat])}

  if(design==5){
    Dados=cbind(Trat,Dados[,-c(1,2)])
    design=2
  }
  if(design==6){
    Dados=cbind(Trat,Dados[,-c(1,2)])
    design=3
  }
  #SemRep
  if(design==1){
    Anova.res=list()
    for( i in 1:ncol(Dados)){
    m=aov(Dados[,i]~Between_Clusters)
   # Fcalc=anova(m)[[1,4]]
   # pValue=round(anova(m)[[1,5]],5)
    #Sig=ifelse(anova(m)[[1,5]]>0.05,"ns",ifelse(anova(m)[[1,5]]>0.01,"*","**"))
    #Anova.res=rbind(Anova.res,c(Fcalc,pValue,Sig))
    Anova.res[[i]]=anova(m)

    }
    #rownames(Anova.res)=colnames(Dados)
    #colnames(Anova.res)=c("Fcalc","pValue","Sig")
    #Anova.res=as.data.frame(Anova.res)

    m=lm(as.matrix(Dados[,-c(8)])~Between_Clusters)
    MANOVA=try(anova(m,test = test),silent = TRUE)
    if(is(MANOVA)[1]=="try-error"){MANOVA="Nao e possivel fazer a MANOVA, pois o numero de variaveis e maior que os graus de liberdade do residuo."}

  }
  #DIC
  if(design==2){
    Anova.res=list()
    for( i in 3:ncol(Dados)){
      ####################################################
      m=aov(Dados[,i]~as.factor(Dados[,1]))
      trat=c(anova(m)[[1,1]],anova(m)[[1,2]])
      resid=c(anova(m)[[2,1]],anova(m)[[2,2]],anova(m)[[2,3]])
      dentro=nomes=NULL
      for(j in 1:length(unique(Cluster))){
        Dados2=Dados[Cluster==j,]
        if(length(unique(as.character(Dados2[,1])))>1){
        dentro=rbind(dentro,sq(Dados2[,1],Dados2[,i]))
        nomes=c(nomes,paste("Within_Cluster",j))
        }
      }
      rownames(dentro)=nomes
      Entre=sq(Cluster,Dados[,i])

      P1=rbind(Treatments=trat,dentro,Between_Clusters=Entre)
      P1=cbind(P1,QM=round(P1[,2]/P1[,1],6))
      P1=cbind(P1,Fc=round(P1[,3]/resid[3],4))
      P1=cbind(P1,pValue=round(pf(P1[,4],df1 = P1[,1],df2=resid[1],lower.tail = F),4))
      P1[,2]=round(P1[,2],6)
      resid[2:3]=round(resid[2:3],6)
      Anova.res[[i-2]]=data.frame(rbind(P1,Residuals=c(resid,"","")))
      }




    m=lm(as.matrix(Dados[,-c(1,2)])~Between_Clusters)
    MANOVA=try(anova(m,test = test),silent = TRUE)
    if(!is(MANOVA)[1]=="try-error") MANOVA=MANOVA[-1,]
    if(is(MANOVA)[1]=="try-error"){MANOVA="Nao e possivel fazer a MANOVA, pois o numero de variaveis e maior que os graus de liberdade do residuo."}

  }
  #DBC
  if(design==3){
    Anova.res=list()
    for( i in 3:ncol(Dados)){
      ####################################################
      m=aov(Dados[,i]~as.factor(Dados[,1])+as.factor(Dados[,2]))
      trat=c(anova(m)[[1,1]],anova(m)[[1,2]])
      bloco=c(anova(m)[[2,1]],anova(m)[[2,2]])
      resid=c(anova(m)[[3,1]],anova(m)[[3,2]],anova(m)[[3,3]])
      dentro=nomes=NULL
      for(j in 1:length(unique(Cluster))){
        Dados2=Dados[Cluster==j,]
        if(length(unique(as.character(Dados2[,1])))>1){
          dentro=rbind(dentro,sq(Dados2[,1],Dados2[,i]))
          nomes=c(nomes,paste("Within_Cluster",j))
        }
      }
      rownames(dentro)=nomes

      Entre=sq(Cluster,Dados[,i])

      P1=rbind(Treatments=trat,dentro,Between_Clusters=Entre,Block=bloco)
      P1=cbind(P1,QM=round(P1[,2]/P1[,1],6))
      P1=cbind(P1,Fc=round(P1[,3]/resid[3],4))
      P1=cbind(P1,pValue=round(pf(P1[,4],df1 = P1[,1],df2=resid[1],lower.tail = F),4))
      P1[,2]=round(P1[,2],6)
      resid[2:3]=round(resid[2:3],6)
      Anova.res[[i-2]]=data.frame(rbind(P1,Residuals=c(resid,"","")))
    }



    Block=as.factor(Dados[,2])
    m=lm(as.matrix(Dados[,-c(1,2)])~Between_Clusters+Block)
    MANOVA=try(anova(m,test = test),silent = TRUE)
    if(!is(MANOVA)[1]=="try-error") MANOVA=MANOVA[-1,]
    if(is(MANOVA)[1]=="try-error"){MANOVA="Nao e possivel fazer a MANOVA, pois o numero de variaveis e maior que os graus de liberdade do residuo."}

  }
  #DQL
  if(design==4){
    Anova.res=list()
    for( i in 4:ncol(Dados)){
      ####################################################
      m=aov(Dados[,i]~as.factor(Dados[,1])+as.factor(Dados[,2])+as.factor(Dados[,3]))
      trat=c(anova(m)[[1,1]],anova(m)[[1,2]])
      Linha=c(anova(m)[[2,1]],anova(m)[[2,2]])
      Coluna=c(anova(m)[[3,1]],anova(m)[[3,2]])
      resid=c(anova(m)[[4,1]],anova(m)[[4,2]],anova(m)[[4,3]])
      dentro=nomes=NULL
      for(j in 1:length(unique(Cluster))){
        Dados2=Dados[Cluster==j,]
        if(length(unique(as.character(Dados2[,1])))>1){
          dentro=rbind(dentro,sq(Dados2[,1],Dados2[,i]))
          nomes=c(nomes,paste("Within_Cluster",j))
        }
      }
      rownames(dentro)=nomes
      Entre=sq(Cluster,Dados[,i])

      P1=rbind(Treatments=trat,dentro,Between_Clusters=Entre,Rows=Linha,Cols=Coluna)
      P1=cbind(P1,QM=round(P1[,2]/P1[,1],6))
      P1=cbind(P1,Fc=round(P1[,3]/resid[3],4))
      P1=cbind(P1,pValue=round(pf(P1[,4],df1 = P1[,1],df2=resid[1],lower.tail = F),4))
      P1[,2]=round(P1[,2],6)
      resid[2:3]=round(resid[2:3],6)
      Anova.res[[i-3]]=data.frame(rbind(P1,Residuals=c(resid,"","")))
    }



    Rows=as.factor(Dados[,2])
    Cols=as.factor(Dados[,2])
    m=lm(as.matrix(Dados[,-c(1,2)])~Between_Clusters+Rows+Cols)
    MANOVA=try(anova(m,test = test),silent = TRUE)
    if(!is(MANOVA)[1]=="try-error") MANOVA=MANOVA[-1,]
    if(is(MANOVA)[1]=="try-error"){MANOVA="Nao e possivel fazer a MANOVA, pois o numero de variaveis e maior que os graus de liberdade do residuo."}

  }
  Res=NULL
  Res$Anova=Anova.res
  #
 # Res$MANOVA=MANOVA

  return(Res)
}
