#' Distancia de dissimilaridade
#'
#' @description Esta funcao retorna a distancia de dissimilaridade.
#' @usage Distancia(Dados,Metodo,Cov=NULL)
#' @param Dados Matriz contendo os dados para calculo das distancias. Nas
#'   linhas devem estar os tratamentos, e nas colunas as variaveis respostas.
#'   Neste arquivo nao deve ter a identificacao dos tratamentos.
#' @param Metodo Valor numerico indicando o metodo a ser utilizado:
#' \itemize{
#'  \item  Dados quantitativos
#'
#'  \itemize{
#'  \item 1 = Distancia euclidiana.
#'  \item  2= Distancia euclidiana media.
#'   \item 3 = Quadrado da distancia euclidiana media.
#'   \item 4 = Distancia euclidiana padronizada.
#'   \item 5 = Distancia euclidiana padronizada media.
#'   \item 6 = Quadrado da distancia euclidiana padronizada media.
#'   \item 7 = Distancia de Mahalanobis.
#'   \item 8 = Distancia de Cole Rodgers.
#'   }
#'
#'   \item Dados qualitativos: binarios ou multicategoricos
#'   \itemize{
#'   \item 9 = Frequencia de coincidencia.
#'   \item 10 = Frequencia de discordancia.
#'   \item 11 = indice Inverso de 1+coincidencia = 1/(1+c)
#'   }
#'
#'   \item Dados qualitativos binarios
#'   \itemize{
#'  \item 12 = Dissimilaridade de Jacard.
#'   \item 13 = Dissimilaridade de Sorensen Dice.
#'   }
#'
#'   \item Dados mistos
#'   \itemize{
#'   \item  14 =
#'   Dissimilaridade de Gower
#'   }
#'   }
#'
#' @param Cov   matriz quadrada e simetrica contendo as variancias e
#'   covariancias (residuais) entre as caracteristicas. Necessaria apenas para
#'   calculo da distancia de Mahalanobis.
#' @return A funcao retorna a distancia estimada entre os tratamentos.
#' @seealso /code{/link{dist}/}
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
#' ##########> Dados quantitativos
#' #1 = Distancia euclidiana.
#' Distancia(Dados.MED,1)
#' #2 = Distancia euclidiana media.
#' Distancia(Dados.MED,2)
#' #3 = Quadrado da distancia euclidiana media.
#' Distancia(Dados.MED,3)
#' #4 = Distancia euclidiana padronizada.
#' Distancia(Dados.MED,4)
#' #5 = Distancia euclidiana padronizada media.
#' Distancia(Dados.MED,5)
#' #6 = Quadrado da distancia euclidiana padronizada media (Dados Quantitativos)".
#' Distancia(Dados.MED,6)
#' #7 = Distancia de Mahalanobis.
#' data(Dados.DBC)
#' m=MANOVA(Dados.DBC,2)
#' Med=apply(Dados.DBC[, -c(1:2)],2,function(x) tapply(x,as.factor(Dados.DBC[,1]),mean))
#' CRE=m$CovarianciaResidual
#' Distancia(Med,7,CRE)
#' #8 = Distancia de Cole Rodgers.
#' Distancia(Dados.MED,8)
#'
#' ######################>Dados qualitativos: binarios ou multicategoricos
#' #9 = Frequencia de coincidencia.
#' data(Dados.CAT)
#' Distancia(Dados.CAT,9)
#' #10 = Frequencia de discordancia.
#' Distancia(Dados.CAT,10)
#' data(Dados.BIN)
#' Distancia(Dados.BIN,10)
#' #11 = indice Inverso de 1+coincidencia  > 1/(1+c)
#' Distancia(Dados.CAT,11)
#'
#' ##############################>Dados qualitativos binarios
#' #12 = Dissimilaridade de Jacard.
#' Distancia(Dados.BIN,12)
#' #13 = Dissimilaridade de Sorensen Dice.
#' Distancia(Dados.BIN,13)
#'
#' @export

Distancia=function(Dados,Metodo=1,Cov=NULL){
  CRE=Cov
############################################################
###############
# Dados quantitativos
D=Dados
##Euclidiana
if(Metodo==1){Dist=dist(D)}
##Euclidiana Media
if(Metodo==2){Dist=dist(D)/sqrt(ncol(D))}
##Quadrado da Euclidiana Media
if(Metodo==3){Dist=dist(D)^2/ncol(D)}

##Euclidiana Padronizada

if(Metodo==4){
  D2=apply(D,2,function(x) x/sd(x))
  Dist=dist(D2)}
##Euclidiana Padronizada Media

if(Metodo==5){
  D2=apply(D,2,function(x) x/sd(x))
  Dist=dist(D2)/sqrt(ncol(D))}
##Quadrado da Euclidiana padronizada Media

if(Metodo==6){
  D2=apply(D,2,function(x) x/sd(x))
  Dist=dist(D2)^2/ncol(D)}

##Distancia de Mahalanobis
if(Metodo==7){
CRE=as.matrix(CRE)
v=ncol(D)
n=nrow(D)
Mat=matrix(0,ncol=n,nrow=n)
for(i in 1: (n-1)){
  for(j in i:n){
    Desv=as.matrix(D[i,]-D[j,])
    Mat[i,j]=Mat[j,i]=t(Desv)%*%InversaG(CRE)%*%(Desv)
  }
}
Dist=as.dist(Mat)
}

##Distancia de Cole-Rodgers
if(Metodo==8){
r=apply(D,2,function(x) (x-min(x))/(max(x)-min(x)))
Dist=dist(r)^2
}

############################################################
###############
# Dados Qualitativos - Multicategoricos

##indice de coincidencia
if(Metodo==9){
n=nrow(D)
Mat=matrix(1,ncol=n,nrow=n)
for(i in 1: (n-1)){
  for(j in i:n){

    Mat[i,j]=Mat[j,i]=mean(D[i,]==D[j,])
  }
}
Dist=as.dist(Mat)
}

##indice de discordancia
if(Metodo==10){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){

      Mat[i,j]=Mat[j,i]=1-mean(D[i,]==D[j,])
    }
  }
  Dist=as.dist(Mat)
}

##indice Inverso de 1+c
if(Metodo==11){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){

      Mat[i,j]=Mat[j,i]=mean(D[i,]==D[j,])
    }
  }
  Dist=1/(1+as.dist(Mat))
}


############################################################
###############
# Dados Qualitativos - binarios
#indice de Jacard
if(Metodo==12){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=(b+c)/(a+b+c)
    }
  }
  Dist=as.dist(Mat)
}

#indice de Sorensen-Dice
if(Metodo==13){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=(b+c)/(2*a+b+c)
    }
  }
  Dist=as.dist(Mat)
}
############################################################
###########################
#Dados mistos
#indice de Gower
if(Metodo==14){
  mat=D
  ntrat=nrow(mat)
  Diss=Div=matrix(0,ncol=ntrat,nrow=ntrat)
  for(i in 1:ntrat){
    for(j in 1:ntrat){
      for(k in 1:ncol(mat)){


        #binomial
        if(is.logical(mat[,k])){
          if(mat[i,k]==T |  mat[j,k]==T){
            if((mat[i,k]!=mat[j,k])) {Diss[i,j]=Diss[i,j]+1}
            Div[i,j]=Div[i,j]+1
          }
        }
        #Multicategorico
        if(is.character(mat[,k])){
          Div[i,j]=Div[i,j]+1
          if((mat[i,k]!=mat[j,k])) {Diss[i,j]=Diss[i,j]+1}

        }
        #Quantitativo
        if(is.numeric(mat[,k])){
          Div[i,j]=Div[i,j]+1
          Diss[i,j]=Diss[i,j]+abs(mat[i,k]-mat[j,k])/(max(mat[,k])-min(mat[,k]))

        }



      }

    }
  }
 Mat= Diss/Div
colnames(Mat)

}


Distb=as.matrix(Dist)
colnames(Distb)=rownames(Distb)=rownames(D)

Distb=as.dist(Distb)
return(Distb)
}

