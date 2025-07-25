#' Distancia de dissimilaridade
#'
#' @description Esta funcao retorna a distancia de dissimilaridade.
#' @name Distancia
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
#'  \item 12 = Dissimilaridade de Jacard: 1-a/(a+b+c).
#'   \item 13 = Dissimilaridade de Sorensen Dice: 1-2a/(2a+b+c).
#'   \item 14 = Dissimilaridade de Sokal e Sneath: 1-2(a+d)/(2(a+d)+b+c)
#'   \item 15 = Dissimilaridade de Roger e Tanimoto: 1-(a+d)/(a+2(b+c)+d)
#'   \item 16 = Dissimilaridade de Russel e Rao: 1-a/(a+b+c+d).
#'   \item 17 = Dissimilaridade de Ochiai: 1-a/sqrt((a+b)(a+c)).
#'   \item 18 = Dissimilaridade de Ochiai II: 1-ab/sqrt((a+b)(a+c)(b+d)(c+d)).
#'   \item 19 = Dissimilaridade de Haman: 1-((a+d)-(b+c))/(a+b+c+d).
#'   \item 20 = Dissimilaridade de Yule: 1-(ad-bc)/(ad+bc).
#'   }
#'
#'   \item Dados mistos
#'   \itemize{
#'   \item  21 =Dissimilaridade de Gower
#'   \item  22 =Dissimilaridade de Gower 2
#'   }
#'   }
#' @details Um problema do indice de Gower (Metodo = 21) e que quando as
#'   variaveis binarias (0 ou 1) indicam a presença ou ausencia de bandas a
#'   informação 0-0 (ausencia de bandas em ambos os individuos) indica que os
#'   dois individuos sao iguais, o que nao e verdade necessariamente. Caso
#'   queira desconsiderar essas informações (0-0) no computo da dissimilaridade,
#'   pode-se usar o "indice de Gower 2" (Metodo =22)).
#' @param Cov   matriz quadrada e simetrica contendo as variancias e
#'   covariancias (residuais) entre as caracteristicas. Necessaria apenas para
#'   calculo da distancia de Mahalanobis.
#' @return A funcao retorna a distancia estimada entre os tratamentos.
#' @seealso \code{\link[stats]{dist}}
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
#' data(Dados.BIN)
#' #12 = Dissimilaridade de Jacard.
#' Distancia(Dados.BIN,12)
#' #13 = Dissimilaridade de Sorensen Dice.
#' Distancia(Dados.BIN,13)
#'   # 14 = Dissimilaridade de Sokal e Sneath: 1-2(a+d)/(2(a+d)+b+c)
#'   Distancia(Dados.BIN,14)
#'   #15 = Dissimilaridade de Roger e Tanimoto: 1-(a+d)/(a+2(b+c)+d)
#'   Distancia(Dados.BIN,15)
#'   #16 = Dissimilaridade de Russel e Rao: 1-a/(a+b+c+d).
#'   Distancia(Dados.BIN,16)
#'   #17 = Dissimilaridade de Ochiai: 1-a/sqrt((a+b)(a+c)).
#'   Distancia(Dados.BIN,17)
#'   #18 = Dissimilaridade de Ochiai II: 1-ab/sqrt((a+b)(a+c)(b+d)(c+d)).
#'   Distancia(Dados.BIN,18)
#'   #19 = Dissimilaridade de Haman: 1-((a+d)-(b+c))/(a+b+c+d).
#'   Distancia(Dados.BIN,19)
#'   #20 = Dissimilaridade de Yule: 1-(ad-bc)/(ad+bc).
#'   Distancia(Dados.BIN,20)
#'
#'#' ##################>Dados mistos (quantitativos, binarios e multicategoricos)
#'   data(Dados.Misto)
#'   Distancia(Dados.Misto,21)
#' @export
#' @exportS3Method print Distancia

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
#   #14 = Dissimilaridade de Sokal e Sneath: 1-2(a+d)/(2(a+d)+b+c)
if(Metodo==14){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=1-2*(a+d)/(2*(a+d)+b+c)
    }
  }
  Dist=as.dist(Mat)
}
#   \item 15 = Dissimilaridade de Roger e Tanimoto: 1-(a+d)/(a+2(b+c)+d)
if(Metodo==15){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=1-(a+d)/(a+2*(b+c)+d)
    }
  }
  Dist=as.dist(Mat)
}
#   \item 16 = Dissimilaridade de Russel e Rao: 1-a/(a+b+c+d).
if(Metodo==16){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=1-a/(a+b+c+d)
    }
  }
  Dist=as.dist(Mat)
}
#   \item 17 = Dissimilaridade de Ochiai: 1-a/sqrt((a+b)(a+c)).
if(Metodo==17){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=1-a/sqrt((a+b)*(a+c))
    }
  }
  Dist=as.dist(Mat)
}
#   \item 18 = Dissimilaridade de Ochiai II: 1-ad/sqrt((a+b)(a+c)(b+d)(c+d)).
if(Metodo==18){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]=1-a*d/sqrt((a+b)*(a+c)*(b+d)*(c+d))
    }
  }
  Dist=as.dist(Mat)
}
#   \item 19 = Dissimilaridade de Haman: 1-((a+d)-(b+c))/(a+b+c+d).
if(Metodo==19){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]= 1-((a+d)-(b+c))/(a+b+c+d)
    }
  }
  Dist=as.dist(Mat)
}
#   \item 20 = Dissimilaridade de Yule: 1-(ad-bc)/(ad+bc).
if(Metodo==20){
  n=nrow(D)
  Mat=matrix(0,ncol=n,nrow=n)
  for(i in 1: (n-1)){
    for(j in i:n){
      a=sum((D[i,]==1)*(D[j,]==1))
      b=sum((D[i,]==1)*(D[j,]==0))
      c=sum((D[i,]==0)*(D[j,]==1))
      d=sum((D[i,]==0)*(D[j,]==0))

      Mat[i,j]=Mat[j,i]= 1-(a*d-b*c)/(a*d+b*c)
    }
  }
  Dist=as.dist(Mat)
}
############################################################
###########################
#Dados mistos
#indice de Gower
if(Metodo==21){

  bin=apply(D,2,function(x2) {x=na.omit(x2) ;length(unique(x))})==2
  class=sapply(D, class)

  numeric=((class=="numeric")|(class=="integer"))&(bin==FALSE)

  idNum=(1:length(numeric))[numeric]
  idquali=(1:length(numeric))[numeric==F]


  mat=div=matrix(0,nrow(D),nrow(D))
    for(i in 1:nrow(D)){
      for(j in 1:nrow(D)){
        for(k in idNum){
          if((is.na(D[i,k])|is.na(D[j,k]))==FALSE){
            mat[i,j]=mat[i,j]+abs(D[i,k]-D[j,k])/(max(D[,k])-min(D[,k]))
            div[i,j]=div[i,j]+1
          }
        }
      }
    }

  for(i in 1:nrow(D)){
    for(j in 1:nrow(D)){
      for(k in idquali){
        if((is.na(D[i,k])|is.na(D[j,k]))==FALSE){
          mat[i,j]=mat[i,j]+(D[i,k]!=D[j,k])
          div[i,j]=div[i,j]+1
        }
      }
    }
  }


 Dist=as.dist(mat/div)

}

############################################################
###########################
#Dados mistos
#indice de Gower2
if(Metodo==22){

  bin=sapply(1:ncol(D),function(i) {x=D[,i];x2=na.omit(x);ifelse(length((unique(x2)))==2,sum(((unique(x2))==c(0,1))|((unique(x2))==c(1,0)))==2,FALSE)})
  class=sapply(D, class)

  numeric=((class=="integer")|(class=="numeric"))&(bin==F)

  quali=(class=="factor")|(class=="character")


  idNum=(1:length(numeric))[numeric]
  idquali=(1:length(quali))[quali]
  idbin=(1:length(bin))[bin]

  mat=div=matrix(0,nrow(D),nrow(D))
  #Numerico
  for(i in 1:nrow(D)){
    for(j in 1:nrow(D)){
      for(k in idNum){
        if((is.na(D[i,k])|is.na(D[j,k]))==FALSE){
          mat[i,j]=mat[i,j]+abs(D[i,k]-D[j,k])/(max(D[,k])-min(D[,k]))
          div[i,j]=div[i,j]+1
        }
      }
    }
  }
  #quali
  for(i in 1:nrow(D)){
    for(j in 1:nrow(D)){
      for(k in idquali){
        if((is.na(D[i,k])|is.na(D[j,k]))==FALSE){
          mat[i,j]=mat[i,j]+(D[i,k]!=D[j,k])
          div[i,j]=div[i,j]+1
        }
      }
    }
  }
  #bin
  for(i in 1:nrow(D)){
    for(j in 1:nrow(D)){
      for(k in idbin){
        if((is.na(D[i,k])|is.na(D[j,k]))==FALSE){
          if((((D[i,k])==1)&((D[j,k])==1))){
            div[i,j]=div[i,j]+1
          }

          if((((D[i,k])==1)!=((D[j,k])==1))){
          mat[i,j]=mat[i,j]+1
          div[i,j]=div[i,j]+1
          }
        }
      }
    }
  }

  Dist=as.dist(mat/div)

}


Distb=as.matrix(Dist)
colnames(Distb)=rownames(Distb)=rownames(D)
Distb=as.dist(Distb)

Res=list(Distancia=Distb,Dados=Dados,MetodoDist=Metodo,Cov=Cov)
class(Res)="Distancia"
return(Res)
}

#' @export
print.Distancia=function(x,...){
  Met=c("1 = Distancia euclidiana.
    "," 2= Distancia euclidiana media.
    ","3 = Quadrado da distancia euclidiana media.
    ","4 = Distancia euclidiana padronizada.
    ","5 = Distancia euclidiana padronizada media.
    ","6 = Quadrado da distancia euclidiana padronizada media.
    ","7 = Distancia de Mahalanobis.
    ","8 = Distancia de Cole Rodgers.
    ","9 = Frequencia de coincidencia.
    ","10 = Frequencia de discordancia.
    ","11 = indice Inverso de 1+coincidencia = 1/(1+c)
    ","12 = Dissimilaridade de Jacard: 1-a/(a+b+c).
    ","13 = Dissimilaridade de Sorensen Dice: 1-2a/(2a+b+c).
    ","14 = Dissimilaridade de Sokal e Sneath: 1-2(a+d)/(2(a+d)+b+c)
    ","15 = Dissimilaridade de Roger e Tanimoto: 1-(a+d)/(a+2(b+c)+d)
    ","16 = Dissimilaridade de Russel e Rao: 1-a/(a+b+c+d).
    ","17 = Dissimilaridade de Ochiai: 1-a/sqrt((a+b)(a+c)).
    ","18 = Dissimilaridade de Ochiai II: 1-ab/sqrt((a+b)(a+c)(b+d)(c+d)).
    ","19 = Dissimilaridade de Haman: 1-((a+d)-(b+c))/(a+b+c+d).
    ","20 = Dissimilaridade de Yule: 1-(ad-bc)/(ad+bc).
    "," 21 =Dissimilaridade de Gower
    "," 22 =Dissimilaridade de Gower 2")



  cat(paste("Medida de dissimilaridade:",Met[x$MetodoDist]),"\n")
  y=SummaryDistancia(x$Distancia,plot = F)
  cat("Menor Distancia:",y$Resumo$Minimo,"\n")
  cat("Maior Distancia:",y$Resumo$Maximo,"\n")
  cat("Media das Distancias:",y$Resumo$Media,"\n")
  cat("Amplitude das Distancias:",y$Resumo$Amplitude,"\n")
  cat("Desvio Padrao das Distancias:",y$Resumo$DesvioPadrao,"\n")
  cat("Coeficiente de variacao das Distancias:",y$Resumo$CoeficienteVariacao,"\n")
  cat("Individuos mais proximos:",y$Resumo$MaisProximo,"\n")
  cat("Individuos mais distantes:",y$Resumo$MaisDistante,"\n")
}
