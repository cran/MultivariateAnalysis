#' Componentes principais
#'
#' @description Esta funcao possibilita o estudo dos componentes principais .
#' @usage ComponentesPrincipais(D,padronizar=TRUE,xlab="PCA 1", ylab="PCA 2",
#'   CR=TRUE,CorPlot=TRUE,CorCol="red",VarCol="blue",
#'   Perc=0.1,NomeTrat=NULL,NomeVar=NULL, bty="L")
#' @param D Matriz com os valores para obtencao dos componentes principais.Esta
#'   matriz deve conter os valores observados, sendo as variaveis respostas na
#'   coluna. Esta matriz nao deve conter a identificacao dos tratamentos na
#'   primeira coluna. Se provir de experimento com repeticao, a matriz deve
#'   conter apenas as medias dos tratamentos.
#' @param padronizar Se for TRUE (default) os dados serao padronizados para ter
#'   media 0 e variancia igual a 1. Se for FALSE os componentes principais
#'   considerarao os valores originais.
#' @param xlab Nome do eixo X do grafico de componentes principais.
#' @param ylab Nome do eixo Y do grafico de componentes principais.
#' @param CR Valor logico.Se for TRUE aparecera a contribuicao relativa  dos
#'   dois primeiros componentes principais no grafico.
#' @param CorPlot Valor logico. Se for TRUE sera apresentado no grafico as
#'   correlacoes.
#' @param CorCol Indica a cor das setas referente a apresentacao das correlacoes
#'   no grafico (default = "red").
#' @param VarCol Cor do nome das variavies na dispersao grafica da correlacao.
#' @param Perc  Valor entre 0 e 1 indicando o recuo dos eixos.
#' @param NomeTrat vetor contendo o nome dos tratamentos/individuos. Se for
#'   igual a NULL sera considerado o nome das linhas do objeto D (conjunto de
#'   dados)
#' @param NomeVar Vetor contendo o nome das variaveis resposta. Se for igual a
#'   NULL sera considerado o nome das colunas do objeto D (conjunto de dados)
#' @param bty Deve receber um character indicando o tipo de borda desejado no
#'   grafico:
#'   \itemize{
#'   \item "o": Todas as bordas.
#'   \item "n": Sem bordas.
#'   \item "7" : Acima e a direita.
#'   \item "L" : Abaixo + esquerda (Default).
#'   \item "C" : Acima + Direita + Abaixo.
#'   \item "U" : Direita + Abaixo + Direita.
#'   }
#' @return  Esta funcao retorna informacoes importantes para o estudo de
#'   componentes princiapais. Sao apresentados autovalores e autovetores da
#'   matriz de covariancia, Escores dos componentes principais, correlacao entre
#'   as variaveis e eos escores, contribuicao na explicacao de cada componente e
#'   o grafico de dispersao dos CPs.
#' @importFrom stats as.dist cophenetic cor dist hclust manova sd var na.omit
#' @importFrom graphics arrows text plot lines par polygon
#' @importFrom candisc candisc
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.
#'   (ISBN: 8572691510)
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p.
#' (ISBN 13:978 8581270630)
#'
#'  HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978 0138132637)
#'
#' @seealso \code{\link{eigen}}, \code{\link{princomp}}
#' @examples
#' \donttest{
#' data(Dados.MED)
#' ComponentesPrincipais(Dados.MED)
#' #Atribuindo nome aos tratamentos
#' Trat=paste("T_",1:nrow(Dados.MED),sep="")
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,CorPlot = FALSE)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,CorPlot = TRUE,
#' CorCol = "blue",VarCol="red" )
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,CorPlot = TRUE,bty = "n")
#' }
#' @export


ComponentesPrincipais=function(D,padronizar=TRUE,xlab="PCA 1",
                ylab="PCA 2",CR=TRUE,CorPlot=TRUE,CorCol="red",VarCol="blue",
                Perc=0.1,
                NomeTrat=NULL,NomeVar=NULL,bty="L"){
  if(is.null(NomeTrat)){NomeTrat=rownames(D)}
if(is.null(NomeVar)){NomeVar=colnames(D)}

  nVar=ncol(D)


  #Componentes principais
if(padronizar==TRUE){
  D2=apply(D,2,function(x) (x-mean(x))/sd(x))
D=D2
}

  Eig=eigen(var(D))
  Avl=Eig$values
  Avt=Eig$vectors

  Escores=as.matrix(D)%*%Avt
  Escores2=apply(Escores,2,function(x) (x-mean(x))/sd(x))


  ############################################################################
  ###########################################################################


 if(CorPlot==TRUE) {Escores2=Normatiza(Escores2,Escores2,-1,1)}



  PercX=Perc*(max(Escores2[,1])-min(Escores2[,1]))


  PercY=Perc*(max(Escores2[,2])-min(Escores2[,2]))

  if (CR==T){
    CR2=100*diag(var(Escores))/sum(diag(var(Escores)))
    xlab=paste(xlab," (",round(CR2[1],2),"%)",sep="")
    ylab=paste(ylab," (",round(CR2[2],2),"%)",sep="")
  }


  plot( matrix(c(min(Escores2[,1])-PercX,min(Escores2[,2])-PercY,
                 max(Escores2[,1])+PercX,max(Escores2[,2])+PercY),
               ncol=2,byrow=T),col=0,
        ylab=ylab,xlab=xlab,bty=bty)
  lines(c(0,0),c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),lty=2)
  lines(c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),c(0,0),lty=2)

  #draw.circle(0,0,0.7,lty=3)

  text(Escores2[,1],Escores2[,2],NomeTrat)

  Cor=cor(D,Escores2[,1:2])

  if(CorPlot==TRUE){
  arrows(rep(0,nVar), rep(0,nVar), Cor[,1], Cor[,2],col=rep(CorCol,nVar))
  text(Cor[,1], Cor[,2],NomeVar,col=VarCol)
}


  Imp=rbind(Autovalor=Avl,
            `% Explicacao`=100*Avl/sum(Avl),
            `% Explicacao Acumulada`=100*(cumsum(Avl/sum(Avl))))


  Resultado=list(`Autovalor da matriz de covariancia`=round(Avl,4),
                 `Autovetor da matriz de covariancia` =round(Avt,4),
                 `Escores dos componentes principais`=round(Escores,4),
                 `Correlacao entre as variaveis e os comp. principais`=round(cor(D,Escores),4),
                 `Explicacao dos componentes principais`=round(Imp,4))
  print(Resultado)
  return(Resultado)


}
