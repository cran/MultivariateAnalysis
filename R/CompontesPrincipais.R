#' Componentes principais
#'
#' @description Esta funcao possibilita o estudo dos componentes principais.
#' @name ComponentesPrincipais
#' @usage ComponentesPrincipais(D,
#' padronizar=TRUE,
#' layout=8,
#' cols=c(1,2),
#' xlab="PCA 1",
#' ylab="PCA 2",
#' CR=TRUE,
#' CorPlot=TRUE,
#' CorCol="red",
#' VarCol="blue",
#' Perc=0.1,
#' NomeTrat=NULL,
#' NomeVar=NULL,
#' bty="L")
#' @param D Matriz com os valores para obtencao dos componentes principais.Esta
#'   matriz deve conter os valores observados, sendo as variaveis respostas na
#'   coluna. Esta matriz nao deve conter a identificacao dos tratamentos na
#'   primeira coluna. Se provir de experimento com repeticao, a matriz deve
#'   conter apenas as medias dos tratamentos.
#' @param padronizar Se for TRUE (default) os dados serao padronizados para ter
#'   media 0 e variancia igual a 1. Se for FALSE os componentes principais
#'   considerarao os valores originais.
#' @param layout Deve ser um numero variando de 1 a 9. Para cada numero teremos
#' um layout diferente.
#' @param cols vetor contendo dois numeros indicando os componentes principais que
#' serao utilizados na representacao bidimencional. Default = c(1,2).
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
#' @importFrom ggplot2 xlab ylab ggplot coord_polar coord_flip element_text element_blank element_rect scale_color_brewer aes geom_tile geom_point scale_colour_gradientn labs geom_text arrow unit theme theme_minimal theme_bw theme_gray geom_segment geom_line theme_linedraw theme_light theme_void theme_classic theme_dark theme_minimal
#' @importFrom graphics layout
#' @importFrom stats cutree line

#
#'
#'
#'
#'
#'
#'
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
#'
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=1)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=2)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=3)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=4)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=5)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=6)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=7)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=8)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,layout=9)
#'
#'
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,CorPlot = FALSE)
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,CorPlot = TRUE,
#' CorCol = "blue",VarCol="red" )
#' ComponentesPrincipais(Dados.MED,NomeTrat = Trat,CorPlot = TRUE,bty = "n")
#' }
#' @export


ComponentesPrincipais=function(D,padronizar=TRUE,layout=8,cols=c(1,2),xlab="PCA 1",
                ylab="PCA 2",CR=TRUE,CorPlot=TRUE,CorCol="red",VarCol="blue",
                Perc=0.1,
                NomeTrat=NULL,NomeVar=NULL,bty="L"){
  if(is.null(NomeTrat)){NomeTrat=rownames(D)}
if(is.null(NomeVar)){NomeVar=colnames(D)}

  nVar=ncol(D)
  x1=x2=NULL

  #Componentes principais
if(padronizar==TRUE){
  D2=apply(D,2,function(x) (x-mean(x))/sd(x))
D=D2
}

  Eig=eigen(var(D))
  Avl=Eig$values
  Avt=Eig$vectors

  Escores=as.matrix(D)%*%(Avt)
# as.matrix(D)%*%Avt
#t(Avt)%*%as.matrix(D)
  Escores2=as.matrix(apply(Escores,2,function(x) (x-mean(x))/sd(x)))


  ############################################################################
  ###########################################################################


 if(CorPlot==TRUE) {Escores2=(Normatiza(Escores2,Escores2,-1,1))}



  PercX=Perc*(max(Escores2[,1])-min(Escores2[,1]))


  PercY=Perc*(max(Escores2[,2])-min(Escores2[,2]))

  if (CR==T){
    CR2=100*diag(var(Escores))/sum(diag(var(Escores)))
    xlab=paste(xlab," (",round(CR2[cols[1]],2),"%)",sep="")
    ylab=paste(ylab," (",round(CR2[cols[2]],2),"%)",sep="")
  }


  if(layout==1){

  plot( matrix(c(min(Escores2[,cols[1]])-PercX,min(Escores2[,cols[2]])-PercY,
                 max(Escores2[,cols[1]])+PercX,max(Escores2[,cols[2]])+PercY),
               ncol=2,byrow=T),col=0,
        ylab=ylab,xlab=xlab,bty=bty)
  lines(c(0,0),c(min(Escores2[,cols[1]])-PercX,max(Escores2[,cols[1]])+PercX),lty=2)
  lines(c(min(Escores2[,cols[1]])-PercX,max(Escores2[,cols[1]])+PercX),c(0,0),lty=2)

  #draw.circle(0,0,0.7,lty=3)

  text(Escores2[,cols[1]],Escores2[,cols[2]],NomeTrat)

  Cor=cor(D,Escores2[,cols])

  if(CorPlot==TRUE){
  arrows(rep(0,nVar), rep(0,nVar), Cor[,1], Cor[,2],col=rep(CorCol,nVar))
  text(Cor[,1], Cor[,2],NomeVar,col=VarCol)
}
}

  if(layout>1){
    mm= matrix(c(min(Escores2[,cols[1]])-PercX,min(Escores2[,cols[2]])-PercY,
                 max(Escores2[,cols[1]])+PercX,max(Escores2[,cols[2]])+PercY),
               ncol=2,byrow=T)

    colnames(mm)=c("x1","x2")
    mm=data.frame(mm)


  P=ggplot(mm, aes(x=x1, y=x2))+
    labs(x=xlab, y = ylab)+
    geom_text(data = data.frame(x1=Escores2[,1],x2=Escores2[,2]),label=NomeTrat)

  L1=data.frame(x1=c(0,0),x2=c(min(Escores2[,cols[1]])-PercX,max(Escores2[,cols[1]])+PercX))
  P=P+  geom_line(data = L1,linetype = "dashed")
  L2=data.frame(x1=c(min(Escores2[,cols[1]])-PercX,max(Escores2[,cols[1]])+PercX),x2=c(0,0))
  P=P+  geom_line(data = L2,linetype = "dashed")

  #########################################
  Cor=cor(D,Escores2[,cols])

  if(CorPlot==TRUE){
    St=data.frame(rep(0,nVar), rep(0,nVar), Cor[,1], Cor[,2])



for(j in 1:nrow(St)){
    P<-P +( geom_segment(x = St[j,1], y = St[j,2], xend = St[j,3], yend =St[j,4],
                         arrow = arrow(length=unit(0.20,"cm")),color=CorCol))

 }

 P=P+   geom_text(data = data.frame(x1=1.1*St[,3],x2=1.1*St[,4]),label=NomeVar,color=VarCol)

  }

  if(layout==2){P=P+theme_gray()}
  if(layout==3){P=P+theme_bw()}
  if(layout==4){P=P+theme_linedraw()}
  if(layout==5){P=P+theme_light()}
  if(layout==6){P=P+theme_dark()}
  if(layout==7){P=P+theme_minimal()}
  if(layout==8){P=P+theme_classic()}
  if(layout==9){P=P+theme_void()}

print(P)
  }




  Imp=rbind(Autovalor=Avl,
            `% Explicacao`=100*Avl/sum(Avl),
            `% Explicacao Acumulada`=100*(cumsum(Avl/sum(Avl))))


  Resultado=list(`Autovalor da matriz de covariancia`=round(Avl,4),
                 `Autovetor da matriz de covariancia` =round(Avt,4),
                 `Escores dos componentes principais`=round(Escores,4),
                 `Correlacao entre as variaveis e os comp. principais`=round(cor(D,Escores),4),
                 `Explicacao dos componentes principais`=round(Imp,4))

  class(Resultado)="ComponentesPrincipais"
  return(Resultado)


}
