#' Estudo de coordenadas principais
#'
#' @description Esta funcao possibilita o estudo dos coordenadas principais.
#' @name CoordenadasPrincipais
#' @usage CoordenadasPrincipais(Dist, layout = 1,
#' main = NULL, NomeTrat = NULL, xlab = "PCoA 1", ylab ="PCoA 2",
#' ColVars = c(1, 2), CR = TRUE, Perc = 0.01, plot = TRUE)
#' @param Dist Matriz com as medidas de dissimilaridade.
#' @param layout Deve ser um numero variando de 1 a 8. Para cada numero teremos
#' um layout diferente.

#' @param main Titulo do grafico.
#' @param NomeTrat Nome dos tratamentos.
#' @param xlab Nome do eixo x no grafico.
#' @param ylab Nome do eixo y no grafico.
#' @param ColVars Numero dos eixos que se pretende apresentar no grafico.
#' O padrao e `c(1,2)`.
#' @param CR  Valor logico (TRUE ou FALSE) indicando se aparecera no grafico
#'   a contriuicao relativa de cada eixo.
#' @param Perc  Valor entre 0 e 1 indicando o recuo dos eixos.

#' @param plot Valor logico.Se for TRUE o grafico sera apresentado.
#' @return  Esta funcao retorna informacoes importantes para o estudo de
#'   coordenadas principais.
#' @importFrom stats as.dist cophenetic cor dist hclust manova sd var na.omit
#' @importFrom graphics arrows text plot lines par polygon
#' @importFrom candisc candisc
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'Cailliez, F. (1983) The analytical solution of the additive constant problem.
#'Psychometrika, 48, 305–308.
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.
#'   (ISBN: 8572691510)
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p.
#' (ISBN 13:978 8581270630)
#'
#' Paradis, E.; Schliep, K. ape 5.0: an environment for modern phylogenetics
#' and evolutionary analyses in R. (2019) Bioinformatics. 526-528.
#'
#'  HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978 0138132637)
#'
#'  Lingoes, J. C. (1971) Some boundary conditions for a monotone analysis of
#'   symmetric matrices. Psychometrika, 36, 195–203.
#'
#' @seealso  \code{\link{ComponentesPrincipais}}
#' @examples
# \donttest{
#' #Exemplo com dados Quantitativos
#' data("Dados.MED")
#' Dist=Distancia(Dados.MED,Metodo=5)
#' CoordenadasPrincipais(Dist)
#' #Compare os resultados com os componentes principais
#' ComponentesPrincipais(Dados.MED,padronizar = TRUE)
#'
#' #Exemplo com dados Qualitativos
#'
#' data=data("Dados.CAT")
#' Dist=Distancia(Dados.CAT,Metodo=10)
#' CoordenadasPrincipais(Dist)
# }
#' @importFrom stats biplot
#' @export





CoordenadasPrincipais=function(Dist,layout=1,main=NULL,NomeTrat=NULL,
                               xlab="PCoA 1",ylab="PCoA 2",ColVars=c(1,2),CR=TRUE,Perc=0.01 ,plot=TRUE){
  if(is(Dist)=="Distancia"){Dist=Dist$Distancia}
   Dist=as.dist(Dist)

   x1=x2=NULL
Res=pco(Dist)

if(plot==TRUE){
  dist=as.matrix(Dist)
  vectors=as.matrix(Res$vectors)
  Escores=dist%*%vectors
  if(is.null(NomeTrat)){NomeTrat=1:nrow(Escores)}

 CR1= round(100*Res$values[ColVars[1]]/sum(Res$values),2)
 CR2= round(100*Res$values[ColVars[2]]/sum(Res$values),2)

  if(CR==TRUE){xlab=paste0(xlab," (",CR1,"%)")
  ylab=paste0(ylab," (",CR2,"%)")}


  mm= Escores[,ColVars]

  colnames(mm)=c("x1","x2")
  mm=data.frame(mm)

  P=ggplot(mm, aes(x=x1*1.2, y=x2*1.2))+
    labs(x=xlab, y = ylab)+
    geom_text(data = data.frame(x1=Escores[,1],x2=Escores[,2]),label=NomeTrat)

  PercX=Perc*(max(Escores[,1])-min(Escores[,1]))
  PercY=Perc*(max(Escores[,2])-min(Escores[,2]))

  L1=data.frame(x1=c(0,0),x2=c(min(Escores[,1])-PercX,max(Escores[,1])+PercX))
  P=P+  geom_line(data = L1,linetype = "dashed")
  L2=data.frame(x1=c(min(Escores[,1])-PercX,max(Escores[,1])+PercX),x2=c(0,0))
  P=P+  geom_line(data = L2,linetype = "dashed")
print(layout)
  if(layout[1]==1){P=P+theme_classic()}
  if(layout==2){P=P+theme_gray()}
  if(layout==3){P=P+theme_bw()}
  if(layout==4){P=P+theme_linedraw()}
  if(layout==5){P=P+theme_light()}
  if(layout==6){P=P+theme_dark()}
  if(layout==7){P=P+theme_minimal()}
  if(layout==8){P=P+theme_void()}

print(P)

}
class(Res)="pcoa"
return(Res)
}



