#' Componentes principais para dados mistos (qualitativos e quantitativos)
#'
#' @description Esta funcao possibilita o estudo dos componentes principais
#' considerando dados quantitativos e qualitativos simultaneamente.
#' @usage ComponentesPrincipais.Misto(Dados,
#' plot="all",
#' NomeTrat=NULL,
#' NomeVar=NULL)
#' @param Dados Dataframe com os valores para obtencao dos componentes principais.Esta
#'   matriz deve conter os valores observados, sendo as variaveis respostas na
#'   coluna. Esta matriz nao deve conter a identificacao dos tratamentos na
#'   primeira coluna. Ha a opcao de colocar o nomes nas linhas para a representacao grafica.
#'   Obrigatoriamente, as colunas com as variaveis quantitivas devem ser do tipo "numeric" ou
#'   "integer". Ja as colunas com valores dos dados qualitativos devem ser do tipo "logic",
#'   "character" ou "factor.
#' @param NomeTrat vetor contendo o nome dos tratamentos/individuos. Se for
#'   igual a NULL sera considerado o nome das linhas do objeto D (conjunto de
#'   dados)
#' @param NomeVar Vetor contendo o nome das variaveis resposta. Se for igual a
#'   NULL sera considerado o nome das colunas do objeto D (conjunto de dados)
#' @param plot Indica o tipo de grafico desejado:
#'
#'   \itemize{
#'   \item "all": Serao apresentados os quatro tipos de graficos.
#'   \item "individuos": Sera apresentado o grafico com a dispersao dos individuos (tratamentos).
#'   \item "nivel" : Sera apresentado o grafico com a dispersao dos niveis das variaveis qualitativas.
#'   \item "correlacao" : Sera apresentado o grafico com a correlacao das variaveis quantitativas com
#'   os componentes principais.
#'   \item "pesos" : Sera apresentado no grafico a contribuicao de cada variavel qualitativa
#'   na explicao dos componentes.
#'   }
#' @return  Esta funcao retorna informacoes importantes para o estudo de
#'   componentes princiapais considerando dados quantitativos e qualitativos simultaneamente.

#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#' Package PCAmix: https://cran.r-project.org/web/packages/PCAmixdata/vignettes/PCAmixdata.html
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
#' @seealso \code{\link{eigen}}, \code{\link{princomp}}, \code{\link{PCAmix}}
#' @importFrom PCAmixdata PCAmix
#' @examples
#' \donttest{
#' data("Dados.Misto")
#'
#' NomeTrat=paste("Trat",1:nrow(Dados.Misto),sep="_")
#' ComponentesPrincipais.Misto(Dados.Misto,NomeTrat = NomeTrat)
#' ComponentesPrincipais.Misto(Dados.Misto,NomeTrat = NomeTrat,plot = "individuos")
#' ComponentesPrincipais.Misto(Dados.Misto,NomeTrat = NomeTrat,plot = "correlacao")
#' }
#' @export



ComponentesPrincipais.Misto=function(Dados,plot="all",NomeTrat=NULL,NomeVar=NULL){
  Dados=as.data.frame(Dados)

  if(is.null(NomeTrat)==F){rownames(Dados)=NomeTrat}

  mat=D=Dados
  ntrat=nrow(mat)
  Diss=Div=matrix(0,ncol=ntrat,nrow=ntrat)
  bin=apply(D,2,function(x) length(unique(x)))==2
  mat2=mat[,bin]

  for(i in 1:ncol(mat2)){
    mat2[,i]=as.numeric(as.factor(mat2[,i]))==max(as.numeric(as.factor(mat2[,i])))
  }
  mat[,bin]=mat2



  Quanti=(sapply(mat,class)=="integer")|(sapply(mat,class)=="numeric")
  Quali=(D[,Quanti==F])
  for(i in 1:ncol(Quali)){Quali[,i]=as.character(Quali[,i])}



  res=PCAmix(X.quanti=D[,Quanti==T], X.quali=Quali,rename.level=TRUE,
             graph=F)


  if(plot=="all"){
    par(mfrow=c(2,2))
    plot(res,choice ="ind" )
    plot(res,choice ="levels" )
    plot(res,choice ="cor" )
    plot(res,choice ="sqload" )
    par(mfrow=c(1,1))
  }
  if(isFALSE((plot=="all")|(plot=="individuos")|(plot=="nivel")|(plot=="correlacao")|(plot=="pesos"))){
    warning("O objeto 'plot' deve conter os textos, 'all', 'individuos', 'nivel', 'correlacao' ou 'pesos' ")
  }

  if(plot=="individuos"){ plot(res,choice ="ind" ) }
  if(plot=="nivel"){ plot(res,choice ="ind" ) }
  if(plot=="correlacao"){ plot(res,choice ="cor" ) }
  if(plot=="pesos"){ plot(res,choice ="sqload" ) }

  Autovalores=res$eig
  Escores=res$ind$coord
  CorrelacaoQuanti=res$quanti$coord
  ImportanciaQuali=res$quali$contrib
  Niveis=res$levels$coord

  Resultado=list(Autovalores=Autovalores,Escores=Escores,
                 CorrelacaoQuanti=CorrelacaoQuanti,
                 ImportanciaQuali=ImportanciaQuali,Niveis=Niveis)
  return(Resultado)
}
