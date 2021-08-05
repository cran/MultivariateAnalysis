#' Coordenadas principais
#'
#' @description Esta funcao possibilita o estudo dos componentes principais .
#' @usage CoordenadasPrincipais(Dist,
#'        Dados=NULL,
#'        Padronizar=FALSE,
#'        main=NULL,
#'        correction="none",
#'        plot=TRUE)
#' @param Dist Matriz com as medidas de dissimilaridade.
#' @param Dados Matriz com os valores de cada variavel.Deve ser utilizada
#' quando se pretende apresentar no grafico a correlacao entre as variaveis
#' respostas e as coordenadas principais. Esses dados devem ser acrescentados
#' apenas em situacoes quando as variaveis sao quantitativas, caso contrario escreva
#' `NULL` (default).
#' @param Padronizar Se for TRUE (default) os dados serao padronizados para ter
#'   media 0 e variancia igual a 1. Se for FALSE serao considerados os valores originais.
#'   Essa informacao e utilizada apenas quando o argumento Dados nao esta com `NULL`.
#' @param main Texto do titulo do grafico.
#' @param correction Metodos de correcao para autovalores negativos: "lingoes"
#'  ou "cailliez". Default e: "none".
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
#' @seealso \code{\link{pcoa}}, \code{\link{ComponentesPrincipais}}
#' @examples
# \donttest{
#' #Exemplo com dados Quantitativos
#' data("Dados.MED")
#' Dist=Distancia(Dados.MED,Metodo=5)
#' CoordenadasPrincipais(Dist)
#' CoordenadasPrincipais(Dist,Dados.MED,Padronizar = TRUE)
#' #Compare os resultados com os componentes principais
#' ComponentesPrincipais(Dados.MED,padronizar = TRUE)
#'
#' #Exemplo com dados Qualitativos
#'
#' data=data("Dados.CAT")
#' Dist=Distancia(Dados.CAT,Metodo=10)
#' CoordenadasPrincipais(Dist)
# }
#' @importFrom ape pcoa
#' @importFrom stats biplot
#' @export





CoordenadasPrincipais=function(Dist,Dados=NULL,Padronizar=FALSE,main=NULL,correction="none",plot=TRUE){
  Dist=as.dist(Dist)
  if(Padronizar==TRUE){
    Dados=apply(Dados,2,function(x) (x-mean(x))/sd(x))
  }
Res=pcoa(Dist,correction=correction)
if(plot==TRUE){
biplot(Res,Dados,main=main,dir.axis1 = -1,dir.axis2 = -1)
}
return(Res)
}



