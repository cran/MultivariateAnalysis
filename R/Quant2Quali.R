#' Transformar dados quantitativos em qualitativos
#'
#' @description Esta funcao converte dados quantitativos em qualitativos. Para isso,
#' deve-se apenas indicar o numero de classes desejadas. A funcao retorna a classificacao
#' de cada individuo em funcao de intervalos equidistantes formados considerando o numero
#' de classes almejados.
#' @name Quant2Quali
#' @usage Quant2Quali(Dados,nclasses)
#' @param Dados Objeto com os dados quantitativos a serem convertidos em qualitativos.
#' @param nclasses Numero maximo de classes desejado.
#' @return A funcao retorna a classificacao
#' de cada individuo em funcao de intervalos equidistantes formados considerando o numero
#' de classes almejados.
#' @seealso \code{\link{Distancia}} , \code{\link{MediaDistancia}}
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
#' data(Dados.Misto)
#' nclasses=5
#' Dados=Dados.Misto[,c(6,7,10)]
#' Quant2Quali(Dados,nclasses)
#' @export










Quant2Quali=function(Dados,nclasses){
Func= function(x,n){
INT=seq(min(x),max(x),l=n+1)

XX=x*0

for(i in 1:(length(INT)-1)){
  XX[x>=INT[i]]=paste0("C",i)
}
XX
}

apply(Dados,2,function(x) Func(x,n=nclasses))

}
