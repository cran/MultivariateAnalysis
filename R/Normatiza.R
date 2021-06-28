#' Normatiza a matriz para que os valores variem entre valores pre-estabelecidos.
#'
#' @description Esta funcao faz a padronizacao da matriz a fim de retirar a escala.
#' Para isso, os valores das matrizes sao calculados a fim de variar entre um
#' "LimiteInferior" e um "LimiteSuperior".
#' @usage Normatiza(DadosEntrada, DadosBase=NULL, LimiteInferior=0, LimiteSuperior=1,Metodo=1)
#' @param DadosEntrada Matriz contendo os dados sendo normatizados.
#' @param DadosBase Matriz contendo o conjunto de dados referencia para a normatizacao.
#' Se for "NULL" essa matriz de referencia sera a propria matriz de entrada.
#' @param LimiteInferior Numero cujo menor valor devera corresponder.
#' @param LimiteSuperior Numero cujo maior valor devera corresponder.
#' @param Metodo indica a forma que a normatizacao sera feita. Pode receber o valor
#' 1 ou 2:
#' \itemize{
#'  \item  1 = A normatizacao sera feita considerando os dados de cada coluna individualmente.
#'  \item  2 = A normatizacao sera feita considerando os dados de toda a matriz simultaneamente.
#'  }
#'
#' @return Retorna a matriz normatizada.
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
#' data("Dados.MED")
#' Dist=Distancia(Dados.MED,3)
#' Dendograma(Dist)
#' DistN=Normatiza((Dist),LimiteInferior = 0,LimiteSuperior = 1,Metodo = 2)
#' Dendograma(DistN)
#' @export




Normatiza=function(DadosEntrada, DadosBase=NULL, LimiteInferior=0, LimiteSuperior=1,Metodo=1){
  #DadosEntrada=as.matrix(DadosEntrada)
  if(is.null(DadosBase)){DadosBase=DadosEntrada}
  #DadosBase=as.matrix(DadosBase)
  if(Metodo==1){
  valMax = apply(DadosBase,2,max)
  valMin = apply(DadosBase,2,min)
  valMax2=DadosEntrada
  valMin2=DadosEntrada
  Normatizado=DadosEntrada
  for (i in 1:ncol(DadosEntrada)){
    for (j in 1:nrow(DadosEntrada)){
      valMax2[j, i] = valMax[i]
      valMin2[j, i] = valMin[i]
    }}
  Normatizado=(LimiteSuperior - LimiteInferior)*(DadosEntrada- valMax2)/(valMax2- valMin2)
  Normatiza=Normatizado+LimiteSuperior
  return(Normatiza)
  }

  if(Metodo==2){
    valMax = max(c(DadosBase))
    valMin = min(c(DadosBase))
    valMax2=max(c(DadosEntrada))
    valMin2=min(c(DadosEntrada))

    Normatizado=(LimiteSuperior - LimiteInferior)*(DadosEntrada- valMax2)/(valMax2- valMin2)
    Normatiza=Normatizado+LimiteSuperior
    return(Normatiza)


  }
}
