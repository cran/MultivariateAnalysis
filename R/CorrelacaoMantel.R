#' Estimacao da correlacao e significancia pelo metodo Mantel.
#'
#' @description Esta funcao faz a correlacao entre matrizes e estima sua
#' significancia pelo teste Mantel.
#' @name CorrelacaoMantel
#' @usage CorrelacaoMantel(Mat1,Mat2,
#'                         nperm=999,
#'                         alternativa="bilateral",
#'                         Plot=TRUE,
#'                         xlab="Dist1",
#'                         ylab="Dist2",
#'                         bty="l")
#' @param Mat1 Objeto contendo a matriz de dissimilaridade. A matriz deve ser
#' quadrada e simetrica. Ou um objeto do tipo `dist`.
#' @param Mat2 Objeto contendo a matriz de dissimilaridade. A matriz deve ser
#' quadrada e simetrica. Ou um objeto do tipo `dist`.
#' @param nperm Numero de permutacoes para identificar a signficancia pelo metodo de Mantel
#' @param alternativa Character indicando a hipotese alternativa considerada. Pode ser:
#' \itemize{
#'      \item "bilateral": Testa se a correlacao e diferente de zero.
#'      \item "maior" : Testa se a correlacao e maior de zero.
#'      \item "menor : Testa se a correlacao e menor de zero.
#'      }
#' @param Plot  Valor logico (TRUE ou FALSE) indicando se aparecera o grafico de
#' correlacao entre as matriz cofenetica e de dissimilaridade
#' @param xlab nome do eixo x do grafico
#' @param ylab nome do eixo y do grafico
#' @param bty deve receber um character indicando o tipo de borda desejado no
#'   grafico.
#'
#'
#'    \itemize{
#'      \item"o": Todas as bordas
#'      \item"n": Sem bordas
#'      \item"7": Acima e a direita
#'      \item"L": Abaixo + esquerda (Default)
#'      \item"C": Acima + Direita + Abaixo
#'      \item"U": Direita + Abaixo + Direita
#'      }

#' @return A funcao retorna resultados do teste Tocher.
#' @references
#' PlayList "Curso de Analise Multivariada":
#' https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' Cruz, C.D.; Ferreira, F.M.; Pessoni, L.A. (2011) Biometria aplicada ao estudo
#' da diversidade genetica. Visconde do Rio Branco: Suprema.
#'
#' Rao, R.C. (1952) Advanced statistical methods in biometric research.
#' New York: John Wiley & Sons.
#'
#' Sharma, J.R. (2006) Statistical and biometrical techniques in plant breeding.
#' Delhi: New Age International.

#' Silva, A.R. & Dias, C.T.S. (2013) A cophenetic correlation coefficient for
#' Tocher's method. Pesquisa Agropecuaria Brasileira, 48:589-596.
#'
#' Vasconcelos, E.S.; Cruz, C.D.; Bhering, L.L.; Resende Junior, M.F.R. (2007) Alternative
#'    methodology for the cluster analysis. Pesquisa Agropecuaria Brasileira, 42:1421-1428.
#'
#' @examples
#' \donttest{
#' data(Dados.MED)
#' #Distancia euclidiana.
#' Mat1=Distancia(Dados.MED,1)
#' #Quadrado da distancia euclidiana padronizada media (Dados Quantitativos)".
#' Mat2=Distancia(Dados.MED,6)
#' CorrelacaoMantel(Mat1,Mat2)
#'}
#' @export
#' @exportS3Method print CorrelacaoMantel
#

CorrelacaoMantel=function(Mat1,Mat2,nperm=999,alternativa="bilateral",Plot=TRUE,
                          xlab="Dist1",ylab="Dist2",bty="l"){

  alternativa <- match.arg(alternativa, c("bilateral", "menor", "maior"))

  if(is(Mat1)=="Distancia"){Mat1=Mat1$Distancia}
  if(is(Mat2)=="Distancia"){Mat2=Mat2$Distancia}
  x=(as.matrix(Mat1))
  y=(as.matrix(Mat2))


  r=cor(as.dist(x),as.dist(y))

  r0=NULL
  for(i in 1:nperm){
    id=sample(1:ncol(x),replace = FALSE)
    r0=c(r0,cor(as.dist(x),as.dist(y[id,id])))
  }




  if(alternativa=="bilateral"){pv=mean(c(abs(r)<=abs(r0),1))}
  if(alternativa=="maior"){pv=mean(c(r<=(r0),1))}
  if(alternativa=="menor"){pv=mean(c(r>=(r0),1))}

  Mantel=list(
    correlation=r,
    p.value=pv,
    alternative=alternativa)
   class(Mantel)="CorrelacaoMantel"

if(Plot==TRUE){
  sig=ifelse(Mantel$p.value>0.05,"ns",ifelse((Mantel$p.value<0.05)&(Mantel$p.value>0.01),"*",ifelse(Mantel$p.value<0.01,"**", "")))
  plot(Mat1,Mat2,col="blue",bty=bty,main=paste("r=", round(Mantel$correlation,4),sig),xlab=xlab,ylab=ylab)
  abline(lm(Mat2~Mat1),col=2)
}
return(Mantel)
}

#' @export
print.CorrelacaoMantel=function(x, ...){
  cat( "Estimativa de correlacao: ",x$correlation,"\n")
  cat( "P-valor obtido pelo teste Mantel: ",x$p.value,"\n")
  cat( "Hipotese alternativa: ",x$alternative,"\n")
}




