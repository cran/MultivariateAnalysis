#' ApplyDissimilaridade
#'
#' @description Esta funcao pode ser utilizado para experimentos com dados
#' qualitativos cujos individuos que compoe cada tratamento possuem valores
#' diferentes. Desta forma, obtem se o a porcentagem de cada classificao
#' para os tratamentos.
#' @usage ApplyDissimilaridade(Dados,Factor)
#' @param Dados    Matriz contendo os dados qualitativos. Nesta matriz
#'  deve conter apenas os dados qualitativos. Nao pode ter a identificacao
#'  de tratamentos, blocos, etc.
#' @param Factor Vetor com os niveis a partir dos quais se pretende obter
#' as porcentagem de cada classificacao.
#' @return A funcao retorna  a porcentagem de cada classificao referente aos
#' dados qualitativos para os tratamentos.
#'
#' @seealso \code{\link{hclust}}, \code{\link{dist}}
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.  (ISBN: 8572691510)
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p. (ISBN13:9788581270630)
#'
#'  HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN13:9780138132637)
#' @examples
#' data(Dados.FMI.Quali)
#' DadosQuali=ApplyDissimilaridade(Dados.FMI.Quali[,6:10],Dados.FMI.Quali[,2])
#' Dist=Distancia(DadosQuali,1)
#' Dendograma(Dist, 3)
#' @export
#'
#'



ApplyDissimilaridade=function(Dados,Factor){
  Trat=(unique(Factor))
Res=Trat
for(i in 1:ncol(Dados)){
  var=na.omit(unique(Dados[,i]))
  Mat=matrix(0,ncol=length(var),nrow=length(Trat))
  colnames(Mat)=paste(colnames(Dados)[i],var,sep="_")
  for(v in 1:length(var)){
    values=tapply((Dados[,i]==var[v])*1,as.factor(Factor),mean,na.rm=T)
    Mat[,v]=values

  }
  Res=cbind(Res,Mat)
}
Res=data.frame(Res)
Res[,-1]=as.numeric(as.matrix(Res[,-1]))
rownames(Res)=names(values)
Res=Res[,-1]
Res}
