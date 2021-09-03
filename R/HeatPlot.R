#' Grafico de calor para a interpretacao do dendograma
#'
#' @description Esta funcao apresenta um mapa de calor junto com o dendograma.
#' @usage HeatPlot(Dendo,Col=NULL)
#' @param Dendo Objeto criado pela funcao `Dendograma`.
#' @param Col Paleta de cores. Veja os exemplos.
#' @seealso /code{/link{Distancia}/} ,/code{/link{Dendograma}/}, /code{/link{heatplot}/}
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
#'@importFrom stats heatmap
#' @examples
#'  ################################################
#' ####################################
#' #PlotHeat

#' #Distancia euclidiana
#' data("Dados.MED")
#' dist=Distancia(Dados.MED,Metodo = 3)
#' dist
#' Dendo=Dendograma(dist)
#' HeatPlot(Dendo)

#' #Distancia Mahalanobis
#' data("Dados.DBC")
#' m=MANOVA(Dados.DBC,Modelo = 2)
#' m
#' dist=Distancia(m$Med,Cov=m$CovarianciaResidual,Metodo = 7)
#' dist
#' Dendo=Dendograma(dist)
#' HeatPlot(Dendo)

#Criando paleta de cores
#' col0 = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))
#' col1 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
#'                           'cyan', '#007FFF', 'blue','#00007F'))
#' col2 = colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
#'                           '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
#'                           '#4393C3', '#2166AC', '#053061'))
#' col3 = colorRampPalette(c('red', 'white', 'blue'))
#' col4 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
#'                           'cyan', '#007FFF', 'blue', '#00007F'))
#' HeatPlot(Dendo,Col=col1)
#' HeatPlot(Dendo,Col=col3)
#' HeatPlot(Dendo,Col=col4)
#'
#'
#' #Dados binarios
#' data("Dados.BIN")
#' Dist=Distancia(Dados.BIN,Metodo=12)
#' Dist
#' Dend=Dendograma(Dist)
#' HeatPlot(Dend)
#' HeatPlot(Dend,Col=col3)
#'
#' #Dados cat
#' data("Dados.CAT")
#' row.names(Dados.CAT)=paste0("T",1:nrow(Dados.CAT))
#' Dist=Distancia(Dados.CAT,Metodo=10)
#' Dist
#' Dend=Dendograma(Dist)
#' HeatPlot(Dend)
#'
#'
#' @export







HeatPlot=function(Dendo,Col=NULL){










  Desnormatiza=function(DadosNormatizado,DadosEntrada){

    Max=apply(DadosEntrada,2,max)
    Min=apply(DadosEntrada,2,min)

    t(apply(DadosNormatizado,1, function(x) Max+(x-1)*(Max-Min)))
  }




  if(is.null(Col)==F){
    Cor=Col
  }
  if(is.null(Col)==T){
    Cor=colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
                                '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
                                '#4393C3', '#2166AC', '#053061'))
  }

  med=suppressWarnings(Normatiza(Dendo$Distancia$Dados))
  Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )[ Dendo$MetodoDendo]
n=length(unique(c(as.matrix(med))))
  heatmap(x =as.matrix(med)*100,col = Cor(n),
          distfun = function(c) Distancia(Dendo$Distancia$Dados,Metodo = Dendo$Distancia$MetodoDist,Cov=Dendo$Distancia$Cov)$Distancia,
          hclustfun = function(x) hclust(x,method =Met),Colv = NA)
  #legend(x="top", legend=c(1:n),fill=Cor(100))
}
