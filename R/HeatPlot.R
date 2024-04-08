#' Grafico de calor para a interpretacao do Dendrograma
#'
#' @description Esta funcao apresenta um mapa de calor junto com o Dendrograma.
#' @name HeatPlot
#' @usage HeatPlot(Dendo,Col=NULL,layout=1,cut=1000)
#' @param Dendo Objeto criado pela funcao `Dendrograma`.
#' @param Col Paleta de cores. Veja os exemplos.
#' @param layout Deve ser um numero variando de 1 a 3. Para cada numero teremos
#' um layout diferente.
#' @param cut Valor do corte no dendrograma para o estabelecimento de cluster.
#'
#' @seealso \code{\link[MultivariateAnalysis]{Distancia}} ,\code{\link[MultivariateAnalysis]{Dendrograma}}
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
#' Dendo=Dendrograma(dist)
#' HeatPlot(Dendo)

#' #Distancia Mahalanobis
#' data("Dados.DBC")
#' m=MANOVA(Dados.DBC,Modelo = 2)
#' m
#' dist=Distancia(m$Med,Cov=m$CovarianciaResidual,Metodo = 7)
#' dist
#' Dendo=Dendrograma(dist)
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
#' Dend=Dendrograma(Dist)
#' HeatPlot(Dend)
#' HeatPlot(Dend,Col=col3)
#'
#' #Dados cat
#' data("Dados.CAT")
#' row.names(Dados.CAT)=paste0("T",1:nrow(Dados.CAT))
#' Dist=Distancia(Dados.CAT,Metodo=10)
#' Dist
#' Dend=Dendrograma(Dist)
#' HeatPlot(Dend)
#'
#'
#' @export







HeatPlot=function(Dendo,Col=NULL,layout=1,cut=1000){

Trat=value=NULL








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

  #med=suppressWarnings(Normatiza(Dendo$Distancia$Dados,Metodo = 1))
  med=Dendo$Distancia$Dados
  Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )[ Dendo$MetodoDendo]
n=length(unique(c(as.matrix(med))))

if(layout==1){
heatmap(x =as.matrix(med),col = Cor(n),
          distfun = function(c) Distancia(med,Metodo = Dendo$Distancia$MetodoDist,Cov=Dendo$Distancia$Cov)$Distancia,
          hclustfun = function(x) hclust(x,method =Met),Colv = NA,scale="column")
  #legend(x="top", legend=c(1:n),fill=Cor(100))

}

if(layout==2){
  Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )[ Dendo$MetodoDendo]
  hc     <- hclust(Dendo$Distancia$Distancia,method = Met)
  hcdata <- dendro_data_k(hc, cut)

  p <- plot_ggdendro(hcdata,
                      direction   = "lr",
                     # scale.color = cols,
                      expand.y    = 0.25) +
    theme(axis.text.x      = element_text(color = "#ffffff"),
          panel.background = element_rect(fill  = "#ffffff"),
          axis.ticks       = element_blank()) +
    scale_color_brewer(palette = "Set1") +
    xlab(NULL) +
    ylab(NULL)

  # scale from 0 to 1 and reshape mtcars data
DATA=Dendo$Distancia$Dados[Dendo$Ordem,]
DATA=DATA[nrow(DATA):1,]
scaled2 <- (Normatiza(as.matrix(DATA),Metodo = 1))

#scaled2c=linearize_image(scaled2)
  scaled2c=data.frame(Trat=rep(1:nrow(scaled2),ncol(scaled2)),
                      var=rep(colnames(scaled2),each=nrow(scaled2)),
                      value=c(scaled2))
  rownames(scaled2c)=paste0(1:nrow(scaled2c),":")


  p2 <-  ggplot(scaled2c, aes(var, Trat,fill= value))+
 geom_tile() +
         scale_fill_gradientn(colours = Cor(100)) +
         theme_minimal() +
         theme(axis.text.y = element_blank(),
               axis.text.x = element_text(angle=0, hjust=0)) +
         xlab(NULL) +
         ylab(NULL)

  gridExtra::grid.arrange(p, p2, ncol = 2, widths = 2:3)

}

if(layout==3){
  Met=c( "single","complete","average","ward.D","ward.D2","median","centroid","mcquitty" )[ Dendo$MetodoDendo]
  hc     <- hclust(Dendo$Distancia$Distancia,method = Met)
  hcdata <- dendro_data_k(hc, h = cut)

  p <- plot_ggdendro(hcdata,
                     direction   = "lr",
                     # scale.color = cols,
                     expand.y    = 0.15) +
    theme(axis.text.x      = element_text(color = "#ffffff"),
          panel.background = element_rect(fill  = "#ffffff"),
          axis.ticks       = element_blank()) +
    scale_color_brewer(palette = "Set1") +
    xlab(NULL) +
    ylab(NULL)

  # scale from 0 to 1 and reshape mtcars data
  DATA=Dendo$Distancia$Dados[Dendo$Ordem,]
  DATA=DATA[nrow(DATA):1,]
  scaled2 <- (Normatiza(as.matrix(DATA),Metodo = 1))

  #scaled2c=linearize_image(scaled2)
  scaled2c=data.frame(Trat=rep(1:nrow(scaled2),ncol(scaled2)),
                      var=rep(colnames(scaled2),each=nrow(scaled2)),
                      value=c(scaled2))
  rownames(scaled2c)=paste0(1:nrow(scaled2c),":")


  p2 <-  ggplot(scaled2c)+
    geom_point(aes(x      = var,
                   y      = Trat,
                   size   = value,
                   color  = value),
               show.legend = F)+
    scale_colour_gradientn(colours= Cor(100))+

    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(angle=0, hjust=0)) +
    xlab(NULL) +
    ylab(NULL)

  gridExtra::grid.arrange(p, p2, ncol = 2, widths = 1:2)

}

}
