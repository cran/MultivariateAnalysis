#' Resumo das informacoes de cada tratamento em funcao da dissimilaridade
#'
#' @description Esta funcao apresenta informacoes que resumem a matriz de dissimilaridade.
#' @name SummaryDistancia
#' @usage SummaryDistancia(Dist,
#' ndec=2,
#' plot=TRUE,
#' layout="shade",
#' type="full",
#' title=NULL,
#' tl.cex =1,
#' tl.col="black",
#' col=NULL)
#' @param Dist Matriz de dissimilaridade
#' @param ndec Valor numerico indicando o numero de casas decimais.
#' @param plot Valor logico (TRUE ou FALSE). Indica se o grafico deve ser apresentado.
#' @param layout Valor do tipo character indicando como sera preenchido cada casela:
#' \itemize{
#' \item "circle"
#' \item "square"
#' \item "ellipse"
#' \item "number"
#' \item "shade"
#' \item "color"
#' \item "pie"
#' }
#' @param type Character, 'full' (default), 'upper' or 'lower', display full matrix,
#'  lower triangular or upper triangular matrix.
#' \itemize{
#' \item "full"
#'  \item "lower"
#'   \item "upper"
#'   }
#' @param title Texto referente ao titulo da figura
#' @param tl.cex Valor numerico indicando o tamanho das letras no grafico
#' @param tl.col Cor das letras. Default ("black")
#' @param col 	Vetor com a paleta de cores. Estas cores devem ser distribuidas uniformemente.
#'  se NULL, a paleta de cores sera colorRampPalette(col2)(200), veja exemplo abaixo.
#' @return Retorna informacoes importantes sobre cada tratamento em relacao aos
#' demais como distancia media, menor distancia, maior distancia, tratamento
#' mais proximo, tratamento mais distante etc.
#' @seealso \code{\link[stats]{dist}}
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
#'  data(Dados.MED)
#'  Dist=Distancia(Dados.MED,1)
#'  SummaryDistancia(Dist)
#'  #Acrescentando nomes aos tratamentos
#'  Dist=as.matrix(Dist$Distancia)
#'  rownames(Dist)=colnames(Dist)=paste("Trat",1:nrow(Dist))
#'  SummaryDistancia(Dist)
#'  #Diferentes configuracoes
#'  SummaryDistancia(Dist,type  ="lower")
#'  #opcoes de paletas de cores
#'  col0 = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))
#'  col1 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
#'    'cyan', '#007FFF', 'blue','#00007F'))
#'  col2 = colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
#'                            '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
#'                            '#4393C3', '#2166AC', '#053061'))
#'  col3 = colorRampPalette(c('red', 'white', 'blue'))
#'  col4 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
#'                            'cyan', '#007FFF', 'blue', '#00007F'))
#'
#'  SummaryDistancia(Dist,type = "upper",col=col4(200),title = "Medidas de dissimilaridade")
#'  SummaryDistancia(Dist,type = "lower",layout = "ellipse",col=col3(200))
#' @importFrom corrplot corrplot
#' @export
#' @exportS3Method print SummaryDistancia






SummaryDistancia=function(Dist,ndec=2,
                          plot=TRUE,
                          layout="shade",
                          type="full",
                          title=NULL,
                          tl.cex =1,
                          tl.col="black",
                          col=NULL){


  if(class(Dist)[1]=="Distancia"){Dist=Dist$Distancia}

  D=Dist2=as.matrix(Dist)
  diag(Dist2)=NA
  Medio=round(apply(Dist2,1,mean,na.rm=T),ndec)
  Minimo=round(apply(Dist2,1,min,na.rm=T),ndec)
  Maximo=round(apply(Dist2,1,max,na.rm=T),ndec)
  Minimo2=apply(Dist2,1,min,na.rm=T)
  Maximo2=apply(Dist2,1,max,na.rm=T)
  sd=round(apply(Dist2,1,sd,na.rm=T),ndec)

  MaisDistante=MaisProximo=NULL
  for(i in 1:nrow(Dist2)){
    id=Dist2[,i]==Minimo2[i]
    MaisProximo=c(MaisProximo,na.omit(colnames(Dist2)[id])[1])
    id=Dist2[,i]==Maximo2[i]
    MaisDistante=c(MaisDistante,na.omit(colnames(Dist2)[id])[1])
  }

  if (is.null(col)){col = colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
                                             '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
                                             '#4393C3', '#2166AC', '#053061'))(200)}


  ##  different color series
if(plot==TRUE){

  diag(Dist2)=0
  corrplot(corr =Dist2,method =layout,type = type,bg = "white",title = title,
           is.corr = FALSE,tl.cex = tl.cex,tl.col = tl.col,col=col )

}

  NomeTrat=function(D,Min){
    D=as.matrix(D)
    x=NULL
    for(i in 1:(ncol(D)-1)){
      for(j in (i+1):ncol(D)){
        if(D[i,j]==Min){
          x=rbind(x,c(colnames(D)[i],colnames(D)[j]))
        }
      }
    }
    #x=as.matrix(x)
    if(nrow(x)>1){x=x[1,]}
    return(c(x))
  }


  tab=data.frame(Medio=Medio, Minimo=Minimo,Maximo=Maximo,sd=sd,MaisProximo=MaisProximo,MaisDistante=MaisDistante)

  Resumo=list(
  Minimo=min(as.dist(Dist)),
  Maximo=max(as.dist(Dist)),
  Media=mean(as.dist(Dist)) ,
  Amplitude=max(as.dist(Dist))-min(as.dist(Dist)),
  DesvioPadrao= sd(as.dist(Dist)),
  CoeficienteVariacao=100*sd(as.dist(Dist))/mean(as.dist(Dist)),
  MaisProximo=NomeTrat(Dist,min(as.dist(Dist))),
  MaisDistante=NomeTrat(Dist,max(as.dist(Dist))))

  res=list(Tabela=tab,Resumo=Resumo)
  class(res)= "SummaryDistancia"
return(res)
  }


print.SummaryDistancia=function(x, ...){
  cat("_________________________________________________________________________","\n")
  cat("Tabela com o resumo da matriz dissimilaridade","\n")
  print(x$Tabela)
  cat("\n")
  cat("Menor Distancia:",x$Resumo$Minimo,"\n")
  cat("Maior Distancia:",x$Resumo$Maximo,"\n")
  cat("Media das Distancias:",x$Resumo$Media,"\n")
  cat("Amplitude das Distancias:",x$Resumo$Amplitude,"\n")
  cat("Desvio Padrao das Distancias:",x$Resumo$DesvioPadrao,"\n")
  cat("Coeficiente de variacao das Distancias:",x$Resumo$CoeficienteVariacao,"\n")
  cat("Individuos mais proximos:",x$Resumo$MaisProximo,"\n")
  cat("Individuos mais distantes:",x$Resumo$MaisDistante,"\n")
  cat("_________________________________________________________________________","\n")

}
