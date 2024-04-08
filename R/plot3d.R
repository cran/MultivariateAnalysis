#' Grafico com tres dimensoes
#'
#' @description Esta funcao cria um grafico 3d a partir de escores.
#' @name plot3d
#' @usage plot3d(Obj,names = NULL,lab=NULL,title=NULL,cols=c(1,2,3),size=1)
#' @param Obj Objeto criado pelas funcoes `ComponentesPrincipais`, `CoordenadasPrincipais` e `VariaveisCanonicas`.
#' @param names Nomes das Variaveis.
#' @param lab Nome dos eixos.
#' @param title  Titulo do grafico.
#' @param cols Numeros dos eixos que aparecera no grafico.
#' @param size Tamanho das letras.

#' @return  Esta funcao retorna um grafico com tres dimensoes.

#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#' @seealso  \code{\link{ComponentesPrincipais}}
#' @examples
# \donttest{
#' #####################################
#' ##### Matriz
#' #################################
#' data("Dados.MED")
#' data=as.matrix(Dados.MED[,5:7])

#' plot3d(Obj = data,names = letters[1:10],
#'        lab=c("Retencao 75",
#'              "Retencao 55",
#'              "Retencao fundo"),size =2 )
#'
#'
#' #####################################
#' ##### Componentes Principais
#' #################################
#' data(Dados.MED)
#' cp=ComponentesPrincipais(Dados.MED,layout = 11,
#'                          CorPlot = TRUE,VarCol = "green")
#' plot3d(Obj = cp,names = letters[1:10],
#'        lab=c("Principal Component 1",
#'              "Principal Component 2",
#'              "Principal Component 3"),size =2 )
#'
#'
#' #####################################
#' ##### Variaveis  canonicas
#' #################################
#' data(Dados.DBC)
#' VC=VariaveisCanonicas(Dados.DBC,2,CorCol = "red",VarCol = "red")
#' plot3d(VC)
#' plot3d(VC,names = paste("Var","1:10"),
#'        lab=c("Variable component 1",
#'              "Variable Component 2",
#'              "Variable Component 3"),size =2 )
#'
#'
#' #####################################
#' ##### Coordenadas Principais
#' #################################
#' data=data("Dados.CAT")
#' Dist=Distancia(Dados.CAT,Metodo=10)
#' CO=CoordenadasPrincipais(Dist)
#' plot3d(Obj = CO,names = letters[1:9],
#'        lab=c("PCoA 1",
#'              "PCoA 2",
#'              "PCoA 3"),size =2 )

#' @export
plot3d=function(Obj,names = NULL,lab=NULL,title=NULL,cols=c(1,2,3),size=1){

if(class(Obj)[1]=="ComponentesPrincipais")  {
  print(class(Obj))
  cp=Obj
Data=data.frame(cp$`Escores dos componentes principais`)[,cols]
colnames(Data)=paste0("CP",1:ncol(Data))
#if(!is.null(lab)) {colnames(Data)=lab}

if(is.null(names)){
  names=rownames(Data)
}

plot=plot_ly(Data, x = ~CP1, y = ~CP2, z = ~CP3)%>%
  #plotly::layout(scene = list(xaxis = list(title = lab[1]),yaxis = list(title = lab[2]),zaxis = list(title = lab[3])))%>%
  plotly::add_text(
  x = Data[,1],
  y = Data[,2],
  z = Data[,3],
  text = names,showlegend = F)%>%
  add_markers(size=size)

}

if(class(Obj)[1]=="VariaveisCanonicas")  {
    cp=Obj
    Data=data.frame(cp$Escores)[,cols]
    colnames(Data)=paste0("VC",1:ncol(Data))
    if(is.null(names)){
      names=rownames(Data)
    }

   plot= plot_ly(Data, x = ~VC1, y = ~VC2, z = ~VC3)%>%
     #layout(scene = list(xaxis = list(title = lab[1]), yaxis = list(title = lab[2]), zaxis = list(title = lab[3])))%>%
     add_text(
       x = Data[,1],
       y = Data[,2],
       z = Data[,3],
       text = names,showlegend = F)%>%
     add_markers(size=size)

}

if(class(Obj)[1]=="pcoa")  {
    cp=Obj
    Data=data.frame(cp$vectors)[,cols]
    colnames(Data)=paste0("PCoA",1:ncol(Data))
    if(is.null(names)){
      names=rownames(Data)
    }

    plot= plot_ly(Data, x = ~PCoA1, y = ~PCoA2, z = ~PCoA3)%>%
      #layout(scene = list(xaxis = list(title = lab[1]), yaxis = list(title = lab[2]), zaxis = list(title = lab[3])))%>%
      add_text(
        x = Data[,1],
        y = Data[,2],
        z = Data[,3],
        text = names,showlegend = F)%>%
      add_markers(size=size)

}

  if(class(Obj)[1]=="matrix")  {
    cp=Obj
    Data=data.frame(cp)[,cols]
    colnames(Data)=paste0("X",1:ncol(Data))
    if(is.null(names)){
      names=rownames(Data)
    }

    plot= plot_ly(Data, x = ~X1, y = ~X2, z = ~X3)%>%
      #layout(scene = list(xaxis = list(title = lab[1]), yaxis = list(title = lab[2]), zaxis = list(title = lab[3])))%>%
      add_text(
        x = Data[,1],
        y = Data[,2],
        z = Data[,3],
        text = names,showlegend = F)%>%
      add_markers(size=size)

  }
  plot
}
