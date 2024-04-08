#' Contribuicao das variaveis independentes para o agrupamento
#'
#' @description Esta funcao retorna a contribuicao relativa entre as variaveis
#' independentes no agrupamento formado pelo metodo Tocher, Kmeans ou Dendrograma.

#' @usage ContribuicaoRelativa(obj,layout=2,theme="default")
#' @name ContribuicaoRelativa

#' @param obj Objeto que se obtem como saida pelas funcoes `Dendrograma()`, `Tocher()` ou `Kmeans()`.
#' @param layout variavel numerica que indica o layout do grafico. Os valores podem variar entre 1 e 5.
#' @param theme Tema utilizado para o graficos do `ggplot2` (Ex.:theme_gray(),
#' theme_bw(), theme_linedraw(), theme_light(), theme_dark(), theme_minimal(),
#' theme_classic(), theme_void(), theme_test()).

#'
#' @return A funcao retorna a contribuicao das variaveis independentes no agrupamento.
#' @seealso \code{\link{Kmeans}}, \code{\link{Kmeans_NumeroOtimo2}} , \code{\link{ContribuicaoRelativa}}
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
#'
#' @export
#' @examples
#'  \donttest{

#'     #Dados sem repeticao considerando o metodo Kmeans
#'      data("Dados.MED")
#'      Dados=Dados.MED
#'      rownames(Dados)=paste("Genotipo",1:10,sep="_")
#'      no=Kmeans_NumeroOtimo2(Dados,design=1,Metodo = 2)
#'      km=Kmeans(Dados,design=1,nclusters=no$ClusterNumber)
#'      km$predict
#'      ContribuicaoRelativa(km,layout = 2)
#'
#'     #Dados de experimento em dic considerando o dendrograma
#'     data("Dados.DIC")
#'     m=MANOVA(Dados = Dados.DIC,Modelo = 1)
#'     dist=Distancia(Dados=m$Med,Metodo =7,Cov = m$CovarianciaResidual)
#'     Dendro=Dendrograma(Dissimilaridade = dist,corte = "Dunn")
#'
#'
#'
#'
#'
#'     #Dados de experimento em dbc
#'     data("Dados.DBC")
#'     Kmeans_NumeroOtimo(Dados=Dados.DBC,design=3,nboot=100,method="silhouette",NumMax=NULL)
#'     Kmeans(Dados=Dados.DBC,design=3,nclusters=2,iter.max = 20,nstart = 1,
#'            algorithm = "Hartigan-Wong")
#'
#'     #Dados de experimento em DQL
#'     data("Dados.DQL")
#'     Kmeans_NumeroOtimo(Dados=Dados.DQL,design=4,nboot=100,method="silhouette",NumMax=NULL)
#'     Kmeans(Dados=Dados.DQL,design=4,nclusters=2,iter.max = 20,nstart = 1,
#'            algorithm = "Hartigan-Wong")
#'
#'     #Dados de experimento em Esquema fatorial em DIC
#'     data("Dados.Fat2.DIC")
#'     Kmeans_NumeroOtimo(Dados=Dados.Fat2.DIC,design=5,nboot=100,method="silhouette",NumMax=NULL)
#'     Kmeans(Dados=Dados.Fat2.DIC,design=5,nclusters=2,iter.max = 20,nstart = 1,
#'            algorithm = "Hartigan-Wong")
#'
#'     #Dados de experimento em Esquema fatorial em DBC
#'     data("Dados.Fat2.DBC")
#'     Kmeans_NumeroOtimo(Dados=Dados.Fat2.DBC,design=6,nboot=100,method="silhouette",NumMax=NULL)
#'     Kmeans(Dados=Dados.Fat2.DBC,design=6,nclusters=2,iter.max = 20,nstart = 1,
#'            algorithm = "Hartigan-Wong")
#'
#' }



ContribuicaoRelativa=function(obj,layout=2,theme="default"){
  CR=obj
  if(is(obj)[1]=="Kmeans"){
    CR=obj$RelativeContribution
  }
  if(is(obj)[1]=="Dendrograma"){
    CR=obj$RelativeContribution
  }

  Variables=""

  dadosCR=data.frame(Variables=names(CR),CR=CR)

  # Crie o gráfico de pizza

if(layout==1){
  if(is(theme)[1]!="theme"){theme=theme_void() }
 p= ggplot(dadosCR, aes(x = "", y = CR, fill = Variables)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme+
    labs(title = "", fill = "Variables")
}
if(layout==2){
  if(is(theme)[1]!="theme"){theme=theme_void() }
  p=  ggplot(dadosCR, aes(x = "", y = CR, fill = Variables)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = paste0(round(CR,2), "%")),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    theme +
    labs(title = "", fill = "Variables")
}
if(layout==3){
  if(is(theme)[1]!="theme"){theme=theme_classic() }
  p=  ggplot(dadosCR, aes(x = "", y = CR, fill = Variables)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(CR,2), "%")),
              position = position_stack(vjust = 0.5)) +
    theme+
    labs(x = "Variables", y = "Relative Contribution (%)", fill = "Variables")
}
if(layout==4){
  if(is(theme)[1]!="theme"){theme=theme_classic() }
  p=  ggplot(dadosCR, aes(x = Variables, y = CR, fill = Variables)) +
    geom_bar(stat = "identity") +
    theme+
    labs(x = "Variables", y = "Relative Contribution (%)", fill = "Variables")
}
if(layout==5){
  if(is(theme)[1]!="theme"){theme=theme_classic() }
  p=  ggplot(dadosCR, aes(x = Variables, y = CR, fill = Variables)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(CR,2), "%")),
              position = position_stack(vjust = 0.5)) +
    theme+
    labs(x = "Variables", y = "Relative Contribution (%)", fill = "Variables")
}
print(p)
return(p)

}
