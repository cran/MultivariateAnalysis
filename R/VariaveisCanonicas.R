#' Analise de variaveis canonicas
#'
#' @description Esta funcao faz a analise dos dados pelo metodo de variaveis
#'   canonicas.
#'   @name VariaveisCanonicas
#' @usage VariaveisCanonicas(Dados,
#'              Modelo=1,
#'               Fator=NULL,
#'               layout=1,
#'                xlab="VC1",
#'                ylab="VC2",
#'                cols=c(1,2),
#'                CR=TRUE,
#'                CorPlot=TRUE,
#'                CorCol="red",
#'                VarCol ="blue",
#'                bty="L",
#'                Perc=0.1,
#'                length = 0.25)
#'
#' @param Dados Matriz contendo os dados para execucao da MANOVA. Para cada
#'   modelo o conjunto de dados precisa estar organizado de uma forma
#'   apropriada:
#'    \itemize{
#'   \item Modelos 1 e 2: As duas primeiras colunas devem conter a
#'   identificacao dos tratamentos, e as demais os valores observanos nas
#'   variaveis respostas.
#'   \item Modelo 3: As tres primeiras colunas devem conter as
#'   informacoes dos tratamentos, linhas e colunas, e posteriormente, os valores
#'   da variavel resposta.
#'   \item Modelos 4 e 5: as primeiras colunas precisam ter a
#'   informacao do fator A, fator B, repeticao/bloco, e posteriormente, as
#'   variaveis respostas.
#'   \item Modelos 6 e 7: as primeiras colunas precisam ter a
#'   informacao do fator A, fator B, fator C, repeticao/bloco, e posteriormente, as
#'   variaveis respostas.
#'   }
#' @param Modelo Valor numerico indicando o delineamento:
#'  \itemize{
#' \item 1 = Delineamento inteiramente casualizado (DIC)
#' \item 2 = Delineamento em blocos casualizados (DBC)
#' \item 3 = Delineamento em quadrado
#'   latino (DQL)
#'   \item 4 = Esquema fatorial duplo em DIC
#'   \item 5 = Esquema fatorial duplo em DBC
#'   \item 6 = Esquema fatorial triplo em DIC
#'   \item 7 = Esquema fatorial triplo em DBC
#'   }
#' @param Fator Indica qual fator deve ser estudado na representacao grafica. Tal
#' decisao pode ser feita baseando na significancia da manova. Esse objeto deve receber:
#'  \itemize{
#' \item NULL = Para os delineamentos DIC, DBC e DQL (1, 2 e 3)
#' \item "A" = Para obter a presentacao grafica apenas dos niveis do fator A em caso
#' de esquema fatorial (Design 4 ou 5)
#' \item "B" = Para obter a presentacao grafica apenas dos niveis do fator B em caso
#' de esquema fatorial (Design 4 ou 5)
#' \item "A:B" = Para obter a presentacao grafica de todos os tratamentos (combinacoes
#' entre os niveis do fator A e B) em caso de esquema fatorial (Design 4, 5, 6 ou 7)
#' \item "A:C" = Para obter a presentacao grafica de todos os tratamentos (combinacoes
#' entre os niveis do fator A e C) em caso de esquema fatorial (Design 6 ou 7)
#' \item "B:C" = Para obter a presentacao grafica de todos os tratamentos (combinacoes
#' entre os niveis do fator B e C) em caso de esquema fatorial (Design 6 ou 7)
#' \item "A:B:C" = Para obter a presentacao grafica de todos os tratamentos (combinacoes
#' entre os niveis do fator A, B e C) em caso de esquema fatorial (Design 6 ou 7)
#'   }
#' @param layout Deve ser um numero variando de 1 a 9. Para cada numero teremos
#' um layout diferente.

#' @param xlab nome do eixo x do grafico de variaveis canonicas
#' @param ylab nome do eixo y do grafico de variaveis canonicas
#' @param cols Numero das variaveis canonicas que se pretende apresentar no grafico.
#' O padrao e `c(1,2)`.
#' @param CR  Valor logico (TRUE ou FALSE) indicando se aparecera no grafico
#'   a contriuicao relativa de cada eixo.
#' @param CorPlot Valor logico. Se for TRUE sera apresentado no grafico as
#'   correlacoes.
#' @param CorCol  Cor das setas na dispersao grafica da correlacao
#'  (default = "black")
#' @param VarCol  Cor do nome das variavies na dispersao grafica da correlacao
#' (default = "red")
#' @param bty deve receber un character indicando o tipo de borda desejado no
#'   grafico.
#'    \itemize{
#'      \item"o": Todas as bordas
#'      \item"n": Sem bordas
#'      \item"7": Acima e a direita
#'      \item"L": Abaixo + esquerda (Default)
#'      \item"C": Acima + Direita + Abaixo
#'      \item"U": Direita + Abaixo + Direita
#'      }
#' @param Perc  Valor entre 0 e 1 indicando o recuo dos eixos.
#' @param length Refere-se ao tamanho da seta. O default e 0.25.
#' @return A funcao retorna resultados associados as variaveis canonicas.
#' @seealso \code{\link{lm}}, \code{\link{manova}}
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
#' \donttest{
#' #Delineamento inteiramente casualizado (DIC)
#' data("Dados.DIC")
#' VariaveisCanonicas(Dados.DIC,1)
#' #Delineamento em blocos casualizados (DBC)
#' data(Dados.DBC)
#' VariaveisCanonicas(Dados.DBC,2,CorCol = "red",VarCol = "red")
#' #Delineamento em quadrado latino (DQL)
#' data(Dados.DQL)
#' VariaveisCanonicas(Dados.DQL,3,layout=2)
#'
#' #Esquema fatorial duplo em DIC
#' data(Dados.Fat2.DIC)
#' VariaveisCanonicas(Dados.Fat2.DIC,4,Fator="A:B")
#' VariaveisCanonicas(Dados.Fat2.DIC,4,Fator="A",layout=3)
#' VariaveisCanonicas(Dados.Fat2.DIC,4,Fator="B",layout=4)
#'
#' #Esquema fatorial duplo em DBC
#' data(Dados.Fat2.DBC)
#' VariaveisCanonicas(Dados.Fat2.DBC,5,Fator="A:B",layout=5)
#' VariaveisCanonicas(Dados.Fat2.DBC,5,Fator="A")
#' VariaveisCanonicas(Dados.Fat2.DBC,5,Fator="B")
#'
#'
#' #Esquema fatorial triplo em DIC
#' data(Dados.Fat3.DIC)
#' VariaveisCanonicas(Dados.Fat3.DIC,6,Fator="A:B")
#' VariaveisCanonicas(Dados.Fat3.DIC,6,Fator="A")
#' VariaveisCanonicas(Dados.Fat3.DIC,6,Fator="B")
#'
#' #Esquema fatorial triplo em DBC
#' data(Dados.Fat3.DBC)
#' VariaveisCanonicas(Dados.Fat3.DBC,7,Fator="A:B")
#' VariaveisCanonicas(Dados.Fat3.DBC,7,Fator="A")
#' VariaveisCanonicas(Dados.Fat3.DBC,7,Fator="B")
#'
#'
#'
#'}
#' @importFrom graphics barplot
#' @export
#' @exportS3Method print VariaveisCanonicas

VariaveisCanonicas=function(Dados,Modelo=1,Fator=NULL,layout=1,xlab="VC1", ylab="VC2",
                            cols=c(1,2),
                        CR=TRUE, CorPlot=TRUE,CorCol="red",VarCol = "blue",
                        bty="L", Perc=0.1,length = 0.25){

D=as.data.frame(Dados)
VC=VariaveisCanonicas2(D,Modelo=Modelo,Factor=Fator,layout=layout,x = cols[1],
                       y = cols[2],xlab=xlab, ylab=ylab,CR=CR, CorPlot=CorPlot,
                       CorCol=CorCol,VarCol = VarCol, bty=bty, Perc=Perc,
                       length = length)
print("a")
class(VC)="VariaveisCanonicas"
return(VC)
}

print.VariaveisCanonicas=function(x, ...){
  cat("__________________________________________________________________","\n")
  cat("Estudo das variaveis canonicas","\n")
  cat("\n")
 cat("Explicacao das variaveis canonicas","\n")
 VC=x$ContribuicaoVC
 rownames(VC)=paste0("VC",1:nrow(VC))
 print(VC)
 cat("\n")

 cat("Escores das variaveis canonicas","\n")
 print(x$Escores)
 cat("\n")

 cat("Importancia","\n")
 cat("Correlacao das caracteristicas com os escores das variaveis canonicas","\n")
 if(length(dim(x$Escores))>1){print(x$`Correlacoes (importancia relativa)`[,1:ncol(x$Escores)])}
 if(length(dim(x$Escores))==1){print(x$`Correlacoes (importancia relativa)`[,1])}


 cat("__________________________________________________________________","\n")
}
