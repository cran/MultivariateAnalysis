% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoordenadasPrincipais.R
\name{CoordenadasPrincipais}
\alias{CoordenadasPrincipais}
\title{Estudo de coordenadas principais}
\usage{
CoordenadasPrincipais(Dist,
       Dados=NULL,
       Padronizar=FALSE,
       main=NULL,
       correction="none",
       plot=TRUE)
}
\arguments{
\item{Dist}{Matriz com as medidas de dissimilaridade.}

\item{Dados}{Matriz com os valores de cada variavel.Deve ser utilizada
quando se pretende apresentar no grafico a correlacao entre as variaveis
respostas e as coordenadas principais. Esses dados devem ser acrescentados
apenas em situacoes quando as variaveis sao quantitativas, caso contrario escreva
`NULL` (default).}

\item{Padronizar}{Se for TRUE (default) os dados serao padronizados para ter
media 0 e variancia igual a 1. Se for FALSE serao considerados os valores originais.
Essa informacao e utilizada apenas quando o argumento Dados nao esta com `NULL`.}

\item{main}{Texto do titulo do grafico.}

\item{correction}{Metodos de correcao para autovalores negativos: "lingoes"
ou "cailliez". Default e: "none".}

\item{plot}{Valor logico.Se for TRUE o grafico sera apresentado.}
}
\value{
Esta funcao retorna informacoes importantes para o estudo de
  coordenadas principais.
}
\description{
Esta funcao possibilita o estudo dos componentes principais .
}
\examples{
#Exemplo com dados Quantitativos
data("Dados.MED")
Dist=Distancia(Dados.MED,Metodo=5)
CoordenadasPrincipais(Dist)
CoordenadasPrincipais(Dist,Dados.MED,Padronizar = TRUE)
#Compare os resultados com os componentes principais
ComponentesPrincipais(Dados.MED,padronizar = TRUE)

#Exemplo com dados Qualitativos

data=data("Dados.CAT")
Dist=Distancia(Dados.CAT,Metodo=10)
CoordenadasPrincipais(Dist)
}
\references{
PlayList "Curso de Analise Multivariada":
 https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR

Cailliez, F. (1983) The analytical solution of the additive constant problem.
Psychometrika, 48, 305–308.

CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
  melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.
  (ISBN: 8572691510)

FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p.
(ISBN 13:978 8581270630)

Paradis, E.; Schliep, K. ape 5.0: an environment for modern phylogenetics
and evolutionary analyses in R. (2019) Bioinformatics. 526-528.

 HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
  (ISBN 13:978 0138132637)

 Lingoes, J. C. (1971) Some boundary conditions for a monotone analysis of
  symmetric matrices. Psychometrika, 36, 195–203.
}
\seealso{
\code{\link{pcoa}}, \code{\link{ComponentesPrincipais}}
}
