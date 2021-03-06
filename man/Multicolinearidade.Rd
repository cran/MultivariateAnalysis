% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Multicolinearidade.R
\name{Multicolinearidade}
\alias{Multicolinearidade}
\title{Diagnostico de multicolinearidade}
\usage{
Multicolinearidade(Matriz)
}
\arguments{
\item{Matriz}{Matriz na qual se deseja verificar a presenca de
multicolinearidade.}
}
\value{
A funcao retorna os autovalores,numero de condicao, fator de inflacao
  de variancia e a determinante da matriz .
}
\description{
Esta funcao retorna o diagnostico de multicolinearidade. E
  indicativo de multicolinearidade a presenca de pelo menos um fator de
  inflacao de variancia maior que 10 ou numero de condicao maior que 100
  (Cruz et al.,2014).
}
\examples{
data(Dados.MED)
D=cov(Dados.MED)
Multicolinearidade(D)
}
\references{
PlayList "Curso de Analise Multivariada":
 https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR


CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
  melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.  (ISBN: 8572691510)

FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p. (ISBN 13:978 8581270630)

 HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
  (ISBN 13:978 0138132637)
}
