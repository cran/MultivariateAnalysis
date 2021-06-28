Multicolinearidade2=function(X){

  Eigen=eigen(X)
  Multi='Diagnostico de multicolinearidade'
  EV=Eigen$values
  #names(EV)=colnames(X)
  Multi$AutoValor=c(EV)


  Multi$NumeroCondicao=max(Eigen$values)/min(Eigen$values)
  FIV=diag(solve(X))
  names(FIV)=colnames(X)
  Multi$FIV=FIV
  Multi$Determinante=prod(Eigen$values)
  return(Multi)}
