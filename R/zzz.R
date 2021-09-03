#' @importFrom crayon green bold
# --------------------------------------



.onAttach <- function(libname, pkgname) {
  packageStartupMessage(green("############################################################"))
  packageStartupMessage(green(paste0("Obrigado por utilizar o",bold(" MultivariateAnalysis "))))
  packageStartupMessage(green("Author: Alcinei Mistico Azevedo (ICA-UFMG)"))
  packageStartupMessage(green("Veja tutoriais sobre este e outros pacotes no youtube:"))
  packageStartupMessage(green("https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ"))
  packageStartupMessage(green("Se inscreva e compartilhe para ajudar o canal a crescer."))
  packageStartupMessage(green("############################################################"))

}




linearizar=function(Dist){
  res=NULL
  Dist=as.matrix(Dist)
  for(i in 1:(ncol(Dist)-1)){
    for(j in (i+1):ncol(Dist)){
      x= c(colnames(Dist)[i],colnames(Dist)[j],Dist[i,j])
      res=rbind(res,x)
    }

  }

  res=data.frame(res)
  res[,3]=as.numeric(res[,3])
  res
}


NomeTrat=function(D,Min){
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



