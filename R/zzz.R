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


##############################################################
CompMed=function(n,Pvalor,Sig){
  if(length(Pvalor)==1){
 if(Pvalor>Sig){Tabela=data.frame(Trat=c(1,2),Letters=c("a","a"))}
    if(Pvalor<=Sig){Tabela=data.frame(Trat=c(1,2),Letters=c("a","b"))}
  }

  if(length(Pvalor)>1){
  PV=matrix(0,n,n)
  a=0
  rownames(PV)=colnames(PV)=1:n
  for(i in 1:(n-1)){
    for(j in (i+1): n){
      a=a+1
      PV[i,j]=Pvalor[a]
      PV[j,i]=Pvalor[a]

     nam=unlist(strsplit(names(Pvalor)[a],split = " - "))
    colnames(PV)[i]=nam[1]
    rownames(PV)[j]=nam[2]
     }
  }

  colnames(PV)[length(colnames(PV))]= rownames(PV)[length(colnames(PV))]

  Pvalor=PV
  Medias=n:1
  Medias1=Medias
  rank=order(-Medias1)
  Medias=Medias1[rank]
  diag(Pvalor)=1
  NumTrat=length(Medias)
  Letras=rep("",NumTrat)
  Ref=1
  Outro=2
  l=1
  #Colocar Letras
  while(Ref!=Inf){
    Letras[(Pvalor[,rank[Ref]]>Sig)*1:NumTrat]=paste(Letras[(Pvalor[,rank[Ref]]>Sig)*1:NumTrat],letters[l],sep="")
    Ref=suppressWarnings(min(((Letras[rank]=="")*1:NumTrat)[(Letras[rank]=="")*1:NumTrat>0]));l=l+1
  }

  Tabela=data.frame(Trat=(1:NumTrat)[rank],Medias,Letters=Letras[rank])[,-2]
  Tabela[,1]=colnames(PV)
  }
  return(Tabela)
}
##################################################################



