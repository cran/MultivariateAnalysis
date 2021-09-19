VariaveisCanonicas2=function(Dados,Modelo=1,Factor=NULL,xlab=NULL, ylab=NULL,
                             CR=TRUE, CorPlot=TRUE,CorCol="red",VarCol="red", x=1,y=2,x3=NULL,bty="L", Perc=0.1){

  xlab=ifelse(is.null(xlab),paste("VC",x),xlab)
  ylab=ifelse(is.null(ylab),paste("VC",y),ylab)
  D=data.frame(Dados)

  modelos=list(
    Y~Trat,
    Y~Trat+Bloco,
    Y~Trat+Linha+Coluna,
    Y~FatorA*FatorB,
    Y~FatorA*FatorB+Bloco)

  #Dic
  if(Modelo==1){
    Trat= as.factor(D[,1])
    Y=as.matrix(D[,-(1:2)])
  }
  #DBC
  if(Modelo==2){
    Trat= as.factor(D[,1])
    Bloco= as.factor(D[,2])
    Y=as.matrix(D[,-(1:2)])
  }
  #DQL
  if(Modelo==3){
    Trat= as.factor(D[,1])
    Linha= as.factor(D[,2])
    Coluna= as.factor(D[,3])
    Y=as.matrix(D[,-(1:3)])
  }

  #Fat2Dic
  if(Modelo==4){
    FatorA= as.factor(D[,1])
    FatorB= as.factor(D[,2])
    #Bloco= as.factor(D[,3])
    Y=as.matrix(D[,-(1:3)])
    Trat=paste(D[,1],D[,2],sep="_")
  }

  #Fat2Dbc
  if(Modelo==5){
    FatorA= as.factor(D[,1])
    FatorB= as.factor(D[,2])
    Bloco= as.factor(D[,3])
    Y=as.matrix(D[,-(1:3)])
    Trat=paste(D[,1],D[,2],sep="_")
  }


  #library(candisc)
  saida=manova(modelos[Modelo][[1]])

  if (Modelo == 1){
  c=candisc(saida, term="Trat", sdcale = T)
  Escores=apply(c$scores[,2:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
  D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }


  if (Modelo == 2){
    c=candisc(saida, term="Trat", sdcale = T)
    Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }


  if (Modelo == 3){
    c=candisc(saida, term="Trat", sdcale = T)
    Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
    }


  if((Modelo==4)){
if(is.null(Factor)){Factor="A:B"}
  if((Factor=="A:B")){
    saida=manova(modelos[1][[1]])
    c=candisc(saida, term="Trat", sdcale = T)
    Escores=apply(c$scores[,2:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }



  if((Factor=="A")){
    saida=manova(modelos[Modelo][[1]])
    c=candisc(saida, term="FatorA", sdcale = T)
    Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
    NomeTrat=rownames(Escores)
    D2=apply(Y,2,function(x)tapply(x, FatorA,mean))
  }

  if((Factor=="B")){
    saida=manova(modelos[Modelo][[1]])
    c=candisc(saida, term="FatorB", sdcale = T)
    Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
    NomeTrat=rownames(Escores)
    D2=apply(Y,2,function(x)tapply(x, FatorB,mean))
  }

}

  if((Modelo==5)){
    if(is.null(Factor)){Factor="A:B"}
  if((Factor=="A:B")){
    saida=manova(modelos[2][[1]])
    c=candisc(saida, term="Trat", sdcale = T)
    Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }



  if((Factor=="A")){
    saida=manova(modelos[Modelo][[1]])
    c=candisc(saida, term="FatorA", sdcale = T)
    Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
    NomeTrat=rownames(Escores)
    D2=apply(Y,2,function(x)tapply(x, FatorA,mean))
  }

  if((Factor=="B")){
    saida=manova(modelos[Modelo][[1]])
    c=candisc(saida, term="FatorB", sdcale = T)
    Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
    NomeTrat=rownames(Escores)
    D2=apply(Y,2,function(x)tapply(x, FatorB,mean))
  }
  }


  saida=manova(modelos[Modelo][[1]])









  Escores2=Escores
  if(CorPlot==TRUE){Escores2=Normatiza(Escores2,Escores2,-1,1)}
  Escores2=apply(Escores,2,function(x) (x-mean(x))/sd(x))
 # Escores2=cbind(Escores2[,x],Escores2[,y])
  Escores2=Normatiza(Escores2,Escores2,-1,1)

  PercX=Perc*(max(Escores2[,1])-min(Escores2[,1]))
  #PercX

  PercY=Perc*(max(Escores2[,2])-min(Escores2[,2]))
  #PercY


  if (CR==T){
    CR2=100*c$eigenvalues/sum(c$eigenvalues)
    xlab=paste(xlab," (",round(CR2[x],2),"%)",sep="")
    ylab=paste(ylab," (",round(CR2[y],2),"%)",sep="")
  }
  plot( matrix(c(min(Escores2[,1])-PercX,min(Escores2[,2])-PercY,max(Escores2[,1])+PercX,max(Escores2[,2])+PercY),ncol=2,byrow=T),col=0,
        ylab=ylab,xlab=xlab,bty=bty)
  lines(c(0,0),c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),lty=2)
  lines(c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),c(0,0),lty=2)

  text(Escores2[,1],Escores2[,2],rownames(Escores2))



  Cor=COR=suppressWarnings(cor(cbind(Escores,D2)))
  Cor=Cor[-c(1:ncol(Escores)),]
  nVar=ncol(Cor)

  if(CorPlot==TRUE){
    arrows(rep(0,nVar), rep(0,nVar), Cor[,1], Cor[,2],col=rep(CorCol,nVar))
    NomeVar=colnames(Y)
    text(Cor[,1], Cor[,2],NomeVar,col=VarCol)
  }




  MANOVAS=list(Teste_Pillai=summary(saida,"Pillai")$stats,
               Teste_Wilks=summary(saida,"Wilks")$stats,
               Teste_HotellingL=summary(saida,"Hotelling-Lawley")$stats,
               Teste_Roy=summary(saida,"Roy")$stats)
  GLR=saida$df.residual
  Cov=summary(saida)$SS$Residuals/GLR
  ContribuicaoVC=cbind(CanRsq=c$canrsq,Autovalor=c$eigenvalues[1:length(c$canrsq)],Porcentagem=100*c$eigenvalues[1:length(c$canrsq)]/sum(c$eigenvalues), PorcentagemAcumulada=cumsum(100*c$eigenvalues[1:length(c$canrsq)]/sum(c$eigenvalues)))



  Resultado=list(Manova=MANOVAS,CovarianciaResidual=round(Cov,4),GLres=GLR,ContribuicaoVC=round(ContribuicaoVC,4),Escores=round(Escores,4), `Correlacoes (importancia relativa)`=round(Cor,4))

  return(Resultado)
}
