VariaveisCanonicas2=function(Dados,Modelo=1,Factor=NULL,layout=1,xlab=NULL, ylab=NULL,
                             length=.25,
                             CR=TRUE, CorPlot=TRUE,CorCol="red",VarCol="red",
                             x,y,x3=NULL,bty="L", Perc=0.1){
x1=x2=NULL
 cols=vc=c(x,y)
  xlab=ifelse(is.null(xlab),paste("VC",x),xlab)
  ylab=ifelse(is.null(ylab),paste("VC",y),ylab)
  D=data.frame(Dados)

  modelos=list(
    Y~Trat,
    Y~Trat+Bloco,
    Y~Trat+Linha+Coluna,
    Y~FatorA*FatorB,
    Y~FatorA*FatorB+Bloco,
    Y~FatorA*FatorB*FatorC,
    Y~FatorA*FatorB*FatorC+Bloco
  )

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
  }

  #Fat2Dbc
  if(Modelo==5){
    FatorA= as.factor(D[,1])
    FatorB= as.factor(D[,2])
    Bloco= as.factor(D[,3])
    Y=as.matrix(D[,-(1:3)])
  }

  #Fat3Dic
  if(Modelo==6){
    FatorA= as.factor(D[,1])
    FatorB= as.factor(D[,2])
    FatorC= as.factor(D[,3])
    Y=as.matrix(D[,-(1:4)])
  }

  #Fat3Dbc
  if(Modelo==7){
    FatorA= as.factor(D[,1])
    FatorB= as.factor(D[,2])
    FatorC= as.factor(D[,3])
    Bloco= as.factor(D[,4])
    Y=as.matrix(D[,-(1:4)])
  }



  #library(candisc)
  saida=manova(modelos[Modelo][[1]])

  if (Modelo == 1){
    c=candisc(saida, term="Trat", sdcale = T)

    x=c$scores[,2]
    Escores=tapply(x, Trat,mean)
    #Escores=c$scores[,2]
    if(ncol(c$scores)>2){
      Escores=apply(c$scores[,2:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    }

    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }


  if (Modelo == 2){
    c=candisc(saida, term="Trat", sdcale = T)

    x=c$scores[,3]
    Escores=tapply(x, Trat,mean)
    Escores=c$scores[,3]
    if(ncol(c$scores)>3){
      Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    }

    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }


  if (Modelo == 3){
    c=candisc(saida, term="Trat", sdcale = T)

    x=c$scores[,4]
    Escores=tapply(x, Trat,mean)
    #Escores=c$scores[,4]
    if(ncol(c$scores)>4){
      Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
    }
    D2=apply(Y,2,function(x)tapply(x, Trat,mean))
  }


  if((Modelo==4)){
    if(is.null(Factor)){Factor="A:B"}
    if((Factor=="A:B")){
      Trat=paste(FatorA,FatorB,sep=":")
      saida=manova(modelos[1][[1]])
      c=candisc(saida, term="Trat", sdcale = T)

      x=c$scores[,2]
      Escores=tapply(x, Trat,mean)
      #Escores=c$scores[,2]
      if(ncol(c$scores)>2){
        Escores=apply(c$scores[,2:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
      }

      D2=apply(Y,2,function(x)tapply(x, Trat,mean))
    }



    if((Factor=="A")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorA", sdcale = T)

      x=c$scores[,3]
      Escores=tapply(x, FatorA,mean)

      Escores=c$scores[,3]
      if(ncol(c$scores)>3){
        Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x,FatorA,mean))
      }

      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorA,mean))
    }

    if((Factor=="B")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorB", sdcale = T)


      x=c$scores[,3]
      Escores=tapply(x, FatorB,mean)
      Escores=c$scores[,3]
      if(ncol(c$scores)>3){
        Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      }
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorB,mean))
    }

  }

  if((Modelo==5)){
    if(is.null(Factor)){Factor="A:B"}
    if((Factor=="A:B")){
      Trat=paste(FatorA,FatorB,sep=":")
      saida=manova(modelos[2][[1]])
      c=candisc(saida, term="Trat", sdcale = T)

      x=c$scores[,3]
      Escores=tapply(x, Trat,mean)

      Escores=c$scores[,3]
      if(ncol(c$scores)>3){
        Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
      }

      D2=apply(Y,2,function(x)tapply(x, Trat,mean))
    }



    if((Factor=="A")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorA", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorA,mean)

      Escores=c$scores[,4]
      if(ncol(c$scores)>4){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
      }

      #Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorA,mean))
    }

    if((Factor=="B")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorB", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorB,mean)

      Escores=c$scores[,4]
      if(ncol(c$scores)>4){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      }

      # Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorB,mean))
    }
  }


  if((Modelo==6)){
    if(is.null(Factor)){Factor="A:B:C"}
    if((Factor=="A:B:C")){
      Trat=FatorA:FatorB:FatorC
      saida=manova(modelos[1][[1]])
      c=candisc(saida, term="Trat", sdcale = T)

      x=c$scores[,2]
      Escores=tapply(x, Trat,mean)

      #Escores=c$scores[,3]
      if(ncol(c$scores)>2){
        Escores=apply(c$scores[,2:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
      }
      #Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
      D2=apply(Y,2,function(x)tapply(x, Trat,mean))
      #print(D2)
    }


    if((Factor=="A:B")){
      saida=manova(modelos[6][[1]])
      c=candisc(saida, term="FatorA:FatorB", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorA:FatorB,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>4){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorA:FatorB,mean))
      }
      #Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x,FatorA:FatorB,mean))
      D2=apply(Y,2,function(x)tapply(x, FatorA:FatorB,mean))
    }


    if((Factor=="A:C")){
      saida=manova(modelos[6][[1]])
      c=candisc(saida, term="FatorA:FatorC", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorA:FatorC,mean)
      #Escores=c$scores[,5]
      if(ncol(c$scores)>4){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorA:FatorC,mean))
      }

      #Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x,FatorA:FatorC,mean))
      D2=apply(Y,2,function(x)tapply(x, FatorA:FatorC,mean))
    }

    if((Factor=="B:C")){
      saida=manova(modelos[6][[1]])
      c=candisc(saida, term="FatorB:FatorC", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorB:FatorC,mean)

      #Escores=c$scores[,5]
      if(ncol(c$scores)>4){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorB:FatorC,mean))
      }

      #Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x,FatorB:FatorC,mean))
      D2=apply(Y,2,function(x)tapply(x, FatorB:FatorC,mean))
    }

    if((Factor=="A")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorA", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorA,mean)

      #Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
      }

      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorA,mean))
    }

    if((Factor=="B")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorB", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorB,mean)
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      }

      #Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorB,mean))
    }

    if((Factor=="C")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorC", sdcale = T)

      x=c$scores[,4]
      Escores=tapply(x, FatorC,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>4){
        Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorC,mean))
      }

      #Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorC,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorC,mean))
      #print(D2)
    }


  }


  if((Modelo==7)){
    if(is.null(Factor)){Factor="A:B:C"}
    if((Factor=="A:B:C")){
      Trat=FatorA:FatorB:FatorC
      saida=manova(modelos[2][[1]])
      c=candisc(saida, term="Trat", sdcale = T)

      x=c$scores[,3]
      Escores=tapply(x, Trat,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>3){
        Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
      }

      #  Escores=apply(c$scores[,3:ncol(c$scores)],2,function(x)tapply(x, Trat,mean))
      D2=apply(Y,2,function(x)tapply(x, Trat,mean))
    }


    if((Factor=="A:B")){
      saida=manova(modelos[7][[1]])
      c=candisc(saida, term="FatorA:FatorB", sdcale = T)

      x=c$scores[,5]
      Escores=tapply(x, FatorA:FatorB,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorA:FatorB,mean))
      }

      # Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x,FatorA:FatorB,mean))
      D2=apply(Y,2,function(x)tapply(x, FatorA:FatorB,mean))
    }


    if((Factor=="A:C")){
      saida=manova(modelos[7][[1]])
      c=candisc(saida, term="FatorA:FatorC", sdcale = T)

      x=c$scores[,5]
      Escores=tapply(x, FatorA:FatorC,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorA:FatorC,mean))
      }

      # Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x,FatorA:FatorC,mean))
      D2=apply(Y,2,function(x)tapply(x, FatorA:FatorC,mean))
    }

    if((Factor=="B:C")){
      saida=manova(modelos[7][[1]])
      c=candisc(saida, term="FatorB:FatorC", sdcale = T)

      x=c$scores[,5]
      Escores=tapply(x, FatorB:FatorC,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorB:FatorC,mean))
      }
      # Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x,FatorB:FatorC,mean))
      D2=apply(Y,2,function(x)tapply(x, FatorB:FatorC,mean))
    }

    if((Factor=="A")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorA", sdcale = T)

      x=c$scores[,5]
      Escores=tapply(x, FatorA,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
      }

      #Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorA,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorA,mean))
    }

    if((Factor=="B")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorB", sdcale = T)

      x=c$scores[,5]
      Escores=tapply(x, FatorB,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      }

      # Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorB,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorB,mean))
    }

    if((Factor=="C")){
      saida=manova(modelos[Modelo][[1]])
      c=candisc(saida, term="FatorC", sdcale = T)

      x=c$scores[,5]
      Escores=tapply(x, FatorC,mean)

      # Escores=c$scores[,5]
      if(ncol(c$scores)>5){
        Escores=apply(c$scores[,5:ncol(c$scores)],2,function(x)tapply(x, FatorC,mean))
      }

      # Escores=apply(c$scores[,4:ncol(c$scores)],2,function(x)tapply(x, FatorC,mean))
      NomeTrat=rownames(Escores)
      D2=apply(Y,2,function(x)tapply(x, FatorC,mean))
    }


  }




  saida=manova(modelos[Modelo][[1]])






  if(length(dim(Escores))>1){


    Escores2=Escores[,cols]
    if(CorPlot==TRUE){Escores2=Normatiza(Escores2,Escores2,-1,1)}
    Escores2=apply(Escores,2,function(x) (x-mean(x))/sd(x))
    # Escores2=cbind(Escores2[,x],Escores2[,y])
    Escores2=Normatiza(Escores2,Escores2,-1,1)
    Escores2=Escores2[,cols]
    PercX=Perc*(max(Escores2[,1])-min(Escores2[,1]))
    #PercX

    PercY=Perc*(max(Escores2[,2])-min(Escores2[,2]))
    #PercY


    if (CR==T){

      CR2=100*c$eigenvalues/sum(c$eigenvalues)
      x=vc[1];y=vc[2]
      xlab=paste(xlab," (",round(CR2[x],2),"%)",sep="")
      ylab=paste(ylab," (",round(CR2[y],2),"%)",sep="")
    }


    if(layout==1){

    plot( matrix(c(min(Escores2[,1])-PercX,min(Escores2[,2])-
                     PercY,max(Escores2[,1])+PercX,max(Escores2[,2])+
                     PercY),ncol=2,byrow=T),col=0,
          ylab=ylab,xlab=xlab,bty=bty)
    lines(c(0,0),c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),lty=2)
    lines(c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),c(0,0),lty=2)

    text(Escores2[,1],Escores2[,2],rownames(Escores2))



    Cor=COR=suppressWarnings(cor(cbind(Escores[,cols],D2)))
    Cor=Cor[-c(1:ncol(Escores)),]
    nVar=ncol(Cor)

    if(CorPlot==TRUE){
      arrows(rep(0,nVar), rep(0,nVar), Cor[,1], Cor[,2],col=rep(CorCol,nVar),length =length )
      NomeVar=colnames(Y)
      text(Cor[,1], Cor[,2],NomeVar,col=VarCol)
    }
    }



    if(layout>1){
      NomeVar=colnames(Y)
      Cor=COR=suppressWarnings(cor(cbind(Escores[,cols],D2)))
      Cor=Cor[-c(1:ncol(Escores)),]
      nVar=ncol(Cor)
      NomeTrat=rownames(Escores2)
      mm= matrix(c(min(Escores2[,1])-PercX,min(Escores2[,2])-PercY,
                   max(Escores2[,1])+PercX,max(Escores2[,2])+PercY),
                 ncol=2,byrow=T)

      colnames(mm)=c("x1","x2")
      mm=data.frame(mm)

      P=ggplot(mm, aes(x=x1, y=x2))+
        labs(x=xlab, y = ylab)+
        geom_text(data = data.frame(x1=Escores2[,1],x2=Escores2[,2]),label=NomeTrat)

      L1=data.frame(x1=c(0,0),x2=c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX))
      P=P+  geom_line(data = L1,linetype = "dashed")
      L2=data.frame(x1=c(min(Escores2[,1])-PercX,max(Escores2[,1])+PercX),x2=c(0,0))
      P=P+  geom_line(data = L2,linetype = "dashed")

      #########################################
      Cor=COR=suppressWarnings(cor(cbind(Escores[,cols],D2)))

      if(CorPlot==TRUE){
        St=data.frame(rep(0,nVar), rep(0,nVar), Cor[,1], Cor[,2])
        St=St[-c(1:2),]


        for(j in 1:nrow(St)){
          P<-P +( geom_segment(x = St[j,1], y = St[j,2], xend = St[j,3], yend =St[j,4],
                               arrow = arrow(length=unit(0.20,"cm")),color=CorCol))

        }


        P=P+   geom_text(data = data.frame(x1=1.1*St[,3],x2=1.1*St[,4]),label=NomeVar,color=VarCol)

      }

      if(layout==2){P=P+theme_gray()}
      if(layout==3){P=P+theme_bw()}
      if(layout==4){P=P+theme_linedraw()}
      if(layout==5){P=P+theme_light()}
      if(layout==6){P=P+theme_dark()}
      if(layout==7){P=P+theme_minimal()}
      if(layout==8){P=P+theme_classic()}
      if(layout==9){P=P+theme_void()}

      print(P)
    }



  }

  if(length(dim(Escores))==1){
    YYY=xlab
    if (CR==T){
      YYY=paste0(xlab," (100%)")

    }
    barplot(Escores~rownames(Escores),
            horiz = F,
            xlab = "",
            ylab = YYY
    )

    Cor=COR=suppressWarnings(cor(cbind(Escores,D2)))[-1,]
  }

  MANOVAS=list(Teste_Pillai=summary(saida,"Pillai")$stats,
               Teste_Wilks=summary(saida,"Wilks")$stats,
               Teste_HotellingL=summary(saida,"Hotelling-Lawley")$stats,
               Teste_Roy=summary(saida,"Roy")$stats)
  GLR=saida$df.residual
  Cov=summary(saida)$SS$Residuals/GLR
  ContribuicaoVC=cbind(CanRsq=c$canrsq,Autovalor=c$eigenvalues[1:length(c$canrsq)],Porcentagem=100*c$eigenvalues[1:length(c$canrsq)]/sum(c$eigenvalues), PorcentagemAcumulada=cumsum(100*c$eigenvalues[1:length(c$canrsq)]/sum(c$eigenvalues)))



  Resultado=list(Manova=MANOVAS,CovarianciaResidual=round(Cov,4),GLres=GLR,ContribuicaoVC=round(ContribuicaoVC,4),Escores=round(Escores,4), `Correlacoes (importancia relativa)`=round(Cor,4))
class(Resultado)="vc"
  return(Resultado)
}
