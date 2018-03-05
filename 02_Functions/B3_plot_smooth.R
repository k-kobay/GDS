# calc.smoothとtmpPCAからプロットする関数


plot.smooth <- function(calc.smooth, DataPCA, PCA=TRUE,num, axisPCs=c(1,2,3), col="mycol", axisLenVal="auto", addAxis=TRUE, day_PCA=TRUE, day_PCA_col="auto",
                        xlim="auto", ylim="auto", zlim="auto",...){
  rslt.mat <- calc.smooth$rslt.mat
  axis.max <- calc.smooth$axis.max
  axis.min <- calc.smooth$axis.min
  mycol    <- calc.smooth$mycol # ;print(mycol)
  xlab     <- calc.smooth$xlab
  ylab     <- calc.smooth$ylab
  zlab     <- calc.smooth$zlab
  
  tmp_col <- ifelse(col=="mycol", mycol, col)
  
  if(length(xlim)==1 && xlim=="auto"){
    xlim <- c(axis.min[1],axis.max[1])
  }
  
  if(length(ylim)==1 && ylim=="auto"){
    ylim <- c(axis.min[2],axis.max[2])
  }
  
  if(length(zlim)==1 && zlim=="auto"){
    zlim <- c(axis.min[3],axis.max[3])
  }
  
  
  plot3d(rslt.mat[,3],rslt.mat[,4],rslt.mat[,5], col=tmp_col, xlim=xlim, ylim=ylim, zlim=zlim, ...)
  # plot3d(rslt.mat[,3],rslt.mat[,4],rslt.mat[,5], col=tmp_col, xlim=xlim, ylim=ylim, zlim=zlim)
  # PCAの軸
  if(PCA==TRUE){
    tmpAxis <- DataPCA$rotation[axisPCs,1:3]
    Labels  <- paste("PC", axisPCs, sep="")
    
    # 軸を４つ以上選びたい場合はあるか？？その場合は変更が必要
    if(axisLenVal == "auto"){
      AxisLengths <- (axis.max - axis.min)/3
    }else{
      AxisLengths <- axisLenVal
    }
    
    axisLength  <- c()
    axisLength[1] <- norm(tmpAxis[1,],type="2")
    axisLength[2] <- norm(tmpAxis[3,],type="2")
    axisLength[3] <- norm(tmpAxis[3,],type="2")
    
    axisCoef <- AxisLengths/axisLength
    
    tmpAxis2 <- matrix(axisCoef, 3, 3) * tmpAxis
    
    if(addAxis==TRUE){
      
      for(i in 1:3){
        # arrow3d(rbind(tmpAxis[i,]*100,0), col=i+1)
        arrow3d(0, tmpAxis2[i,], type = "rotation", col = i+1, width=1/6)
        text3d(tmpAxis2[i,], texts=Labels[i])
      }
      
    }		
  }
  
  
  if(day_PCA==TRUE){
    PCA.filtered <- calc.smooth[[2]]
    
    for(i in num){
      oo   <- PCA.filtered[[i]][,1]
      PC1  <- t(PCA.filtered[[i]][,c(2,3)])
      PC2  <- t(PCA.filtered[[i]][,c(4,5)])
      aa    <- PC1[1,] - oo
      bb    <- PC2[1,] - oo
      
      theta <- seq(-pi, pi, 0.02)
      
      XYZ <- t(aa%*%t(cos(theta)) + bb%*%t(sin(theta)) + oo) 
      
      if(length(day_PCA_col)==1 && day_PCA_col=="auto"){
        lines3d(XYZ, col=mycol[i], lwd=2)
      }else if(length(day_PCA_col)==1 && day_PCA_col!="auto"){
        lines3d(XYZ, col=day_PCA_col, lwd=2)
      }else{
        lines3d(XYZ, col=day_PCA_col[i], lwd=2)
      }
      
    }
  }
}





