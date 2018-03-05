### ドロネー分割の結果を描画するためのオブジェクトを返す返す関数
### 色分けオプションを指定できる
### Stationでは1列目にlongitude, ２列目にlatitudeを指定すること
### fは相関係数と距離の関数で、出力は1次元とすること


del_col_calc <- function(Station, CorMat, gradation=c("blue", "cyan", "green", "yellow", "red"), mapping=FALSE,f){
  require(deldir)
  
  # calclation delaunay triangle
  vtess <- deldir(Station[,1], Station[,2])
  
  # get delaynay decomposition edge information(matrix)
  tmpMat  <- vtess$delsgs
  
  # coloring by correlations
  # tmpCor    <- myCor(Data)
  CorValue  <- CorMat[as.matrix(tmpMat[,5:6])]
  
  tmpDist   <- dist(Station)
  DistVal   <- as.matrix(tmpDist)[as.matrix(tmpMat[,5:6])]
  
  if(mapping==FALSE){
    colValue   <- CorColor(CorValue, gradation)
  }else{
    CorValue2   <- f(CorValue, DistVal)
    colValue    <- CorColor(CorValue2, gradation)
  }
  
  # modifying edge matrix to use matplot function
  xMat2 <- t(tmpMat[,c(1,3)])
  yMat2 <- t(tmpMat[,c(2,4)])

  rr <- list(colValue=colValue, xMat2=xMat2, yMat2=yMat2, Station=Station)
  
  return(rr)
}


# Coefficients : intercept=0.5642,  beta1=0.2109
# xxx      <- DistVal^{-fit1$coef[2]}
# yyy      <- exp(CorValue-fit1$coef[1])/{DistVal^{fit1$coef[2]}}
# StdCor   <- yyy/max(yyy)
#
# my_func <- function(CorValue, DistVal){
#   xxx <- DistVal^{-0.2109}
#   yyy <- exp(CorValue-0.5642)/{DistVal^{0.2109}}
#   rrr <- yyy/max(yyy)
# }
# xxx <- del_col_calc(Station=tmpStation[,c(4:5)], Data=tmpAmedas, gradation=gradation, f=my_func, mapping=TRUE)
