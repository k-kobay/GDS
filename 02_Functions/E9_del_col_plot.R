### del_col_calcのオブジェクトを使って描画する関数

del_col_plot <- function(del_col_calc, prefectures, jpn.xlim="auto", jpn.ylim="auto", main=""){
  #========
  # extract
  xMat2    <- del_col_calc$xMat2
  yMat2    <- del_col_calc$yMat2
  colValue <- del_col_calc$colValue
  Station  <- del_col_calc$Station
  
  # plot
  laymat2 <- matrix(c(rep(2,21),1,1,0), 3,8)
  layout(laymat2)
  
  par(mar=c(1,1,1,2.5))
  plot(NULL, xlim=c(0,1), ylim=c(-100,100), axes=FALSE, xlab="", ylab="", main="Color")
  rect(0, -100:99, 1, -99:100, col=CorColor2(seq(-1,1,length.out=200), gradation), border=NA)
  axis(4, at=seq(-100,100,length.out=length(seq(-1,1,0.1))), labels=seq(-1,1,0.1), las=2)
  
  
  # The default is c(5, 4, 4, 2) + 0.1
  par(mar=c(3,3,1,1))
  JapanPlot2d(preName=prefectures, jpn.xlim=jpn.xlim, jpn.ylim=jpn.ylim, main=main, obs.only=TRUE)
  points(Station[,1], Station[,2])
  matlines(xMat2, yMat2, col=colValue, lty=1)
  
  layout(1)
  #========
}

### sample code ###
# my_func <- function(CorValue, DistVal){
#   xxx <- DistVal^{-0.2109}
#   yyy <- exp(CorValue-0.5642)/{DistVal^{0.2109}}
# }
# xxx <- del_col_calc(Station=tmpStation[,c(4:5)], Data=tmpAmedas, gradation=gradation, f=my_func, mapping=TRUE)
# del_col_plot(xxx, prefectures=prefectures, jpn.xlim=c(138.0,140.9), jpn.ylim=c(34.8 ,37.5), main="correlation")
