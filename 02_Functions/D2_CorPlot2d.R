# すでに描画されているrglの3dプロットの画面に下記のように観測所と観測所間の相関係数をプロットする関数
# 引数詳細
#  CorMatrix : 扱う全観測所のデータに基づいた相関係数行列を指定する
#  Points    : 観測所の地点情報を指定[longitude, latitude]の順序で列を持つ行列を指定すること
#  center    : c(longitude, latitude)で地点を指定する、プロット上はデフォルトで c と表示される
#  radius    : center の地点から半径 radius 以内の観測所を抜き出すために指定する
#  distance  : distance で指定した距離以内の距離にある２つの観測所を線で結ぶ
#  size      : 観測所を表す丸の大きさを指定する
#  centercex : c の大きさを指定する
#  centercol : cの色を指定する
#  circlecol : 中心c、半径radiusの円を描く時の色を指定する
#  centerlwd : 中心c、半径radiusの円を描く時の線の太さを指定する
#  edgecol   : 観測所間を結ぶ直線の色を指定する。デフォルトは"heat"で相関係数に基づいて青~白~赤と塗り分けられる
#  edgelwd   : 観測所間を結ぶ直線の太さを指定する。デフォルトは"auto"で相関係数に基づいて0~3の値の間を振り分けている
#  text      : edgeの中央に相関係数を表示できる。FALSEにすれば消える
#  textcol   : 表示する相関係数の色を指定する
#  textcex   : 表示する相関係数の大きさを指定する


CorPlot2d <- function(preName, CorMatrix, Points, center, radius=1, distance=0.5, size=10, centercex=1,
                      centermark="c", centercol="black", circlecol="grey", circlelwd=1,
                      edgecol="heat", edgelwd="auto", text=TRUE, textcol="black", textcex=2, 
                      gradation=c("blue", "cyan", "green", "yellow", "red"), stationcol=2, 
                      stationsize=1, stationpch=19, Standardize=FALSE, Sfunction,
                      colsamp=TRUE, jpn.xlim='auto', jpn.ylim='auto', lwd=1, axes=TRUE){
	
	
	# 半径radiusの円を描くためのオブジェクト
	theta    <- seq(-pi, pi, 0.01)
  circle   <- t(c(radius,0,0)%*%t(sin(theta)) + c(0,radius,0)%*%t(cos(theta)) + c(center,0))
    
  # 地点情報Pointsから中心centerから半径radiusの円の内部の観測所を抜き出す
  stations  <- sqrt((Points[,1] - center[1])^2 + (Points[,2]-center[2])^2) <= radius
	tmpPoints <- Points[stations, ]
	
	
	# 距離行列を求める
	Dist <- dist(tmpPoints)
	
	# 距離パラメータを指定し、それより近い距離の点だけ結ぶ
	adjPoint <- which(Dist <= distance)
	Dist2    <- as.matrix(Dist)
	
	tmpPairs <- combn(nrow(Dist2),2)
	
  # 相関行列から該当する相関係数を抜き出す
	tmpCor <- CorMatrix[stations, stations]
	
  if(Standardize==TRUE){
		SCor  <- tmpCor/Sfunction(Dist2)
		SCor2 <- (SCor-min(SCor))/(max(SCor)-min(SCor))
    }	
	# カラーマップを設定
	#cols = colorRamp(c("#0080ff","white","#FF4500"))
	#cols = colorRamp(c("#004080","#0080ff", "white","#B2182B","#67001F"))
	#cols = colorRamp(c("blue", "cyan", "green", "yellow", "red"))
	#cols <- colorRamp(gradation)

	CorColor <- function(x, gradation){
	  
	  n <- length(gradation)
	  
	  # gradationの色の数が奇数のとき
	  if(n %% 2 != 0){
	    # MinusGrad <- gradation[1:(n %/% 2 + 1)]
	    MinusGrad <- gradation[(n %/% 2 + 1):1]
	    PlusGrad  <- gradation[(n %/% 2 + 1):n]
	    
	  # gradationの色の数が偶数の時
	  }else if(n %% 2 == 0){
	    # MinusGrad <- gradation[1:(n %/% 2)]
	    MinusGrad <- gradation[(n %/% 2):1]
	    PlusGrad  <- gradation[(n %/% 2 + 1) : n]
	  }
	  
	  PlusCols   <- colorRamp(PlusGrad)
	  MinusCols  <- colorRamp(MinusGrad)
	  
	  PlusNum  <- which(x > 0)
	  MinusNum <- which(!x > 0)
	  
	  colVal <- rep(NA,length(x))
	  
	  colVal[PlusNum]  <- rgb( PlusCols(x[PlusNum])/255 )
	  colVal[MinusNum] <- rgb( MinusCols(abs(x[MinusNum]))/255 )
	  
	  return(colVal)
	}	
	
	
	usePairs  <- tmpPairs[,adjPoint]
	useCorVal <- round(tmpCor[t(usePairs)], 2)
	
	if(Standardize==TRUE){
	  useCorVal <- round(SCor2[t(usePairs)], 2)
	}
	
	if(edgecol=="heat"){
	  useCorCol <- CorColor(useCorVal, gradation) 
	}else{
	  useCorCol <- edgecol
	}

	if(edgelwd=="auto"){
	  tmpLwd <- abs(useCorVal)*3
	}else{
	  tmpLwd <- edgelwd
	}
	
	
	tmpx1 <- tmpPoints[usePairs[1,], 1]
	tmpx2 <- tmpPoints[usePairs[2,], 1]
	useLineX  <- rbind(tmpx1, tmpx2)
	
	tmpy1 <- tmpPoints[usePairs[1,], 2]
	tmpy2 <- tmpPoints[usePairs[2,], 2]
	useLineY  <- rbind(tmpy1, tmpy2)
	
	if(colsamp==TRUE){
	  laymat2 <- matrix(c(rep(2,21),1,1,0), 3,8)
	  layout(laymat2)
	  
	  # color bar
	  # par(mar=c(2,2,1,1))
	  # plot(NULL, xlim=c(-50,50), ylim=c(0,1), axes=FALSE, xlab="", ylab="", main="Gradation Sample")
	  # rect(-50:49, 0, -49:50, 1, col=CorColor(seq(-1,1,length.out=100), gradation), border=NA)
	  # axis(1, at=seq(-50,50,length.out=length(seq(-1,1,0.1))), labels=seq(-1,1,0.1))
	  
	  par(mar=c(1,1,1,3))
	  plot(NULL, xlim=c(0,1), ylim=c(-50,50), axes=FALSE, xlab="", ylab="", main="Color")
	  rect(0, -50:49, 1, -49:50, col=CorColor(seq(-1,1,length.out=100), gradation), border=NA)
	  axis(4, at=seq(-50,50,length.out=length(seq(-1,1,0.1))), labels=seq(-1,1,0.1), las=2)
	  
	  # The default is c(5, 4, 4, 2) + 0.1
	  par(mar=c(3,3,1,1))
	  JapanPlot2d(preName)
	  matlines(x=useLineX, y=useLineY, col=useCorCol, lty=1, lwd=tmpLwd)
	  
	  # 描画する
	  # clear3d(); JapanPlot3d(preName=c("Ibaraki", "Gunma", "Fukushima", "Tochigi"))
	  points(tmpPoints, col=stationcol, cex=stationsize, pch=stationpch)
	  text(x=center[1], y=center[2], labels=centermark,col=centercol, cex=centercex)
	  lines(circle, col=circlecol, lwd=circlelwd)	
	  
	  if(text==TRUE){
	    meanX <- apply(useLineX,2,mean)
	    meanY <- apply(useLineY,2,mean)
	    
	    text(x=meanX, y=meanY, labels=useCorVal, col=textcol, cex=textcex)
	  }
	  
	  layout(1)
	  par(mar=c(5, 4, 4, 2) + 0.1)
	}else{
	  JapanPlot2d(preNames)
	  matlines(x=useLineX, y=useLineY, col=useCorCol, lty=1, lwd=tmpLwd)
	  
	  # 描画する
	  # clear3d(); JapanPlot3d(preName=c("Ibaraki", "Gunma", "Fukushima", "Tochigi"))
	  points(tmpPoints, col=stationcol, cex=stationsize, pch=stationpch)
	  text(x=center[1], y=center[2], labels=centermark,col=centercol, cex=centercex)
	  lines(circle, col=circlecol, lwd=circlelwd)	
	  
	  if(text==TRUE){
	    meanX <- apply(useLineX,2,mean)
	    meanY <- apply(useLineY,2,mean)
	    
	    text(x=meanX, y=meanY, labels=useCorVal, col=textcol, cex=textcex)
	  }
	  
	}


}
