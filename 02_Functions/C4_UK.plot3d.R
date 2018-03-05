#==============================================================================
# 008. UK.plot3dの定義
#      path.calcの出力結果を元に指定したalphaに対応する三次元プロットを出力する関数
# 関数定義
#    Data  : path.calcの出力オブジェクトを指定
#    num   : path.calcで計算したときのalphaの要素番号を指定
#    size  : plot3dのデータ点の大きさを指定
#    pcol  : データ点の色を指定, "day"は日毎に分ける, "month"は月毎に分ける, "SPL"は最大値と最小値を基準に分ける
#            その他"black"など色を指定すると単色で描画できる
#    lcol  : 線分の色を指定。単色のみ
#    lwd   : 線分の太さを指定
#    theta : 実数を指定. y軸に対して垂直方向に回転させる 正の値で時計回り、負の値で反時計回りに回転する
#    phi   : -90~90の値を指定. x軸に対して垂直方向に回転させる 正の値で時計回り、負の値で反時計回りに回転する
#    fov   : 正数を指定. 手前の面と後方の面の視覚的な大きさの違いを指定する。0であれば手前と後方の大きさは
#            同じように描画される
#    Xp    : Dataと同じサイズの行列を指定することで描画するデータ点をこちらに変更することができる。指定しない場合はDataについて描画する


UK.plot3d <- function(Data, num=1, size=5, pcol="day", lcol="black", lwd=1, theta=0, phi=15, fov=60, main="", xlim="Auto", ylim="Auto", zlim="Auto", Xp=FALSE, fix=TRUE){
	
	# plotした情報を格納するリストを作成

    # ret <- dPathLengthAb(D_origin, alpha=0.1, beta=Inf, nargout=2)
    
    # i の数字は varlist の要素数だけ変更可能
    # いまは iを1と置く。1~6まででそれぞれ α= -20, -15, -10, -5, 0, 5 に対応
    
    # ret に D_alpha_list の i番目の要素を代入
	ret <- Data$D_alpha_list[[num]]
	
	# D に 距離行列を格納
	D <- as.matrix(ret[[1]])
	
	# used_edgesに最短距離に使われる辺の情報を格納
    used_edges <- ret[[2]]
  

    # プロットするために使う最短経路に使われる辺の行列を作成
    # 最短経路の行列を格納するリストを作成
    lines.list <- list()
    
    if(is.data.frame(Xp)==FALSE){
    	Xp <- Data$Xp
    }else{
    	Xp <- Xp
    }
    
    
    # lines.listのk番目の要素にk番目の線分の座標を格納する
    k_max   <- nrow(used_edges)
    k_print <- round(k_max * c(1:10)/10)
    
    for(k in 1:nrow(used_edges)){
	    lines.list[[k]] <- t(Xp[used_edges[k,],])
	    
	    if(sum(k %in% k_print)!=0){
	      print( paste(round((k/k_max), 3)*100, "%")) 
	    }
    }

    # tmp に最短経路の辺で結ばれている点の情報を転置して格納
    tmp <- t(used_edges)

    # tmpをベクトルに変換
    dim(tmp) <- c(1, nrow(tmp)*ncol(tmp))

    # tmpを転置してlinesM.rownamesとする。（これがlinesMの行名となる)
    linesM.rownames <- t(tmp)
    rm(tmp)
    
    # lines.list を データフレーム型に変換する
    linesM <- t(as.data.frame(lines.list))

    # linesMの行名を変更する
    rownames(linesM) <- linesM.rownames

    # linesM の列名を変更
    colnames(linesM) <- c("x", "y", "z")

    # Plot used edges
    # 作図領域のクリア
     # 作図
    # plot3d(linesM, type="l", col="black")
    
    if(pcol=="SPL"){
    # 色分けするためのコード
    SPL  <- Data$SPL_list[[num]]  

    # SPLの最小値を格納
    minl <- min(SPL)
    
    # SPLの最大値を格納
    maxl <- max(SPL)
    
    # データ点の長さを指定
        if(Data$leap==TRUE){
    	    len <- 366
        }else{
    	    len <- 365
        }

    
    # データ点の長さ分色を指定する
    colorlut <- rainbow(len+1)
    
    # 重み付け
    col <- colorlut[len*(SPL - minl)/(maxl - minl)+1]

    }else if(pcol=="month"){
    	tmp  <- rainbow(12)
    	tmp2 <- rep(tmp, each=31)
    	
    	if(Data$leap==TRUE){
    		col <- tmp2[-c(61,62,124,186,279,341)]
    	}else{
    		col <- tmp2[-c(60,61,62,124,186,279,341)]
    	}   	
    }else if(pcol=="day"){
    	col <- rainbow(365)
    }else{
    	col = pcol
    }
    
    
    # データ点を打ち込む
    
    if(is.character(xlim)){
      	xlim <- NA
    }else if(is.numeric(xlim)==TRUE&length(xlim)==2){
    	xlim <- xlim    
    }else{
    		stop("xlimにはmaxとminを指定してください")
    }

    if(is.character(ylim)){
      	ylim <- NA
    }else if(is.numeric(ylim)==TRUE&length(ylim)==2){
     	ylim <- ylim      	
    }else{
     	stop("ylimにはc(min, max)と指定してください")
    }

    if(is.character(zlim)){
      	zlim <- NA
    }else if(is.numeric(zlim)==TRUE&length(zlim)==2){
     	zlim <- zlim
    }else{
     	stop("zlimにはc(min, max)と指定してください")
    }

    if(is.na(xlim) && is.na(ylim) && is.na(zlim)){
    	plot3d(linesM, type="l", col="black", main=main, xlab="PC1", ylab="PC2", zlab="PC3")
    	points3d(Xp, size=size, col=col, lwd=lwd)
    }else{
        plot3d(linesM, type="l", col="black", main=main, xlim=xlim, ylim=ylim, zlim=zlim, xlab="PC1", ylab="PC2", zlab="PC3")
        points3d(Xp, size=size, col=col, lwd=lwd)    	
    }


    
    
    
    # 複数を描画する
　　

   # 視点のアングルを指定する関数
  if(fix==TRUE){
    rgl.viewpoint(theta=theta, phi=phi, fov=fov)
  }

	
}

#==============================================================================
