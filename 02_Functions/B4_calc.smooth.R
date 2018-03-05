#====================================================
# plot.smoothの定義
# 引数
#  smoothing_calc : smoothing_clacで返されたオブジェクトを指定する
#  PCA_each_day   : PCA_each_dayで返されたオブジェクトを指定する
#  tms.prm        : 主成分の点を結ぶ長さを定数倍するための引数　実数を一つ指定する
#  num            : 主成分の点を描画する日付の数を指定。datefuncで計算すると楽
#  PC1.size       : 第１主成分の点の大きさを指定する。第２主成分も同様
#  pcol           : 主成分の点の色を指定する。デフォルトは黒。monthと指定すると月ごとに
#                   dayと指定すると日毎にグラデーションする。単色での指定も可能。
#  lcol           : 主成分の点を結ぶ線の色を指定する。単色のみ。
#  filter.num     : 主成分の値を時系列に沿って移動平均をとる時の点の数を指定。
#                   デフォルトは１、つまりそのまま。
#  leap           : 閏年を含めるかを指定。TRUEで閏年を含める。
#  xlab           : x軸のラベルを指定, y軸, z軸も同様


calc.smooth <- function(smoothing_calc_result, PCA_each_day, tms.prm=0.2, num, PC1.size=5, PC2.size=2, 
                        pcol="black", lcol="grey", filter.num=1, leap=TRUE, xlab=FALSE, ylab=FALSE, zlab=FALSE){ 
	if(ncol(smoothing_calc_result)!=5){
		stop("smoothing_calcの列は　月　日　データ１　データ２　データ３　の５列のみにして下さい")
	}
	
	# 輪の点、つまりスムージングした値を取り出す
	# test : rslt.mat <- smoothingRslt ; pcol <- 'day'
	rslt.mat  <- smoothing_calc_result
	
	# PCAの結果のリストを取り出す
	# test : PCA.temp <- PCArslt[[2]]
	PCA.temp      <- PCA_each_day$Day.PCA.list

	# 主成分の始点を輪の対応する点に変換した行列を格納するリストを作成
	PCA.move.list <- list(0)
	
	X1 <- matrix(nrow=nrow(rslt.mat),ncol=2)
	X2 <- matrix(nrow=nrow(rslt.mat),ncol=2)
	X3 <- matrix(nrow=nrow(rslt.mat),ncol=2)
	
	ALL_PC1 <- data.frame(matrix(nrow=nrow(rslt.mat), ncol=3))
	ALL_PC2 <- data.frame(matrix(nrow=nrow(rslt.mat), ncol=3))
	
	# 1年の日数分繰り返す
	# leap=FALSEの場合2/29を飛ばす
	# if(leap==FALSE){
	#   # num_list <- num_list[-60]
	#   rslt.mat <- rslt.mat[-60,]
	# }
	
	num_list <- 1:dim(rslt.mat)[1]

	for(i in num_list){
	  
	  # i日目の輪の値を取り出す
	  temp  <- rslt.mat[i,c(1,2,3)]
	  
	  # i日目のPCAの結果の標準偏差(各固有値の平方根)を取り出す
	  sdev <- PCA.temp[[i]]$sdev
	  
	  # PC1,PC2に対応する標準偏差をかけたものを２で割り、さらにtms.prm(両者一律の定数倍)をかけたベクトル
	  PC1  <- (((PCA.temp[[i]]$rotation[,1]*sdev[1])/2)*tms.prm)
	  PC2  <- (((PCA.temp[[i]]$rotation[,2]*sdev[2])/2)*tms.prm)
	  
	  ALL_PC1[i,] <- PC1
	  ALL_PC2[i,] <- PC2
	  
	  # 輪の値からPC1,PC2のプラスとマイナス方向にそれぞれ半分進んだベクトルを作る
	  plus.PC1  <- temp + PC1
	  plus.PC2  <- temp + PC2
	  minus.PC1 <- temp - PC1
	  minus.PC2 <- temp - PC2
	  
	  # temp, PC1, PC2, minus.PC1, minus.PC2 をそれぞれ行ベクトルに持つ行列を作成
	  temp <- data.frame(rbind(rslt.mat[i,c(1,2,3)], plus.PC1, minus.PC1, plus.PC2, minus.PC2))
	  rownames(temp) <- c("circle", "PC1/2", "-PC1/2", "PC2/2", "-PC2/2")
	  
	  
	  # PC1, PC2の値をまとめる
      X1[i,c(1,2)] <- c(PC1[1], PC2[1])
      X2[i,c(1,2)] <- c(PC1[2], PC2[2])
      X3[i,c(1,2)] <- c(PC1[3], PC2[3])
      	  
	 # 作った行列をリストのi番目に格納
	 PCA.move.list[[i]] <- t(temp)
	}
　
	# rg1,2,3.matの列名を変更する
	colnames(X1) <- c("PC1","PC2")
	colnames(X2) <- c("PC1","PC2")
	colnames(X3) <- c("PC1","PC2")
	
	# ALL_PC1,2それぞれの値について移動平均を取る
	
	m <- filter.num
	
	#filt.X1_1 <- filter(X1[,1], filter=c(rep(1,m)/m),circular=TRUE)
	#filt.X1_2 <- filter(X2[,2], filter=c(rep(1,m)/m),circular=TRUE)
	
	#filt.X2_1 <- filter(X1[,1], filter=c(rep(1,m)/m),circular=TRUE)
	#filt.X2_2 <- filter(X2[,2], filter=c(rep(1,m)/m),circular=TRUE)
	
	#filt.X3_1 <- filter(X3[,1], filter=c(rep(1,m)/m),circular=TRUE)
	#filt.X3_2 <- filter(X3[,2], filter=c(rep(1,m)/m),circular=TRUE)
	
	
	filt.PC1_1 <- stats::filter(ALL_PC1[,1], filter=c(rep(1,m)/m),circular=TRUE)
	filt.PC1_2 <- stats::filter(ALL_PC1[,2], filter=c(rep(1,m)/m),circular=TRUE)
	filt.PC1_3 <- stats::filter(ALL_PC1[,3], filter=c(rep(1,m)/m),circular=TRUE)

	filt.PC2_1 <- stats::filter(ALL_PC2[,1], filter=c(rep(1,m)/m),circular=TRUE)
	filt.PC2_2 <- stats::filter(ALL_PC2[,2], filter=c(rep(1,m)/m),circular=TRUE)
	filt.PC2_3 <- stats::filter(ALL_PC2[,3], filter=c(rep(1,m)/m),circular=TRUE)	

	pr.filt.PCA <- list(0)
	PCA.filtered <- list(0)
	
	for(i in 1:nrow(rslt.mat)){
		temp.filt <- rslt.mat[i,c(3:5)]
		
		PC1.filt  <- c(filt.PC1_1[i], filt.PC1_2[i], filt.PC1_3[i])
		PC2.filt  <- c(filt.PC2_1[i], filt.PC2_2[i], filt.PC2_3[i])
		temp1     <- cbind(PC1.filt, PC2.filt)
		pr.filt.PCA[[i]] <- temp1
		
		# 移動平均の値を中心にしたPC1, PC2の始点と終点の座標を計算
		fplus.PC1  <- temp.filt + PC1.filt
		fplus.PC2  <- temp.filt + PC2.filt
		fminus.PC1 <- temp.filt - PC1.filt
		fminus.PC2 <- temp.filt - PC2.filt
		
		# smoothingの値とPC1,PC2のずらした値を行列にまとめる  
		temp2 <- data.frame(filt.val=t(temp.filt), fplus.PC1=t(fplus.PC1), fminus.PC1=t(fminus.PC1), fplus.PC2=t(fplus.PC2), fminus.PC2=t(fminus.PC2))
		colnames(temp2) <- c("smooth.val","fplus.PC1","fminus.PC1","fplus.PC2","fminus.PC2")
		PCA.filtered[[i]] <- temp2
	}
	
	names(pr.filt.PCA)   <- names(PCA.temp)[num_list]
	names(PCA.filtered)  <- names(PCA.temp)[num_list]
	
	
	# rg1,2,3.matをリストに格納する
	#PCA.filter.list <- list(X1=X1, X2=X2, X3=X3)
	PCA.filter.list <- list(ALL_PC1, ALL_PC2)
	
	# 以下作図 
	
	# プロットする前に作図領域をクリアにする（もしすでにプロットされていると重ね書きされてしまうことがある）
	
  # calculating month.color
	# 一年の日数分だけのベクトルを作る
	month.color <- rep(0,dim(rslt.mat)[1])
	
	# smoothing.valueの月の列を抜きだす
	temp <- rslt.mat[,1]
	
	# 一年の日数分繰り返す
	for(i in 1:length(month.color)){
		# i月の場所にはrainbow(12)のi番目の色を保存する
		month.color[i] <- rainbow(12)[temp[i]]
	}
	
	day.color <- rainbow(366)
	
		
	# 三次元プロットした際のそれぞれの軸の幅を調整する変数を作成

    # plot3dを使い時の各軸の最大値、最小値をaxis.max, axis.min に保存
    axis.max <- apply(data.frame(PCA.filtered),1,max)
    axis.min <- apply(data.frame(PCA.filtered),1,min)
　　 xlim     <- ceiling(c(axis.min[1], axis.max[1]))
　　 ylim     <- ceiling(c(axis.min[2], axis.max[2]))
　　 zlim     <- ceiling(c(axis.min[3], axis.max[3]))

　　 a     <- c(axis.min[1], axis.max[1])
　　 b     <- c(axis.min[2], axis.max[2])
　　 c     <- c(axis.min[3], axis.max[3])


    axis.value <- data.frame(xlim=a, ylim=b, zlim=c)
    row.names(axis.value) <- c("min", "max")


   if(is.logical(xlab)&&xlab==FALSE){
   	xlab=""
   }else{
   	xlab=as.character(xlab)
    }

   if(is.logical(ylab)&&ylab==FALSE){
   	ylab=""
   }else{
   	ylab=as.character(ylab)
    }

   if(is.logical(zlab)&&zlab==FALSE){
   	zlab=""
   }else{
   	zlab=as.character(zlab)
    }

   if(pcol=="day"){
        mycol  <- day.color
     }else if(pcol=="month"){
        mycol  <- month.color
     }else if(is.character(pcol)&&length(pcol)==1){
        mycol  <- rep(pcol,366)
     }else if(is.numeric&&length(pcol)==1){
        mycol <- rep(floor(pcol),366)
     }else{
        stop("pcolの値が不正です")
     }
 
    
    if(is.logical(leap)==FALSE){
    	stop("leapには論理値を指定して下さい")
    }else if(leap==FALSE){
    	rslt.mat <- rslt.mat[-60,]
    	
    	if(length(which(num==60))!=0){
    		num <- num[-which(num==60)]
    	}   	
    }
    
    
	
	# 輪をプロットする	
    #require("rgl")

	#plot3d(rslt.mat[,3],rslt.mat[,4],rslt.mat[,5],xlab=xlab,ylab=ylab,zlab=zlab, col=mycol , xlim=c(axis.min[1],axis.max[1]), ylim=c(axis.min[2],axis.max[2]), 	zlim=c(axis.min[3],axis.max[3]))
	
	# PCAとMeanが両方共TRUEのとき、PCAをFALSEにする. 二つともの重ね書きは許さない。

	# 輪に対応するnumで決めた日付の第１、第２主成分をプロットする
		#for(i in num){
		  # i日目のtemp.listの要素を抜き出す
		  #PC1  <- t(PCA.filtered[[i]][,c(2,3)])
		  #PC2  <- t(PCA.filtered[[i]][,c(4,5)])
		  
		  # i日目の第１主成分をプロットする  点のサイズを第２主成分より大きくしている
		  #points3d(PC1, size=PC1.size, color=mycol[i])
	  
		  # i日目の輪の点と第１主成分の点を線で結ぶ
		  #lines3d(PC1,color=lcol)
	  
		  # i日目の第２主成分をプロットする  点のサイズを第１主成分より小さくしている
		  #points3d(PC2, size=PC2.size, color=mycol[i])
	  
		  # i日目の輪の点と第１主成分の点を線で結ぶ
		  #lines3d(PC2,color=lcol)
		  #}
	
	return.list <- list(axis.value=axis.value, PCA.filtered=PCA.filtered, rslt.mat=rslt.mat, xlab=xlab, ylab=ylab, zlab=zlab, axis.min=axis.min, axis.max=axis.max, mycol=mycol)	
    invisible(return.list)

}