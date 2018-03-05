# 使う変数
# 最適化する点p0, p0に隣接する点, 隣接行列(delauney triangle) , 対応する相関係数行列

# point_matrix <- data.frame(x=tmpStation$longitude, y=tmpStation$latitude)
# delauney_matrix <- df1[,c(1:6)] # 隣接行列 delauney の結果の行列を代入すると. ind1, ind2の列が必要
# fix_number <- 7 # 最適化点
# Cor_Value  <- tmpCor


my_MDS <- function(point_matrix, delauney_matrix, fix_number, Cor_Value, x_ratio=sin(pi*(90-35)/180)){
  
  #------------------
  # データフレーム -> リストへの変換
  Zip <- function(...) Map(list, ...)
  
  # リスト -> データフレムへの変換
  Unzip <- function(...) rbind(data.frame(), ...)
  #------------------
  
  # 変数名を扱いやすいものに変更
  pMat <- point_matrix
  dfp  <- delauney_matrix
  fnum <- fix_number
  CV   <- Cor_Value
  
  # 隣接行列から最適化点に隣接しているものを抜き出す
  edge_num <- which(dfp$ind1 == fnum | dfp$ind2 == fnum)
  tmp1     <- data.frame(dfp$ind1, dfp$ind2)[edge_num,]
  
  # 隣接する頂点の行番号
  adj_point <- tmp1[tmp1 != fnum]
  
  # 隣接する頂点の座標と次数
  adj_mat <- pMat[adj_point, ]
  
  # 最適化点の座標
  p0　<- pMat[fnum,]
  
  # p0の隣接点が成すサイクル
  cycle_num <- dfp$ind1 %in% adj_point & dfp$ind2 %in% adj_point
  cycle_mat <- dfp[cycle_num, ]
  
  # for(ee in 1:nrow(cycle_mat)){
  #  if(ee==1){
  #     plot(x=c(cycle_mat[ee,1], cycle_mat[ee,3]), y=c(cycle_mat[ee,2], cycle_mat[ee,4]), type="l",
  #          xlim=range(cycle_mat[,c(1,3)]), ylim=range(cycle_mat[,c(2,4)]), col=ee)
  #   }else{
  #     lines(x=c(cycle_mat[ee,1], cycle_mat[ee,3]), y=c(cycle_mat[ee,2], cycle_mat[ee,4]), col=ee)
  #   }
  # }
  # points(p0, col="black", pch=15)
  # all_point <- unique(data.frame(x=c(cycle_mat[,1],cycle_mat[,3]), y=c(cycle_mat[,2], cycle_mat[,4])))
  # points(all_point, pch=as.character(1:6))
  
  
  # 線分にかんする判定を作成
  # 傾きとy切片を求める
  #---------------------------
  ab_mat2 <- t(apply(cycle_mat, 1, FUN=function(x){
    tmp1 <- x[c(1,3)]
    xx   <- data.frame(1,tmp1)
    
    if(xx[1,2]==xx[2,2]){
      ab <- c(NA, NA)
    }else{
      tmp2 <- c(x[2],x[4])
      ab   <- solve(xx,tmp2)
    }
  }))
  
  ab_mat2 <- data.frame(ab_mat2)
  colnames(ab_mat2) <- c("intercept", "slope")
  #---------------------------
  #---------------------------
  # 線分テストの条件について
  # p0のx座標を入れた時のf(x)の値とp0のy座標比べれば良い

  #---------------------------
  #---------------------------
  ab_list <- do.call(Zip, ab_mat2)
  tmp2 <- lapply(ab_list, FUN=function(x){
    ty <- x[[1]] + x[[2]] * p0$x
    r  <- c()
    if(is.na(ty)){
      r <- NA
    }else if(ty < p0$y){
      r[1] <- "over"
    }else if(ty == p0$y){
      r[1] <- "euql"
    }else if(ty > p0$y){
      r[1] <- "under"
    }
  })
  tmp3 <- as.character(do.call(Unzip, tmp2)[,1])
  #---------------------------
  cycle_mat$test <- tmp3
  ab_mat2$test   <- tmp3
  
  nn2 <- 100 # 分割数
  x_range <- range(pMat$x[c(adj_point, fnum)])      # x-axis(longitude)
  xx <- seq(x_range[1], x_range[2], length.out=nn2) # x-axis nn2 partition
  
  y_range <- range(pMat$y[c(adj_point, fnum)])      # y-axis(latitude)
  yy <- seq(y_range[1], y_range[2], length.out=nn2) # y-axis nn2 partition
  
  xx_mat <- matrix(rep(xx, nn2), length(xx), nn2)      # x value matrix for matplot()
  yy_mat <- matrix(rep(yy, each=nn2), length(yy), nn2) # y value matrix for matplot()
  
  ttt <- ab_mat2$slope[1] * xx_mat[,50] + ab_mat2$intercept[1] # linear equation value
  yy_mat[,50] < ttt
  
  test_mat <- rbind(xx_mat, yy_mat)
  
  xx_mat2 <- xx_mat
  yy_mat2 <- yy_mat
  
  #===============================
  # 凸方じゃない場合ここの領域を狭めるプログラムが
  # 上手くいかないのであとで考える
  # test_res_list <- list()
  # for(i in 1:nrow(ab_mat)){
  #  tmp_ab <- ab_mat[i,]
  #  tmp2 <- apply(test_mat, 2, function(x){
  #    xxx <- x[1:nn2]
  #    yyy <- x[(nn2+1):(nn2*2)]
  #    ttt <- tmp_ab$intercept + tmp_ab$slope * xxx
  #    
  #    if(tmp_ab$test=="under"){
  #      ttt > yyy
  #    }else if(tmp_ab$test=="over"){
  #      ttt < yyy
  #    }
  #  })
  #  
  #  xx_mat2[!tmp2] <- NA
  #  yy_mat2[!tmp2] <- NA
  #  test_res_list[[i]] <- t(tmp2)
  #  
  #}
  #===============================
  
  # matpoints(xx_mat2, yy_mat2, pch=19, col="green", cex=0.3)
  
  XX <- xx_mat2
  dim(XX) <- c(length(xx_mat2), 1)
  
  YY <- yy_mat2
  dim(YY) <- c(length(yy_mat2), 1)
  
  XXYY <- data.frame(x=XX, y=YY)
  # plot(XXYY, pch=18)
  adj_Cor <- Cor_Value[c(fnum, adj_point), c(fnum,adj_point)]
  use_Cor <- adj_Cor[1,-1]
  
  #----------------------
  # x <- XXYY[188, ]
  tmp0 <- dfp[adj_point,]
  result1 <- (apply(XXYY, 1, function(x){
    tmp1 <- matrix(rep(x, each=nrow(adj_mat)), nrow=nrow(adj_mat), ncol=2)
    tmp2 <- cbind(adj_mat$x, adj_mat$y, tmp1)
    # xx <- tmp2[1,]
    tmp3 <- apply(tmp2, 1, function(xx){ 
      xx <- as.numeric(xx)
      sqrt( {(xx[1]-xx[3])*x_ratio}^2+{xx[2]-xx[4]}^2 ) }) - use_Cor
    tmp4 <- sum(tmp3)
    return(tmp4)
  }))
  #----------------------

  
  min_num <- which(result1==min(result1, na.rm=TRUE))
  XYZ <- data.frame(XXYY, result1)
  # XYZ[min_num,]
  if(length(min_num)!=1){
    min_point <- as.numeric(XYZ[min_num[1],])
  }else{
    min_point <- as.numeric(XYZ[min_num,])
  }
  
  
  names(min_point) <- c("x", "y", "min_value")
  r <- list(min_point=min_point)
  
  return_list <- list(adj_mat=adj_mat, pMat=pMat, xx_mat=xx_mat, 
                      yy_mat=yy_mat, xx_mat2=xx_mat2, yy_mat2=yy_mat2, 
                      min_point=min_point, ab_mat=ab_mat2, fix_number=fnum)
  
  # points(XXYY[min_num, ], col="orange", pch=18, cex=2)
  return(return_list)  
  
  # confirm plot
  # plot(adj_mat$x, adj_mat$y, type="n")
  # text(adj_mat$x, adj_mat$y, labels=adj_point)
  # points(pMat[fnum,]$x, pMat[fnum,]$y, pch=19, col="red")
  
  # for(i in 1:nrow(ab_mat2)){
  #   abline(ab_mat2[i,1], ab_mat2[i,2], col="red")
  # }
  
  # matpoints(xx_mat, yy_mat, pch=19, col="blue", cex=0.2)
  # matpoints(xx_mat2, yy_mat2, pch=19, col="green", cex=0.3)
  
  # points(XXYY[min_num, ], col="orange", pch=18, cex=2)
  
} # function end