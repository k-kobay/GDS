# 引数
# point_matrix, 列:x, y, degree, optim (0,1,flag)

search_max_degree <- function(point_matrix){
  # 次数の一番高い点が複数あれば中心からの距離を計算
  test_degree <- point_matrix$degree
  num1  <- which(point_matrix$optim==1)
  if(length(num1)!=0){
    # test1[num1] <- NA
    test_degree[num1] <- NA
  }
  
  test1 <- test_degree==max(test_degree, na.rm=TRUE)
  
  if(sum(test1, na.rm=TRUE)!=1){
    # 平均座標
    center <- as.numeric( lapply(data.frame(point_matrix$x, point_matrix$y), function(x){mean(x, na.rm=TRUE)}) )
    
    # 次数が一番高い点の座標
    num1 <- which(test1)
    tmp1 <- matrix(c(point_matrix$x[num1], point_matrix$y[num1]), ncol=2, nrow=length(num1))
    tmp2 <- matrix(rep(center, each=length(num1)), ncol=2, nrow=length(num1))
    
    dist_max <- (apply((tmp1 - tmp2)^2, 1, function(x){ sqrt(sum(x)) }))
    
    fix_number <- num1[which.min(dist_max)]
  }else{ # なければ一番高い次数を選ぶ
    fix_number <- which.max(test_degree)
  }
  
  return(fix_number)
}
