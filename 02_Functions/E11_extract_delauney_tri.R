# ドロネー分割を与えられた時に全ての三角形の頂点集合を返す関数
# deldir()関数の結果のdelsgsを引数に与える
extract_delauney_tri <- function(delauney, vertex_list){
  
  # 三角形を列挙する
  triangle_list <- c(NA,NA,NA)
  for(i in 1:nrow(vertex_list)){
    adj_nrow <- c(which(delauney$ind2 == i), which(delauney$ind1 == i))
    adj_num <- c(delauney$ind1[which(delauney$ind2 == i)], delauney$ind2[which(delauney$ind1 == i)])
    pair_list <- combn(x=adj_num, m=2)
    res <- apply(pair_list, 2, function(x){ 
      res_1 <- sum({delauney$ind1==x[1] & delauney$ind2==x[2]}|{delauney$ind1==x[2] & delauney$ind2==x[1]})
    })
    if(length(res)==1){
      tmp_tri <- c(i,pair_list[,which(res==1)])
    }else{
      tmp_tri <- rbind(i,pair_list[,which(res==1)])    
    }
    triangle_list <- cbind(triangle_list, tmp_tri)
  }
  colnames(triangle_list) <- c()
  tmp1 <- data.frame((t(triangle_list)[-1,]),1)
  colnames(tmp1) <- c("v1","v2","v3","flag")
  
  # 三角形を成す３点を列挙する
  test <- TRUE
  iter <- 1
  while(test){
    if(sum(tmp1$flag)==0) break
    iter <- min(which(tmp1$flag==1))
    tmp1$flag[iter] <- 0
    ttt  <- tmp1[iter,1:3]
    # omit_num <- as.numeric(which(apply(tmp1[-iter,],1,function(x){sum(x[1:3] %in% ttt)==3})))
    omit_num <- as.numeric(which(apply(tmp1,1,function(x){sum(x[1:3] %in% ttt)==3})))
    omit_num <- omit_num[-which(omit_num==iter)]
    
    if(length(omit_num)!=0){
      tmp1 <- tmp1[-omit_num,]
      rownames(tmp1) <- c()
    }
  }
  
  tmp2 <- tmp1
  rownames(tmp2) <- c()
  
  # 三角形の内部に他の頂点が存在するか判定する
  inner_test <- c()
  
  for(i in 1:nrow(tmp2)){
    t1 <- as.numeric(tmp2[i,1:3])
    b_mat <- vertex_list[t1,]
    
    test <- apply(vertex_list[-t1,], 1, function(v){
      
      A <- cbind(c(v[1] - b_mat[1,1]), c(v[2] - b_mat[1,2]))
      tmp11 <- b_mat[2,1] - b_mat[1,1]
      tmp12 <- b_mat[2,2] - b_mat[1,2]
      tmp21 <- b_mat[3,1] - b_mat[1,1]
      tmp22 <- b_mat[3,2] - b_mat[1,2]
      B <- t(matrix(c(tmp11, tmp12, tmp21, tmp22), nrow=2, ncol=2))
      
      B_inv <- solve(B)
      
      st   <- as.numeric(data.frame(A %*% B_inv))
      res     <- {sum(st>0)==2}&&{sum(st)<=1}
      return(res)
    })
    
    inner_test[i] <- sum(test)!=0
  }
  
  # ドロネー分割の三角形のリストを作成し各三角形の面積を計算する
  triangle_list <- tmp2[!inner_test, -4]
  rownames(triangle_list) <- c()
  
  return(triangle_list)
}