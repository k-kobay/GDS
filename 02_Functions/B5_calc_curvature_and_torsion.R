mycalc_3dCT <- function(x){ 
  # calc diff
  d1_mat  <- t(apply(cbind(x, x[c(2:nrow(x),1),]), 1, function(xx){ 
    xx[4:6] - xx[1:3]
  }))
  d1_mat2 <- t(apply(cbind(d1_mat, d1_mat[c(2:nrow(d1_mat),1),]), 1, function(xx){
    (xx[1:3] + xx[4:6])/2
  }))
  d2_mat  <- t(apply(cbind(d1_mat, d1_mat[c(2:nrow(d1_mat),1),]), 1, function(xx){ 
    xx[4:6] - xx[1:3] 
  }))
  d3_mat  <- t(apply(cbind(d2_mat, d2_mat[c(2:nrow(d2_mat),1),]), 1, function(xx){ 
    xx[4:6] - xx[1:3]
  }))
  d3_mat2 <- t(apply(cbind(d3_mat, d3_mat[c(2:nrow(d3_mat),1),]), 1, function(xx){
    (xx[1:3] + xx[4:6])/2 
  }))
  
  # cbind each axis
  XX <- cbind(x[,1], d1_mat2[,1], d2_mat[,1], d3_mat2[,1])
  YY <- cbind(x[,2], d1_mat2[,2], d2_mat[,2], d3_mat2[,2])
  ZZ <- cbind(x[,3], d1_mat2[,3], d2_mat[,3], d3_mat2[,3])
  
  # calculating each term
  term1 <- (ZZ[,3]*YY[,2] - YY[,3]*ZZ[,2])
  term2 <- (XX[,3]*ZZ[,2] - ZZ[,3]*XX[,2])
  term3 <- (YY[,3]*XX[,2] - XX[,3]*YY[,2])
  term4 <- (XX[,2]^2 + YY[,2]^2 + ZZ[,2]^2)
  
  # curvature
  curvature <- sqrt( term1^2 + term2^2 + term3^2 ) / (term4^{(3/2)})
  
  # torsion
  torsion   <- {(XX[,4]*term1) + (YY[,4]*term2) + (ZZ[,4]*term3)}/{ term1^2 + term2^2 + term3^2 }
  
  # length
  tmp1           <- cbind(x, x[c(2:nrow(x),1),])
  rownames(tmp1) <- c()
  c_length       <- sum(apply(tmp1, 1, function(x){ dist(rbind(x[1:3], x[4:6])) }))
  
  # return
  return(list(length=c_length ,curvature = curvature, torsion = torsion))
}