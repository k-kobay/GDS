triangle_move <- function(p1, q1, r1, p2, q2, r2, v){
  A <- c(c(v[1] - p1[1]), c(v[2] - p1[2]))
  B <- matrix(c(q1 - p1,r1 - p1), 2, 2, byrow=TRUE)
  # tmp1 <- q1 - p1
  # tmp2 <- r1 - p1
  # B <- rbind(tmp1, tmp2)
  
  B_inv <- 1/(B[1,1]*B[2,2] - B[1,2]*B[2,1]) * matrix(c(B[2,2], -B[1,2], -B[2,1], B[1,1]), 2, 2, byrow=TRUE)
  
  # B_inv <- solve(B)
  
  st <- as.numeric(t(A) %*% B_inv)
  
  tmp1 <- q2 - p2
  tmp2 <- r2 - p2
  C    <- rbind(tmp1, tmp2)
  
  w <- t(st) %*% C + p2
  
  r <- list(st=st, w=w)
  
  return(r)
}