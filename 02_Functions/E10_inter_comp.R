inter_comp <- function(x1, x2, eps){
  tmpX  <- rbind(1, c(x1[1],x2[1]))
  tmpY  <- c(x1[2], x2[2])
  tmpAB <- as.numeric(t(tmpY) %*% solve(tmpX)) # c(intercept, slope)
  inter_value <- {eps}/{sqrt(1+tmpAB[2]^2)}
  tmpxx <- seq(from=min(c(x1[1],x2[1])), to=max(x1[1], x2[1]), by=inter_value)
  tmpyy <- tmpAB[1] + tmpAB[2]*tmpxx
  
  return(cbind(tmpxx[-1], tmpyy[-1]))
}
