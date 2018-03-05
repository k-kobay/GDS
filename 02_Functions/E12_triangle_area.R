# 三角形の面積を計算する関数を作成
triangle_area <- function(triangle_vertices){
  vv  <- triangle_vertices
  res <- (1/2)*abs( vv[1,1]*(vv[2,2]-vv[3,2]) + vv[2,1]*(vv[3,2]-vv[1,2]) + vv[3,1]*(vv[1,2]-vv[2,2]) )
  # res <- (1/2)*abs(det(cbind(as.matrix(vv), 1)))
  return(res)
}
