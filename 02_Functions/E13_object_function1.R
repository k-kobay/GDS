# 地図変形で最小化する関数
# arguments
# v_points : 頂点の座標をベクトル化したもの
# tri_list : 三角形の頂点集合を列にもつ行列
# cor_mat  : 標本相関係数行列
# adj_mat  : 隣接行列(隣接する要素に1,していない要素に0とした行列)
# pre_area : 更新前の符号付き面積を与える
# area_denom : 元々の三角形の面積のベクトルを与える
# dist_to_cor : 目的関数の距離の値を相関係数に回帰した値に変換するかどうか TRUE or FALSE
# coefs : dist_to_corで利用する回帰係数パラメータベクトル

# p0 : 変更前の三角形の頂点行列
# p1 : 変更後の三角形の頂点行列
# eg) x1, y1
#     x2, y2
#     x3, y3

# 外積の計算関数

my_outer <- function(vv){
  a <- c(vv[2,] - vv[1,]) # ベクトルの計算
  b <- c(vv[3,] - vv[1,]) # ベクトルの計算
  res <- (a[1]*b[2] - a[2]*b[1])/2
  return(res)
}


object_function <- function(v_points, tri_list, cor_mat, adj_mat, penalty="inverse power", p=-1, c=1,
                            c2=1, area_denom = 1, pre_area = 1, dist_to_cor=FALSE, coefs){
  # 行列化
  v_mat <- matrix(v_points, nrow=nrow(cor_mat), ncol=2)
  
  # 距離行列
  dist_v <- as.matrix(dist(v_mat))
  
  # 距離による相関係数の推定
  if(dist_to_cor==TRUE){
    t_dist <- {coefs[1]/(1+coefs[2]*exp(coefs[3]*dist_v))}
  }
  
  # edgeの重複排除
  adj_mat2  <- adj_mat
  adj_mat2[lower.tri(adj_mat2, diag=TRUE)] <- 0
  
  # adj_mat2によりedgeがある部分の距離を抜き出す
  if(dist_to_cor==FALSE){
    dist_edge   <- dist_v[adj_mat2==1]
  }else{
    t_dist_edge <- t_dist[adj_mat2==1]
  }
  
  
  # adj_mat2によりedgeがある部分の相関係数を抜き出す
  cor_edge  <- cor_mat[adj_mat2==1]
  
  # 全ての距離と相関係数の差の二乗の和を計算
  if(dist_to_cor==FALSE){
    first_term <- sum((dist_edge - cor_edge)^2)
  }else{
    first_term <- sum((t_dist_edge - cor_edge)^2)
  }
  
  
  # 各三角形の面積を計算
  area_list <- apply(tri_list, 1, function(x){
    v   <- as.matrix(v_mat[as.numeric(x),])
    if(penalty %in% c("inverse power", "log", "ratio", "log-ratio")){
      res <- triangle_area(v)
    }else{
      res <- my_outer(v)
    }
    return(res)
  })
  
  # 罰則項
  if(penalty=="inverse power"){
    # -1 =< p < 0 を満たしていない場合はストップ
    if(!{-1 <= p}&&{ p < 0 }){ stop("-1 <= p < 0 を満たしていません") }
    # 面積ごとに逆べきを計算し総和を取る
    penalty_term <- sum({area_list/area_denom}^{p})
  }else if(penalty=="log"){
    penalty_term <- sum(-log(area_list))
  }else if(penalty=="ratio"){
    penalty_term <- sum((area_list/area_denom)^{-1})
  }else if(penalty=="log-ratio"){
    penalty_term <- sum(-log(c2*area_list/area_denom))
  }else if(penalty=="exp-outer"){
    penalty_term <- sum( exp( -{c2}*(area_list/pre_area) )   )
  }else if(penalty=="log_exp"){
    penalty_term <- sum( log( 1 + exp(-{c2}*{area_list}/{pre_area})) )
  }else{
    stop('penaltyは"inverse power","log","ratio","exp-outer"のいずれかを指定してください')
  }
  
  # 関数値
  res <- first_term + (c*penalty_term)
  return(res)
}