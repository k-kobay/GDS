#==============================================================================
# 006. dPathLengthAbの定義
# aPathAlphaAb.R : 各データ点間のαβ距離を計算
#
# 関数定義
#
dPathLengthAb <- function(D_origin, alphas, beta, nargout=1, cpp=TRUE) {
  # [INPUT]
  # D_origin: an object of class "dist"
  #           Value Inf is for non-adjacent pair of nodes.
  #
  # alphas:= c(alpha, alpha0) 
  #         alpha is the alpha we would like to analze with.
  #         alpha0 is used to make an initial geodesic graph (default is alpha0=0).
  #         e.g.) if alphas=c(-2,-1), the geodesic graph for alpha=-1 is used with
  #         the initial graph by alpha=-2.
  #
  #         The geodesic graph must shrink with smaller alpha but the
  #         opposite can occur because of the numerical error of dijekstra method
  #         for a very small value of alpha.
  #
  # D_alpha,beta(P,Q) = f_beta[sum_i d(p_i,p_{i+1})^(1-alpha)]
  # which is the sum of the scalded length of the edges along the
  # shortest path from P to Q
  # w.r.t. d^(1-alpha) (therefore, w.r.t. f_beta(d^(1-alpha)) )
  #
  # [OUTPUT]
  # if nargout=1(default)
  #     an object D of class "dist" with the deformed metric d_alpha,beta,gamma
  #     Here, d_alpha,beta,gamma(P,Q) = Sum_i f_beta[d(p_i,p_{i+1})^(1-alpha)]^gamma
  #     Use squareform(D) for the corresponding distance matrix.
  # if nargout=2
  #     A list with two components:
  #         1. an object of class "dist" with the deformed metric d_alpha,beta,gamma (save as above)
  #         2. a matrix used_edges: used_edges(i,j)=1 if the edge(i,j) appears in dijkstra method and =0 otherwise.
  #
  # # This program uses Floyd algorithm 'floyd.R'.
  
  # source('floyd.R')
  
  if (missing(D_origin) || missing(alphas) || missing(beta)) {
  	stop('We need three input variables! (except nargout)')
  }
  
  if (length(alphas)<=0 || length(alphas)>2) {
      stop('Size of alphas must be 1 or 2!')
  }
  
  if (!is.matrix(D_origin)) {
  	D_origin <- as.matrix(D_origin)
  }
  
  n <- nrow(D_origin);
  
  if (beta<0) {
      stop('beta must be non-negetive.')
  }
  
  # alphasの一番最後の要素を持ってくる
  alpha <- alphas[length(alphas)]
  
  # MAIN PART
  
  # D_origin の各要素を(1-alpha)乗する    
  D_a0 <- D_origin^(1-alpha)
  M <- list(D_a0)
  
  # D_a0の発散した要素に Inf を代入する
  D_a0[which(is.infinite(D_origin))] <- Inf
  
  # D_a0について最短距離を計算する
  if(alpha==0){
  	# alphaが0であれば1乗なのでそのまま
  	D_a <- D_a0
  }else{
  	# alphaが0でなければ以下を計算
    if(cpp){
      D_a <- rcpp_floyd(D_a0)
    }else{
      D_a <- floyd(n,D_a0)
    }
  }
  
  
  # beta が発散しているかどうかで場合分け
  if (is.infinite(beta)) {
  	# betaが発散しているなら D_a を D_abとする
      D_ab <- D_a
  } else {
  	# betaが発散していないなら D_ab を以下のように計算
      D_ab <- sin(pi*pmin(D_a, beta)/(2*beta)) # min -> pmin
  }
  
  # D_ab の対角要素が1の場所があればそれを0で置き換える
  D_ab[which(diag(n)==1)] <- 0;  # set the diagonal elements of D_ab zero.
  
  # FOR THE GEODESIC GRAPH
  
  # nargoutが2 または alphasの最大と最小が同じ という条件で場合分け
  if (nargout==2 || min(alphas)!=max(alphas)) {  # (specify nargout by the last parameter)
  	
  	# D_used にD_a==D_a0の論理値を代入
      #D_used <- (D_a==D_a0)
      D_used <- (D_a==M[[1]])
      
      # D_used0 に D_usedを代入
      D_used0 <- D_used
      
      # 下三角行列に変換する。（対角要素もゼロにする)
      D_used0[lower.tri(D_used0,diag=T)] <- FALSE; # Set the lower triangle and diagonal zero.
      used_edges <- which(D_used0==TRUE, arr.ind=T)
  }
  
  # alphas の最大値と最小値の値が同じかどうかで分岐
  if (min(alphas)==max(alphas)) {
  	
  	# nargout==2 で分岐
      if (nargout==2) {
      	
      	# alphasの最大値と最小値が同じ　かつ　nargout==2 が真の時
      	# D_abをdist型に変換したものと、used_edgesのリストを返す
          return.list <- list(as.dist(D_ab), used_edges)
          names(return.list) <- c("D_ab","used_edges")
          return(return.list)
          
      } else {
      	
      	# nargout==2が偽の時
          # D_abをdist型にして返す
          return(as.dist(D_ab))
      }
      
  } else {
  	
  	# alphasの最大値と最小値が異なる時
      # alphas を alphasの第一要素とする
      alphas <- alphas[1]
      
      # D_alpha_graph に D_originを代入
      D_alpha_graph <- D_origin
      
      # D_alpha_graph の D_usedが偽の要素に Inf を代入（最短距離で使われない辺をInfにする)
      D_alpha_graph[which(D_used==FALSE)] <- Inf
      
      # dpathLengthAb関数に引数 D_alpha_graph, alphas, beta, 2 を与えた実行結果を返す
      # alphasはこの時点で一つの要素しかないので必ず２回の実行で終了する
      return(dPathLengthAb(D_alpha_graph, alphas, beta, 2, Euclidian=Euclidian))
      print("here!!")
  }

}

# 実行
# beta=Inf に固定
# rslt.dPLAb <- dPathLengthAb(D_origin=dmat.1931, alphas=c(1,0) , beta=Inf, nargout=1)

#===========end================================================================
