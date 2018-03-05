# p:point p <- c(x, y)
# v:vertices n*2 matrix column1 = x, column2 = y
wn_test <- function(p, v){
  test_value <- 0
  v <- v[c(1:nrow(v),1), ]
  for(vi in 1:(nrow(v)-1)){ 
    vv <- v[c(vi, vi+1) ,] # 多角形vをなす辺を一つ選ぶ
    # y=y(p)と交差しているかどうか
    # まず y=0とするために、2点のy座標からy(p)を引く
    # snが負なら交差している可能性がある
    sn <- (vv[2,2]-p[2]) * (vv[1,2]-p[2])
    if(sn < 0){ # 負なら次の判定へ
      # y=y(p) & x>x(p) の直線と交差しているかどうか
      if({vv[1,1]>p[1]}&&{vv[2,1]>p[1]} == TRUE){ # x座標が両方正なら y=y(p) & x>x(p) を通る
        if({vv[2,2]-vv[1,2]} > 0){
          test_value <- test_value + 1 # 上向きの辺ならプラス1
        }else{
          test_value <- test_value - 1 # 下向きの辺ならマイナス1
        }
      }else if({vv[1,1]<p[1]}&&{vv[2,1]<p[1]}){# x座標が両方負なら y=y(p) & x>x(p) を通らない
        test_value = test_value
      }else{
        # x座標の符号が異なる場合のy=y(p)となるx座標を求める
        vv2 <- vv
        vv2[,2] <- vv[,2] - p[2] # y=0となるx座標を求めるように変換
        x_test2 <- vv2[1,1] - vv2[1,2]*{(vv2[2,1]-vv2[1,1])/(vv2[2,2]-vv2[1,2])}
        if(x_test2>p[1]){ # y=0となるx座標がx(p)よりも大きければ交差している
          if({vv[2,2]-vv[1,2]} > 0){
            test_value <- test_value + 1 # 上向きの辺ならプラス1
          }else{
            test_value <- test_value - 1 # 下向きの辺ならマイナス1
          }
        }
      }
    }
    
  }
  return(test_value)
}