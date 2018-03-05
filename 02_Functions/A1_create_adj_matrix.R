#==============================================================================
# create.adj.matの定義
#
# アメダスデータについて、年ごとに距離行列を作成する関数 (ユークリッド距離)の作成
# 引数
# year     : 1976~2015の値を指定する（閏年かどうかは関数の中で考慮される）
# distance : TRUE or FALSE の論理値を指定 TRUEであれば距離行列も計算する
#
# 関数定義
create.adj.matrix <- function(AllData, period, year, distance=TRUE){
  if(!is.logical(distance)){
    stop("distanceの値が論理値ではありません")
  }

  # 閏年のリスト
  All_leap <- seq(1584, 2017, 4)
  
  # 閏年に該当する年数のベクトルを作成(1931~2015の間で)
  # leap.year <- seq(1976, 2015, 4)
  
  # 閏年の場合の存在しない日付
  # omit.leap <- c(61,62,124,186,279,341)
  
  # 閏年でない場合の存在しない日付
  # omit.nonleap  <- c(60,61,62,124,186,279,341)
  
  # AllDataのyearに該当する行を抽出
  tmp  <- AllData[which(AllData$Year==year),]
  tmp2 <- tmp[,-c(1:3)]
  
  # 閏年であるかどうかで分岐
  if(sum(All_leap==year)==1){
    
    # 閏年の場合の存在しない日付を削除
    # tmp <- tmp[-omit.leap,]
    leap <- TRUE
    
  }else{
    
    # 閏年でない場合の存在しない日付を削除
    # tmp <- tmp[-omit.nonleap,]
    leap <- FALSE
  }
  
  
  if(distance==FALSE){
    return.list <- list(tmp, leap)
    names(return.list) <- c(paste("Data", year, sep="."), "leap")
    return(return.list)
    print("distanse is FALSE")
  }
  
  # 距離行列の作成
  distance.matrix <- dist(tmp2)
  
  return.list <- list(assign(paste("mat",year,sep="."), tmp), distance.matrix, leap)
  names(return.list) <- c(paste("Data", year, sep="."), "distance.matrix", "leap")
  return(return.list)
  
}
#==============================================================================
