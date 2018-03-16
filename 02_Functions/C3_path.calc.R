#==============================================================================
# 007. path.calcの定義
#    指定したalpha,beta,gammaについて最短経路を求める関数
#
# 関数定義
#  year : 1931 ~ 2015 までの間のどれかを１つ指定
#  alphas : 計算したいalphaを指定、ベクトル形式で複数指定可能
#  beta   : デフォルトInf, 実数を一つ指定
#  gamma  : 整数を指定
path.calc <- function(AllData, year, alphas, beta=Inf, gamma=1, scale=FALSE, cpp=TRUE){
	
	
	# 降水量の全データから該当する年の行列を作成
  X    <- AllData %>% dplyr::filter(Year %in% year)
	leap <- year %in% seq(1584, 2017, 4)
	X    <- X[,-c(1:3)]
	
	# 基準化
    if(scale==TRUE){
        X <- X/mean(sqrt(rowSums(X^2)))    	
    }
	
	# 行数を保存
	n <- nrow(X)
	
	# Xの距離行列を計算しD_originとする
    D_origin <- as.matrix(dist(X))

    # 空の varlist というオブジェクトを作成
    varlist <- c()

    # 結果を格納するリスト
    D_alpha_list <- list()
    SPL_list     <- list()

    #library(tcltk)
    #pb <- txtProgressBar(min=1, max=length(alphas), style=3)
    for (i in 1:length(alphas)) { # to avoid round-off errors
    	
    	# alpha_str に alpha の文字列を "-" を "m"　に、"."を"d"にそれぞれ変換したものを格納 
        alpha_str <- chartr("-.", "md", alphas[i])
        
        # alpha_strを表示する
        # print(alpha_str)
              
        # D にDpathLengthAbの実行結果を保存
        D <- dPathLengthAb(D_origin=D_origin, alphas=alphas[i], beta=beta, nargout=2, cpp=cpp)
        
        # Dをγ乗し列ごとに合計した値を格納
        tmp  <- colSums(as.matrix(D[[1]])^gamma)
        SPL  <- c(sum=sum(tmp)/(length(tmp)^2), min=min(tmp)/length(tmp))
        
        SPL_list[[i]] <- SPL
        
        # SPLの和と最小値を計算する
        
        # varname に以下の名前を保存
		    varname <- paste0('D_alpha_', alpha_str)
		
		    # オブジェクトDを リストに格納
        D_alpha_list[[i]] <- D
        
        # varlist に varlistの後にvarnameをくっつけたベクトルを格納
		    varlist <- append(varlist, varname)
		
		    # Dを削除する
        rm(D)
        
        # i の数を1増やす
        #setTxtProgressBar(pb, i)
    }
        # D_alpha_listの名前を変更する
		    names(D_alpha_list) <- varlist

    # データ点を計算
    Xp <- cmdscale(D_origin,k=3)

    # Xpの列名を変更
    colnames(Xp) <- c("x", "y", "z")
	
	return.list <- list(D_alpha_list, SPL_list, leap, Xp)
	names(return.list) <- c("D_alpha_list", "SPL_list", "leap", "Xp")
	return(return.list)	
			
}
#===========end================================================================
