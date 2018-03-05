#=====================================================================
# PCA_each_day の定義
# 指定した列の各日の各年の値の行列を作成してリストに格納して返す　要素数366のリスト
# 上記で作成した行列に対するPCAの結果を格納したリストも返す
# Dataで与える行列については１列目には年、２列目には月、３列目には日の値が来るように加工してください
# colnumにはデータがある範囲で任意の数を与えられます。

PCA_each_day <- function(Data, colnum, scale=TRUE, leap=TRUE, ...){
	if(ncol(Data)<6){
		stop("Data で与える行列には１列目には年、２列目には月、３列目には日の値が来るようにして下さい")
	}
	
	X <- Data[,c(1:3,colnum)]
	
	data.num     <- length(colnum)
	Day.list     <- list()
	Day.PCA.list <- list()
	
	if(leap==TRUE){
	  omit.list <- c(-61,-62,-124,-186,-279,-341)
	}else{
	  omit.list <- c(-60,-61,-62,-124,-186,-279,-341)
	}
	
	month     <- rep(1:12, each=31)
	day       <- rep(1:31, 12)
	md_list   <- data.frame(month, day)[omit.list,]
	day.name  <- paste(month,day,sep="-")[omit.list]
	
	#------
	count <- 0
	for(row_num in 1:nrow(md_list)){
	  i <- md_list[row_num, 1] # month number
	  j <- md_list[row_num, 2] # day number
	  
	  subX <- subset(X, X[,2]==i&X[,3]==j)[,c(1,4:(3+data.num))] # create subset
	  
	  if(nrow(subX)==0){
	    count <- count
	  }else{
	    count <- count + 1
	    Day.list[[count]] <- subX
	    if(sum(is.na(subX))!=0){
	      subX <- na.omit(subX)
	    }
	    
	    if(scale==TRUE){
	      var_zero <- which(sapply(subX, var)==0)
	      
	      subX[,-c(1,var_zero)] <- apply(subX[,-c(1,var_zero)], 2, function(x){
	        scale(x)
	      })
	      
	    }
	    
	    Day.PCA.list[[count]] <- prcomp(subX[,-1])

	  }
	}
	#------
	
	
	#------
	# count <- 0
	# for(i in 1:12){
	# 	for(j in 1:31){
	# 		subX <- subset(X, X[,2]==i&X[,3]==j)[,c(1,4:(3+data.num))]
	# 		if(nrow(subX)==0){
	# 			count <- count
	# 		}else{
	# 		count <- count + 1
	# 		Day.list[[count]] <- subX
	# 		if(sum(is.na(subX))!=0){
	# 			subX <- na.omit(subX)
	# 		}
	# 		scale(subX[,-1])
	# 		# Day.PCA.list[[count]] <- prcomp(subX[,-1], scale=scale, ...)
	# 		Day.PCA.list[[count]] <- prcomp(subX[,-1], scale=FALSE)
	# 		}
  # 
	# 	}
  # }
	#------ 

    
    names(Day.list)     <- day.name
    names(Day.PCA.list) <- day.name

    return.list <- list(Day.list=Day.list, Day.PCA.list=Day.PCA.list)
    invisible(return.list)

}

# sample
# x1 <- PCA_each_day(Data=mod.uk, colnum=c(4:6))

