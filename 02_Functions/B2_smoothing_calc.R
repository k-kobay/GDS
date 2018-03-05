#===========================================================================
# smoothing_calc の定義
# 引数
# Data   : 行列を指定、1列目に年、２列目に月、３列目に日の値が来るように整理しておく
# colnum : 
#

smoothing_calc <- function(Data, colnum, type="spline", filter.num=30, k=-1, leap=TRUE){
  
  require("mgcv")

  # 引数が適切か確認
	if(is.character(type)==FALSE){
		stop("typeには spline か filter を指定して下さい")
	}else if(type!="spline"&&type!="filter"){
		stop("typeには spline か filter を指定して下さい")
	}
	
	if(is.data.frame(Data)==FALSE){
		stop("Data にはデータフレームを指定してください")
	}else if(ncol(Data)<4){
		stop("Data は1列目に年、２列目に月、３列目に日の値でなければなりません")
	}
	
	X <- Data[,c(1:3,colnum)]
	
    if(is.character(X[,1])){
    	X[,1] <- as.numeric(X[,1])
    	X[,2] <- as.numeric(X[,2])
    	X[,3] <- as.numeric(X[,3])
    }
	
	if(leap==TRUE){
	  omit.list <- c(-61,-62,-124,-186,-279,-341) 
	}else{
	  omit.list <- c(-60,-61,-62,-124,-186,-279,-341) 
	}
	
	data.num  <- length(colnum)
	month <- rep(1:12, each=31)
	day   <- rep(1:31, 12)
	mean.X <- data.frame(month=month, day=day, matrix(0, nrow=length(day), ncol=data.num))[omit.list,]

	
	# 各日の全年の平均を計算する	
	for(i in 1:12){
		for(j in 1:31){
			subX <- subset(X, X[,2]==i&X[,3]==j)
			
			if(nrow(subX)!=0){
				tmp <- as.vector(apply(subX, 2, function(x)mean(x, na.rm=TRUE))[colnum])
				mean.X[which(mean.X[,1]==i&mean.X[,2]==j), c(3:(2+data.num))] <- tmp
			}
			
		}
	}
	
	smoothing.value <- data.frame(month=month, day=day,matrix(0, nrow=length(day), ncol=data.num))[omit.list,]
	tt <- 1:(nrow(mean.X)+1)
	
	if(type=="spline"){
		for(j in (3:(2+data.num))){
		# スプラインをかけた値をsmooting. valueに保存する
		smoothing.value[,j] <- gam(mean.X[c(1:nrow(mean.X),1),j]~s(tt, bs="cc", k=k))$fitted[-(nrow(mean.X)+1)]
		}		
	}else if(type=="filter"&&is.numeric(filter.num)&&length(filter.num)==1){
		
		m <- filter.num
		
		for(j in (3:(2+data.num))){
		# スプラインをかけた値をsmooting. valueに保存する
		smoothing.value[,j] <- filter(mean.X[,j], filter=c(rep(1,m)/m), circular=TRUE)
		}		

	}else{
		
		stop("filter.numには整数を一つ指定してください")
	}
	

	colnames(smoothing.value) <- c("month","day", paste("X",1:data.num, sep=""))
	return(smoothing.value)

	
}
#===========================================================================
