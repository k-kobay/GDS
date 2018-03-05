#============================================================================================================
# datefunc の定義
# 日付を二つ指定してその間をプロットするための添え字番号を返す関数を定義する
# 引数
# t1   : 日付を"month-day"の形で指定。
# t2   : 同上
# leap :
# 注意：一年の中で未来の日付を指定すると、時系列を遡った形で数列が返される。


datefunc <- function(t1,t2, leap=TRUE){
	tmp.month <- c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31),rep(6,31),rep(7,31),rep(8,31),rep(9,31),rep(10,31),rep(11,31),rep(12,31))
	tmp.date <- c(1:31)
	tmp.date <- rep(tmp.date,12)
	tmp.date <- cbind(tmp.month,tmp.date)
	
	if(leap==TRUE){tmp.date <- tmp.date[-c(61,62,124,186,279,341),];n=366
	}else{tmp.date <- tmp.date[-c(60,61,62,124,186,279,341),];n=365}
	date.num <- rep(0,n)
	for(i in 1:n){
		date.num[i] <- paste(tmp.date[i,1],tmp.date[i,2],sep="-")
	}
	num <- c(which(date.num==t1):which(date.num==t2))
	return(num)
}
#===============================================end==========================================================
