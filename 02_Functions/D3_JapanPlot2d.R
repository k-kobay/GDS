#==========================
# 日本地図描画関数を定義
# JapanPlot3d
# 日本地図を描画する関数
# 都道府県をローマ字で指定するとその県のみ描画する（複数指定可能）
# ローマ字の対応については以下を参照のこと
# HyōgoになってたのでHyogoに修正（2017/02/08）
PreNames <- 
c("Aichi","Ehime","Ibaraki","Okayama","Okinawa","Iwate","Gifu","Miyazaki", 
"Miyagi","Kyoto","Kumamoto","Gunma","Hiroshima", "Kagawa","Kochi","Saga",
"Saitama","Mie","Yamagata","Yamaguchi","Yamanashi","Shiga","Kagoshima","Akita",
"Niigata","Kanagawa","Aomori","Shizuoka","Ishikawa","Chiba","Osaka","Oita",
"Nagasaki","Nagano","Tottori","Shimane","Tokyo","Tokushima","Tochigi", "Nara",
"Toyama","Fukui","Fukuoka","Fukushima","Hyogo","Hokkaido","Wakayama")
#==========================

#==========================
# 地図座標データ読み込み
# map.All = 日本地図全データ
# load('~/Dropbox/03_さきがけ（酒井）/アメダスデータ解析/2016_12/week3/MapAll.Rdata')
# map.data = 都道府県ごとのデータ
# load('~/Dropbox/03_さきがけ（酒井）/アメダスデータ解析/2016_12/week3/MapData.Rdata')
#==========================

#==========================
# パッケージ読み込み
library(rgl)
# その他関数定義
# データフレーム -> リストへの変換
Zip <- function(...) Map(list, ...)

# リスト -> データフレムへの変換
Unzip <- function(...) rbind(data.frame(), ...)
#==========================

#==========================
# 観測所がある場所のみの要素番号
prefs <- c("Hokkaido","Niigata", "Hyogo", "Kagawa", "Shimane", "Kagoshima", "Yamaguchi", "Nagasaki", "Kumamoto", "Okinawa")
# 各県のリストの要素数
lapply(map.data, FUN=length)

# 各リストの中で一番点の多い要素を選ぶ
tmp1 <- lapply(map.data, FUN=function(x){
  tmp00 <- lapply(x, FUN=length)
  tmp01 <- do.call(Unzip, tmp00)[,1]
  which.max(tmp01)
})
tmp2 <- do.call(Unzip, tmp1)[,1]

obs_num_list <- list()
obs_num_list[[1]]  <- c(9,11);             names(obs_num_list)[1]  <- "Hokkaido"
obs_num_list[[2]]  <- c(1,3);             names(obs_num_list)[2]  <- "Niigata"
obs_num_list[[3]]  <- c(2,5,6);           names(obs_num_list)[3]  <- "Hyogo"
obs_num_list[[4]]  <- c(3,12);            names(obs_num_list)[4]  <- "Kagawa"
obs_num_list[[5]]  <- c(1,3);             names(obs_num_list)[5]  <- "Shimane"
obs_num_list[[6]]  <- c(7,19,20);         names(obs_num_list)[6]  <- "Kagoshima"
obs_num_list[[7]]  <- c(6,17);            names(obs_num_list)[7]  <- "Yamaguchi"
obs_num_list[[8]]  <- c(1,39);            names(obs_num_list)[8]  <- "Nagasaki"
obs_num_list[[9]]  <- c(9,10,14);         names(obs_num_list)[9]  <- "Kumamoto"
obs_num_list[[10]] <- c(1,7,11,19,30,31); names(obs_num_list)[10] <- "Okinawa"

num1 <- which(names(tmp1) %in% prefs)
# i <- 5
for(i in num1){
  num2 <- which(names(obs_num_list)==names(tmp1)[i])
  tmp1[[i]] <- obs_num_list[[num2]]
}

obs_only_num <- tmp1
#==========================

#==========================
# 関数定義
#  テスト用
#   preName=c("Saitama","Ibaraki", "Chiba", "Gunma", "Tochigi", "Fukushima","Niigata")
#   preName=c("Hyogo")
#   preName="all"
JapanPlot2d <- function(preName='all', jpn.xlim='auto', jpn.ylim='auto', lwd=1, axes=TRUE, main="",
                        lcol="black", x.axis.ratio=sin(pi*(90-35)/180), obs.only=FALSE, axis.value=TRUE,...){
    if(length(preName)==1){
    	if(preName=='all'){
    		if(length(jpn.xlim)==1 && jpn.xlim=='auto'){
    			jpn.xlim <- c(125, 150)
    		}
    		if(length(jpn.ylim)==1 && jpn.ylim=='auto'){
    			jpn.ylim <- c(25, 48)
    		}

    		
    		n <- length(map.All)
    		for(i in 1:n){
    			tmp1 <- map.All[[i]]
    			if(i==1){
    				plot(tmp1[,1], tmp1[,2],  xlim=jpn.xlim, ylim=jpn.ylim, 
    					   axes=axes, xlab='', ylab='',  type="l", lwd=lwd, main=main, col=lcol,...)
    			}else{
    				lines(tmp1[,1], tmp1[,2], lwd=lwd, col=lcol)
    			}
    		}
    		
    	}else{
    			num1    <- which(names(map.data)==preName)
    			tmpData <- map.data[[num1]]
    			if(obs.only){
    			  tmp_num1  <- which(names(obs_only_num)==preName)
    			  n2        <- obs_only_num[[tmp_num1]]
    			}else{
    			  n2        <- 1:length(tmpData)  
    			}
    			
    			
	            mymin <- function(x){ apply(x, 2, min) }
	            mymax <- function(x){ apply(x, 2, max) }            
	            
	            if(obs.only){
	              axisMin <- apply(t(as.data.frame(lapply(tmpData[n2], FUN=mymin))), 2, min)
	              axisMax <- apply(t(as.data.frame(lapply(tmpData[n2], FUN=mymax))), 2, max)
	            }else{
	              axisMin <- apply(t(as.data.frame(lapply(map.data[num1], function(X){lapply(X, mymin)}))), 2, min)
	              axisMax <- apply(t(as.data.frame(lapply(map.data[num1], function(X){lapply(X, mymax)}))), 2, max)
	            }
	            
	            xlim.center <- (axisMin[1]+axisMax[1])/2
	            xlim.length <- axisMax[1] - axisMin[1]
	            
	            ylim.center <- (axisMin[2]+axisMax[2])/2
	            ylim.length <- axisMax[2] - axisMin[2]


	            if(length(jpn.xlim)==1 && jpn.xlim=='auto'){
	              jpn.xlim    <- c(axisMin[1], axisMax[1])
	            }
	            
	            if(length(jpn.ylim)==1 && jpn.ylim=='auto'){
	              jpn.ylim    <- c(axisMin[2], axisMax[2])
	              
	              # アスペクト比の補正
	              if(xlim.length <= ylim.length){
	                jpn.xlim <- c(xlim.center - (ylim.length/2/x.axis.ratio), xlim.center + (ylim.length)/2/x.axis.ratio )
	              }else{
	                jpn.ylim <- c(ylim.center - (xlim.length/2), ylim.center + (xlim.length)/2 )
	                new_y.center <- mean(jpn.ylim)
	                new_y.length <- jpn.ylim[2] - jpn.ylim[1]
	                jpn.xlim <- c(xlim.center - (new_y.length/2/x.axis.ratio), xlim.center + (new_y.length)/2/x.axis.ratio )
	              }
	            }
	            

    			# i <- 6
	        iter <- 1
    			for(i in n2){
    				tmp1 <- tmpData[[i]]
    				if(iter==1){
    				  plot(tmp1[,1], tmp1[,2], xlim=jpn.xlim, ylim=jpn.ylim,
    					   axes=axes, xlab='', ylab='', type="l", lwd=lwd, main=main, col=lcol,...)
    				     iter <- iter + 1
    				}else{
    					   lines(tmp1[,1], tmp1[,2], lwd=lwd, col=lcol)
    				}
    			}

    		}
    	}else{
    		  n3 <- length(preName)

	        mymin <- function(x){ apply(x, 2, min) }
	        mymax <- function(x){ apply(x, 2, max) }            
    		
    		  num2 <- which(names(map.data) %in% preName)
    		
    		  if(obs.only){
    		    min_list <- list()
    		    max_list <- list()
    		    for(kk in 1:n3){
    		      num00 <- which(names(map.data)==preName[kk])
    		      num01 <- which(names(obs_only_num)==preName[kk])
    		      num02 <- obs_only_num[[num01]]
    		      
    		      tmp_map <- map.data[[num00]]
    		      min_list[[kk]] <- apply(t(as.data.frame(lapply(tmp_map[num02], FUN=mymin))), 2, min)
    		      max_list[[kk]] <- apply(t(as.data.frame(lapply(tmp_map[num02], FUN=mymax))), 2, max)
    		    }
    		    
    		    axisMin <- apply(t(as.data.frame(min_list)), 2, min)
    		    axisMax <- apply(t(as.data.frame(max_list)), 2, max)
    		    
    		    
    		  }else{
    		    axisMin <- apply(t(as.data.frame(lapply(map.data[num2], function(X){lapply(X, mymin)}))),2,min)
    		    axisMax <- apply(t(as.data.frame(lapply(map.data[num2], function(X){lapply(X, mymax)}))),2,max)
    		  }
    		  
    		  xlim.center <- (axisMin[1]+axisMax[1])/2
    		  xlim.length <- axisMax[1] - axisMin[1]
    		  
    		  ylim.center <- (axisMin[2]+axisMax[2])/2
    		  ylim.length <- axisMax[2] - axisMin[2]
    		
    		iter <- 1
    		for(i in 1:n3){ # i <- 1
    			num1    <- which(names(map.data)==preName[i])
    			tmpData <- map.data[[num1]]
    			
    			if(obs.only){
    			  tmp_num1 <- which(names(obs_only_num)==preName[i])
    			  n4       <- obs_only_num[[tmp_num1]]
    			}else{
    			  n4      <- 1:length(tmpData)
    			}
    			

    			if(length(jpn.xlim)==1 && jpn.xlim=='auto'){
    			  jpn.xlim    <- c(axisMin[1], axisMax[1])
    			}
    			
    			if(length(jpn.ylim)==1 && jpn.ylim=='auto'){
    			  jpn.ylim    <- c(axisMin[2], axisMax[2])
    			  
    			  # アスペクト比の補正
    			  if(xlim.length <= ylim.length){
    			    jpn.xlim <- c(xlim.center - (ylim.length/2/x.axis.ratio), xlim.center + (ylim.length)/2/x.axis.ratio )
    			  }else{
    			    jpn.ylim <- c(ylim.center - (xlim.length/2), ylim.center + (xlim.length)/2 )
    			    new_y.center <- mean(jpn.ylim)
    			    new_y.length <- jpn.ylim[2] - jpn.ylim[1]
    			    jpn.xlim <- c(xlim.center - (new_y.length/2/x.axis.ratio), xlim.center + (new_y.length)/2/x.axis.ratio )
    			  }
    			}
    			
    			
    			
    			
    			for(j in n4){
    				tmp1 <- tmpData[[j]]
    				if(iter==1){
    				    plot(tmp1[,1], tmp1[,2], xlim=jpn.xlim, ylim=jpn.ylim, 
    					       axes=axes, xlab='', ylab='', type='l', lwd=lwd, main=main, col=lcol,...)
    				    iter <- iter + 1
    				}else{
    					lines(tmp1[,1], tmp1[,2], lwd=lwd, col=lcol)
    				}
    			}    			
    		}
    	}
  
  if(axis.value==TRUE){
    return(data.frame(xlim=jpn.xlim, ylim=jpn.ylim))
  }
  
}
#==========================

#==========================
# Test Code
# jpn.xlim <- c(125, 150)
# jpn.ylim <- c(25, 48)
#
# preName <- c('Hokkaido', 'Ehime', 'Tokyo', 'Niigata')
#
# JapanPlot2d(preName='all', jpn.xlim=jpn.xlim, jpn.ylim=jpn.ylim)
#==========================
