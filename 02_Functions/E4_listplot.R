#------------------
# データフレーム -> リストへの変換
Zip <- function(...) Map(list, ...)

# リスト -> データフレムへの変換
Unzip <- function(...) rbind(data.frame(), ...)
#------------------


listplot <- function(plot_list, ...){
  tmp1  <- lapply(plot_list, FUN=length)
  nn1   <- sum(do.call(Unzip, tmp1)[,1])
  tmp1  <- lapply(plot_list, FUN=function(x){ lapply(x, FUN=nrow) })
  nn2   <- max(as.numeric(t(as.data.frame(tmp1))[,1]))
  
  
  xmat  <- matrix(NA, nrow=nn2, ncol=nn1)
  ymat  <- matrix(NA, nrow=nn2, ncol=nn1)
  
  iter <- 1
  for(i in 1:length(plot_list)){ # i <- 1
    PL2 <- plot_list[[i]]
    for(j in 1:length(PL2)){
      PL3 <- PL2[[j]]
      if(nrow(PL3)==nn2){
        xmat[,iter] <- PL3[,1]
        ymat[,iter] <- PL3[,2]
        iter <- iter + 1
      }else{
        tmpNA <- rep(NA, nn2-nrow(PL3))
        xmat[,iter] <- c(PL3[,1], tmpNA)
        ymat[,iter] <- c(PL3[,2], tmpNA)
        iter <- iter + 1
      }
      
    }
  }
  
  matplot(x=xmat, y=ymat, ...)
  
}