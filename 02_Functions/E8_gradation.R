CorColor <- function(x, gradation){
  
  n <- length(gradation)
  
  # gradation value : 0 ~ 0.7
  # < 0 : blue
  # 0.7 < : red
  # if x include value that is x < 0 or 0.7 < x, print warning( "there exist x value out of range 0~0.7" )
  
  GradCols   <- colorRamp(gradation)
  
  OverNum   <- which(x > 0.7)
  UnderNum  <- which(x < 0)
  InNum     <- which({0<=x}&{x<=0.7})
  
  colVal <- rep(NA,length(x))
  
  colVal[InNum]    <- rgb( GradCols((x[InNum])/0.7)/255 )
  colVal[OverNum]  <- "#FF0000"
  colVal[UnderNum] <- "#0000FF"
  
  return(colVal)
}	


### sample code ###
# gradation <- c(c("blue", "cyan", "green", "yellow", "red"))	
# plot(NULL, xlim=c(-100,100), ylim=c(0,1), axes=FALSE, xlab="", ylab="", main="Color")
# rect(-100:99, 0, -99:100, 1, col=CorColor(seq(-1,1,length.out=200), gradation), border=NA)
# axis(1, at=seq(-100,100,length.out=length(seq(-1,1,0.1))), labels=seq(-1,1,0.1), las=1, cex=0.7)
