# cor()関数ではna.rmの引数が扱えなかったので自分で関数を組んだ

myCor <- function(Data){

    CovMatrix <- var(Data, na.rm=TRUE)
	stds <- as.numeric(sqrt(diag(CovMatrix)))
	N1   <- length(stds)
	StdMatrix <- matrix(NA, N1, N1)
	for(i in 1:N1){
		StdMatrix[i,] <- stds*stds[i]
	}
	CorMatrix <- CovMatrix/StdMatrix
	
	return(CorMatrix)	
}