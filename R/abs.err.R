abs.err <- function(yhat, y) {
	f <- function(yhat, y) {
		if ( y > 0 )
			return(abs((y - yhat) / y) * 100)
		else
			return(abs((y - yhat)) * 100)
	}
		
	return(mapply(FUN = f, yhat = yhat, y = y))
}