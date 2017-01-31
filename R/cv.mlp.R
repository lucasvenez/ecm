cv.mlp <- function(x, y, mlpfit, cost = function(y,yhat) mean((y-yhat)^2), K = 10, seed = NA, maxVal = NA, minVal = NA) {

  if (is.na(maxVal) || is.na(minVal))
    stop("invalid values for maxVal and minVal parameters")
  
	sample0 <- function(x, ...) x[sample.int(length(x), ...)]
	
	denormalizeData <- function(normVal, maxVal, minVal) normVal * (maxVal - minVal) + minVal
	
	call <- match.call()
	
	if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
	
	seed <- if (is.na(seed)) get(".Random.seed", envir=.GlobalEnv, inherits = FALSE) else seed;
	
	if (!is.data.frame(x)) x <- as.data.frame(x)
	
	if (!is.data.frame(y)) y <- as.data.frame(y)
	
	n <- nrow(x)
	
	out <- NULL
	
	if ((K > n) || (K <= 1))
		stop("'K' outside allowable range")
	
	K.o <- K
	
	K <- round(K)
	
	kvals <- unique(round(n/(1L:floor(n/2))))
	
	temp <- abs(kvals - K)
	
	if (!any(temp == 0))
		K <- kvals[temp == min(temp)][1L]
	
	if (K != K.o) 
		warning(gettextf("'K' has been set to %f", K), domain = NA)
	
	f <- ceiling(n/K)
	
	s <- sample0(rep(1L:K, f), n)
	
	n.s <- table(s)
	
	mlp.y <- y
	
	cost.0 <- cost(
	    denormalizeData(unlist(mlp.y), maxVal, minVal), 
	    denormalizeData(unlist(fitted(mlpfit)), maxVal, minVal))
	
	ms <- max(s)
	
	CV <- 0
	
	for(i in seq_len(ms)) {
		
		j.out <- seq_len(n)[(s == i)]
		
		j.in <- seq_len(n)[(s != i)]
		
		d.mlp <- mlp(
				x                     = x[j.in, , drop = FALSE], 
				y                     = y[j.in, , drop = FALSE], 
				initFunc              = mlpfit$initFunc,
				initFuncParams        = mlpfit$initFuncParams,
				learnFunc             = mlpfit$learnFunc,
				learnFuncParams       = mlpfit$learnFuncParams,
				maxit                 = mlpfit$maxit,
				shufflePatterns       = mlpfit$shufflePatterns,
				updateFunc            = mlpfit$updateFunc,
				updateFuncParams      = mlpfit$updateFuncParams,
				computeIterativeError = mlpfit$computeIterativeError) 
		
		p.alpha <- n.s[i]/n
		
		cost.i <- cost(
		      denormalizeData(unlist(mlp.y[j.out,]), maxVal, minVal),
				  denormalizeData(unlist(predict(d.mlp, x[j.out, , drop = FALSE])), maxVal, minVal))
		
		CV <- CV + p.alpha * cost.i
		
		cost.0 <- cost.0 - p.alpha *
				cost(
				  denormalizeData(unlist(mlp.y), maxVal, minVal), 
				  denormalizeData(unlist(predict(d.mlp, x)), maxVal, minVal))
	}
	
	list(call = call, K = K,
		delta = as.numeric(c(CV, CV + cost.0)),  # drop any names
		seed = seed)
}