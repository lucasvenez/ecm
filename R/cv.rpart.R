cv.rpart <- function(data, rtfit, cost = function(y,yhat) mean((y-yhat)^2), K = 10, seed = NA) {
	
	sample0 <- function(x, ...) x[sample.int(length(x), ...)]
	
	call <- match.call()
	
	if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
	
	seed <- if (is.na(seed)) get(".Random.seed", envir=.GlobalEnv, inherits = FALSE) else seed;
	
	n <- nrow(data)
	
	out <- NULL
	
	if ((K > n) || (K <= 1))
		stop("'K' outside allowable range")
	
	K.o <- K
	
	K <- round(K)
	
	kvals <- unique(round(n/(1L:floor(n/2))))
	
	temp <- abs(kvals-K)
	
	if (!any(temp == 0))
		K <- kvals[temp == min(temp)][1L]
	
	if (K!=K.o) 
		warning(gettextf("'K' has been set to %f", K), domain = NA)
	
	f <- ceiling(n/K)
	
	s <- sample0(rep(1L:K, f), n)
	
	n.s <- table(s)

	mrt.y <- rtfit$y
	
	cost.0 <- cost(mrt.y, predict(rtfit))
	
	ms <- max(s)
	
	CV <- 0
	
	Call <- rtfit$call
	
	for(i in seq_len(ms)) {
		
		j.out <- seq_len(n)[(s == i)]
		
		j.in <- seq_len(n)[(s != i)]
		
		Call$data <- data[j.in, , drop=FALSE]
		
		d.glm <- eval.parent(Call)
		
		p.alpha <- n.s[i]/n
		
		cost.i <- cost(mrt.y[j.out],
				predict(d.glm, data[j.out, , drop=FALSE],
						type = "vector"))
		
		CV <- CV + p.alpha * cost.i
		
		cost.0 <- cost.0 - p.alpha *
				cost(mrt.y, predict(d.glm, data, type = "vector"))
	}
	
	list(call = call, K = K,
			delta = as.numeric(c(CV, CV + cost.0)),  # drop any names
			seed = seed)
}