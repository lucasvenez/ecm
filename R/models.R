###########################################################
# Power Consumption Prediction Model via Generalized Linear 
# Regression
###########################################################

###########################
#
# Required libraries and sources
#
lapply(list("rpart", "RSNNS", "ggplot2", "scales"), FUN = require, character.only = TRUE)

lapply(list("R/formulas.R"), FUN = source)

###########################
#
# Loading measuring data
#
intel <- read.table("data/intel.csv", sep = ",", dec = ".", header = T)

amd   <- read.table("data/amd.csv",   sep = ",", dec = ".", header = T)

both  <- NULL

if (!file.exists("data/both.csv")) {

	###
	# Mergin data
	#
	both <- rbind(
	   cbind(data.frame(ARCH =  1), intel), #intel 
	   cbind(data.frame(ARCH = -1), amd))   #amd
	
	write.table(both, "data/both.csv", sep=",", dec=".", row.names = F, col.names = T)
	
} else {
  
	both <- read.table("data/both.csv", sep = ",", dec = ".", header = T)
	
}

################################################
#
# Multivariate Linear Regression
#
intel.mlr.time <- system.time(intel.mlr <- glm(intel.f, data = intel))
save(intel.mlr, file = "data/intel.mlr.rda")

amd.mlr.time <- system.time(amd.mlr <- glm(amd.f,   data = amd))
save(amd.mlr, file = "data/amd.mlr.rda")

both.mlr.time <- system.time(both.mlr  <- glm(both.f,  data = both))
save(both.mlr, file = "data/both.mlr.rda")

###############################################
#
# Regression Tree (rpart)
#
intel.rt.time <- system.time(intel.rt <- rpart(intel.f, data = intel))
save(intel.rt, file = "data/intel.rt.rda")

amd.rt.time <- system.time(amd.rt <- rpart(amd.f, data = amd))
save(amd.rt, file = "data/amd.rt.rda")

both.rt.time <- system.time(both.rt <- rpart(both.f,  data = both, method = "anova"))
save(both.rt, file = "data/both.rt.rda")

###############################################
#
# Normalizing Data
#
intel.inputs <-
	normalizeData(
		intel[strsplit((as.character(intel.f)[-1])[2], " + ", fixed = TRUE)[[1]]],
		type = "0_1")
intel.output <- normalizeData(
		intel[, (as.character(intel.f)[-1])[1]],
		type = "0_1")

amd.inputs <- normalizeData( 
		amd[strsplit((as.character(amd.f)[-1])[2], " + ", fixed = TRUE)[[1]]],
		type = "0_1")
amd.output <- normalizeData(
		amd[, (as.character(amd.f)[-1])[1]],
		type = "0_1")

both.inputs <- normalizeData(
		both[strsplit((as.character(both.f)[-1])[2], " + ", fixed = TRUE)[[1]]],
		type = "0_1")
both.output <- normalizeData(
		both[, (as.character(both.f)[-1])[1]],
		type = "0_1")

###############################################
#
# Multilayer Perceptron
#
intel.mlp.time <- system.time(intel.mlp  <- mlp(
	x               = intel.inputs, 
	y               = intel.output,  
	size            = c(44, 44, 44),
	learnFunc       = "BackpropChunk",     #Std_Backpropagation, BackpropBatch, TimeDelayBackprop, BackpropMomentum
	learnFuncParams = c(5, 0, 50, -5, 5),
	hiddenActFunc   = "Act_TanH",          #Act_Logistic, Act_TanH, Act_Signum 
	updateFunc      = "Topological_Order", #Synchronous_Order, Serial_Order, Random_Permutation, Random_Order
	maxit           = 1000, 
	linOut          = FALSE,
	inputsTest      = intel.inputs, 
	targetsTest     = intel.output
))
save(intel.mlp, file = "data/intel.mlp.rda")

amd.mlp.time <- system.time(amd.mlp  <- mlp(
		x               = amd.inputs, 
		y               = amd.output,  
		size            = c(36, 36, 36),
		learnFunc       = "BackpropChunk",     #Std_Backpropagation, BackpropBatch, TimeDelayBackprop, BackpropMomentum
		learnFuncParams = c(5, 0, 50, -5, 5),
		hiddenActFunc   = "Act_TanH",          #Act_Logistic, Act_TanH, Act_Signum 
		updateFunc      = "Topological_Order", #Synchronous_Order, Serial_Order, Random_Permutation, Random_Order
		maxit           = 1000, 
		linOut          = FALSE,
		inputsTest      = amd.inputs, 
		targetsTest     = amd.output
))
save(amd.mlp, file = "data/amd.mlp.rda")

both.mlp.time <- system.time(both.mlp  <- mlp(
		x               = both.inputs, 
		y               = both.output,  
		size            = c(54, 54, 54),
		learnFunc       = "BackpropChunk",     #Std_Backpropagation, BackpropBatch, TimeDelayBackprop, BackpropMomentum
		learnFuncParams = c(5, 0, 50, -5, 5),
		hiddenActFunc   = "Act_TanH",          #Act_Logistic, Act_TanH, Act_Signum 
		updateFunc      = "Topological_Order", #Synchronous_Order, Serial_Order, Random_Permutation, Random_Order
		maxit           = 1000, 
		linOut          = FALSE,
		inputsTest      = both.inputs, 
		targetsTest     = both.output
))
save(both.mlp, file = "data/both.mlp.rda")

all.time <- rbind(
	data.frame(arch = "intel", method = "mlr", time.user = intel.mlr.time[1], time.system = intel.mlr.time[2]),
	data.frame(arch = "amd",   method = "mlr", time.user = amd.mlr.time[1],   time.system = amd.mlr.time[2]),
	data.frame(arch = "both",  method = "mlr", time.user = both.mlr.time[1],  time.system = both.mlr.time[2]),
	data.frame(arch = "intel", method = "rt",  time.user = intel.rt.time[1],  time.system = intel.rt.time[2]),
	data.frame(arch = "amd",   method = "rt",  time.user = amd.rt.time[1],    time.system = amd.rt.time[2]),
	data.frame(arch = "both",  method = "rt",  time.user = both.rt.time[1],   time.system = both.rt.time[2]),
	data.frame(arch = "intel", method = "mlp", time.user = intel.mlp.time[1], time.system = intel.mlp.time[2]),
	data.frame(arch = "amd",   method = "mlp", time.user = amd.mlp.time[1],   time.system = amd.mlp.time[2]),
	data.frame(arch = "both",  method = "mlp", time.user = both.mlp.time[1],  time.system = both.mlp.time[2]))

write.table(all.time, file = "data/models.convergence.time.csv", sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
