#' @title Accuracy Analysis
#' 
#' @author Lucas Venezian Povoa
#'
#' @lastupdate January 14th, 2016
#'
#' @description Accuract analysis for the models generated using 
#' methods Multiple Linear Regression, Regression Tree, and
#' Multilayer Perceptron. The accuracy analysis was realized with 
#' cross-validation using average, variance, and standard 
#' deviation of the following metrics: Squared Error, Absolute Error, 
#' Percent Error, Absolute Percent Error, and Absolute Scaled Error.
#'

#
# Loading required packages, sources, and data.
#
lapply(list("boot", "rpart", "RSNNS"), FUN = require, character.only = TRUE)

lapply(list("R/r.squared.R", "R/accuracy.metrics.R", "R/cv.glm.R", "R/cv.rpart.R", "R/cv.mlp.R", "R/formulas.R"), FUN = source)

load("data/intel.mlr.rda")
load("data/intel.rt.rda")
load("data/intel.mlp.rda")

load("data/amd.mlr.rda")
load("data/amd.rt.rda")
load("data/amd.mlp.rda")

load("data/both.mlr.rda")
load("data/both.rt.rda")
load("data/both.mlp.rda")

intel <- read.table("data/intel.csv", sep = ",", dec = ".", header = T)
amd   <- read.table("data/amd.csv",   sep = ",", dec = ".", header = T)
both  <- read.table("data/both.csv",  sep = ",", dec = ".", header = T) 

#
# Fixing seed to make the analysis reproducible.
#
SEED <- 237

#
# Defining accuracy metrics function
#
cost <- function(y, yhat)
	c(
	  r.squared(y, yhat), 
	  se.mean(y, yhat), 
	  se.var(y, yhat),
	  se.sd(y, yhat),
	  ae.mean(y, yhat), 
	  ae.var(y, yhat),
	  ae.sd(y, yhat),
	  pe.mean(y, yhat), 
	  pe.var(y, yhat),
	  pe.sd(y, yhat),
	  ape.mean(y, yhat), 
	  ape.var(y, yhat), 
	  ape.sd(y, yhat), 
	  ase.mean(y, yhat),
	  ase.var(y, yhat),
	  ase.sd(y, yhat)
	)

#
# Cross-validation 10-fol for Multiple Linear Regression models
#
intel.mlr.cv <- cv.glm(intel, intel.mlr, cost, K = 10, seed = SEED)$delta
amd.mlr.cv   <- cv.glm(amd,   amd.mlr,   cost, K = 10, seed = SEED)$delta
both.mlr.cv  <- cv.glm(both,  both.mlr,  cost, K = 10, seed = SEED)$delta

#
# Cross-validation 10-fold for Regression Tree models
#
intel.rt.cv <- cv.rpart(intel, intel.rt, cost, K = 10, seed = SEED)$delta
amd.rt.cv   <- cv.rpart(amd,   amd.rt,   cost, K = 10, seed = SEED)$delta
both.rt.cv  <- cv.rpart(both,  both.rt,  cost, K = 10, seed = SEED)$delta

#
# Normalizing data for analyzing Multilayer Perceptron models
#
intel.inputs <- normalizeData(
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

#
# Cross-validation 10-fold for Multilayer Perceptron
#
intel.mlp.cv <- cv.mlp(intel.inputs, intel.output, intel.mlp, cost, K = 10, seed = SEED, 
                       maxVal = max(intel[, (as.character(intel.f)[-1])[1]]), minVal = min(intel[, (as.character(intel.f)[-1])[1]]))$delta

amd.mlp.cv   <- cv.mlp(amd.inputs,   amd.output,   amd.mlp,   cost, K = 10, seed = SEED, 
                       maxVal = max(amd[, (as.character(amd.f)[-1])[1]]), minVal = min(amd[, (as.character(amd.f)[-1])[1]]))$delta

both.mlp.cv  <- cv.mlp(both.inputs,  both.output,  both.mlp,  cost, K = 10, seed = SEED, 
                       maxVal = max(both[, (as.character(both.f)[-1])[1]]), minVal = min(both[, (as.character(both.f)[-1])[1]]))$delta

#
# Exporting data
#
intel.accuracy <- data.frame(
	arch        = "Intel",
	methods     = c("MLR", "RT", "MLP"),
	r2       = c(intel.mlr.cv[01], intel.rt.cv[01], intel.mlp.cv[01]),
	se.mean  = c(intel.mlr.cv[02], intel.rt.cv[02], intel.mlp.cv[02]),
	se.var   = c(intel.mlr.cv[03], intel.rt.cv[03], intel.mlp.cv[03]),
	se.sd    = c(intel.mlr.cv[04], intel.rt.cv[04], intel.mlp.cv[04]),
	ae.mean  = c(intel.mlr.cv[05], intel.rt.cv[05], intel.mlp.cv[05]),
	ae.var   = c(intel.mlr.cv[06], intel.rt.cv[06], intel.mlp.cv[06]),
	ae.sd    = c(intel.mlr.cv[07], intel.rt.cv[07], intel.mlp.cv[07]),
	pe.mean  = c(intel.mlr.cv[08], intel.rt.cv[08], intel.mlp.cv[08]),
	pe.var   = c(intel.mlr.cv[09], intel.rt.cv[09], intel.mlp.cv[09]),
	pe.sd    = c(intel.mlr.cv[10], intel.rt.cv[10], intel.mlp.cv[10]),
	ape.mean = c(intel.mlr.cv[11], intel.rt.cv[11], intel.mlp.cv[11]),
	ape.var  = c(intel.mlr.cv[12], intel.rt.cv[12], intel.mlp.cv[12]),
	ape.sd   = c(intel.mlr.cv[13], intel.rt.cv[13], intel.mlp.cv[13]),
	ase.mean = c(intel.mlr.cv[14], intel.rt.cv[14], intel.mlp.cv[14]),
	ase.var  = c(intel.mlr.cv[15], intel.rt.cv[15], intel.mlp.cv[15]),
	ase.sd   = c(intel.mlr.cv[16], intel.rt.cv[16], intel.mlp.cv[16])
)
	
amd.accuracy <- data.frame(
	arch        = "AMD",
	methods     = c("MLR", "RT", "MLP"),
	r2       = c(amd.mlr.cv[01], amd.rt.cv[01], amd.mlp.cv[01]),
	se.mean  = c(amd.mlr.cv[02], amd.rt.cv[02], amd.mlp.cv[02]),
	se.var   = c(amd.mlr.cv[03], amd.rt.cv[03], amd.mlp.cv[03]),
	se.sd    = c(amd.mlr.cv[04], amd.rt.cv[04], amd.mlp.cv[04]),
	ae.mean  = c(amd.mlr.cv[05], amd.rt.cv[05], amd.mlp.cv[05]),
	ae.var   = c(amd.mlr.cv[06], amd.rt.cv[06], amd.mlp.cv[06]),
	ae.sd    = c(amd.mlr.cv[07], amd.rt.cv[07], amd.mlp.cv[07]),
	pe.mean  = c(amd.mlr.cv[08], amd.rt.cv[08], amd.mlp.cv[08]),
	pe.var   = c(amd.mlr.cv[09], amd.rt.cv[09], amd.mlp.cv[09]),
	pe.sd    = c(amd.mlr.cv[10], amd.rt.cv[10], amd.mlp.cv[10]),
	ape.mean = c(amd.mlr.cv[11], amd.rt.cv[11], amd.mlp.cv[11]),
	ape.var  = c(amd.mlr.cv[12], amd.rt.cv[12], amd.mlp.cv[12]),
	ape.sd   = c(amd.mlr.cv[13], amd.rt.cv[13], amd.mlp.cv[13]),
	ase.mean = c(amd.mlr.cv[14], amd.rt.cv[14], amd.mlp.cv[14]),
	ase.var  = c(amd.mlr.cv[15], amd.rt.cv[15], amd.mlp.cv[15]),
	ase.sd   = c(amd.mlr.cv[16], amd.rt.cv[16], amd.mlp.cv[16])
)

both.accuracy <- data.frame(
	arch        = "Both",
	methods     = c("MLR", "RT", "MLP"),
	r2       = c(both.mlr.cv[01], both.rt.cv[01], both.mlp.cv[01]),
	se.mean  = c(both.mlr.cv[02], both.rt.cv[02], both.mlp.cv[02]),
	se.var   = c(both.mlr.cv[03], both.rt.cv[03], both.mlp.cv[03]),
	se.sd    = c(both.mlr.cv[04], both.rt.cv[04], both.mlp.cv[04]),
	ae.mean  = c(both.mlr.cv[05], both.rt.cv[05], both.mlp.cv[05]),
	ae.var   = c(both.mlr.cv[06], both.rt.cv[06], both.mlp.cv[06]),
	ae.sd    = c(both.mlr.cv[07], both.rt.cv[07], both.mlp.cv[07]),
	pe.mean  = c(both.mlr.cv[08], both.rt.cv[08], both.mlp.cv[08]),
	pe.var   = c(both.mlr.cv[09], both.rt.cv[09], both.mlp.cv[09]),
	pe.sd    = c(both.mlr.cv[10], both.rt.cv[10], both.mlp.cv[10]),
	ape.mean = c(both.mlr.cv[11], both.rt.cv[11], both.mlp.cv[11]),
	ape.var  = c(both.mlr.cv[12], both.rt.cv[12], both.mlp.cv[12]),
	ape.sd   = c(both.mlr.cv[13], both.rt.cv[13], both.mlp.cv[13]),
	ase.mean = c(both.mlr.cv[14], both.rt.cv[14], both.mlp.cv[14]),
	ase.var  = c(both.mlr.cv[15], both.rt.cv[15], both.mlp.cv[15]),
	ase.sd   = c(both.mlr.cv[16], both.rt.cv[16], both.mlp.cv[16])
)

all.accuracy <- rbind(intel.accuracy, amd.accuracy, both.accuracy)

write.table(
	all.accuracy, 
	file = "data/models.accuracy.csv", 
	sep = ",", dec = ".", 
	row.names = FALSE, col.names = TRUE)