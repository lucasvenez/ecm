###########################
#
# Dispersion analysis
#
lapply(list("ggplot2", "scales"), require, FUN = require, character.only = TRUE)

###########################
#
# Setting workspace
#
setwd("/home/lucas/Dropbox/Papers/Writing/FGCS/analysis/")

###########################
#
# Loading measuring data
#
intel <- read.table("data/intel.csv",
	sep = ",", dec = ".", header = T)[-1, ]

amd   <- read.table("data/amd.csv",   
	sep = ",", dec = ".", header = T)[-1, ]

both  <- read.table("data/both.csv", 
	sep = ",", dec = ".", header = T)[-c(1,2), ]

###########################
#
# Mean
#
intel.mean <- apply(intel, 2, mean)
amd.mean   <- apply(amd,   2, mean)
both.mean  <- apply(both,  2, mean)

###########################
#
# Standard Deviation
#
intel.sd <- apply(intel, 2, sd)
amd.sd   <- apply(amd,   2, sd)
both.sd  <- apply(both,  2, sd)

###########################
#
# Variance
#
intel.var <- apply(intel, 2, var)
amd.var   <- apply(amd,   2, var)
both.var  <- apply(both,  2, var)

###########################
#
# Variance Coeficient
#

intel.varc <- intel.sd / intel.mean
amd.varc   <- amd.sd   / amd.mean
both.varc  <- both.sd  / both.mean

###########################
#
# Maximum
#
intel.max <- apply(intel, 2, max)
amd.max   <- apply(amd,   2, max)
both.max  <- apply(both,  2, max)

###########################
#
# Minimum
#
intel.min <- apply(intel, 2, min)
amd.min   <- apply(amd,   2, min)
both.min  <- apply(both,  2, min)

###########################
#
# Amplitude
#
intel.amp <- intel.max - intel.min
amd.amp   <- amd.max - amd.min
both.amp  <- both.max - both.min

###########################
#
# Quantiles
#
intel.quant <- apply(intel, 2, quantile, probs = seq(.25, 1, by = .25))
amd.quant   <- apply(amd,   2, quantile, probs = seq(.25, 1, by = .25))
both.quant  <- apply(both,  2, quantile, probs = seq(.25, 1, by = .25))

###########################
#
# Merging all
#
cn <- c(
	"Variable",
	"Min.", 
	"1st Qu.", 
	"Median", 
	"Mean", 
	"3rd Qu.", 
	"Max", 
	"Standard Deviation", 
	"Variance",
	"Coefficient of Variance",
	"Amplitude")

intel.all.stat <- data.frame(
	variable       = colnames(intel.quant),
	min            = as.vector(intel.min),
	Q1             = as.vector(t(intel.quant)[,1]),
	median         = as.vector(t(intel.quant)[,2]),
	mean           = as.vector(intel.mean),
	Q3             = as.vector(t(intel.quant)[,3]),
	max            = as.vector(intel.max),
	sd             = as.vector(intel.sd),
	var            = as.vector(intel.var),
	varc           = as.vector(intel.varc),
	amplitude      = as.vector(intel.amp))

amd.all.stat <- data.frame(
	variable  = colnames(amd.quant),
	min       = as.vector(amd.min),
	Q1        = as.vector(t(amd.quant)[,1]),
	median    = as.vector(t(amd.quant)[,2]),
	mean      = as.vector(amd.mean),
	Q3        = as.vector(t(amd.quant)[,3]),
	max       = as.vector(amd.max),
	sd        = as.vector(amd.sd),
	var       = as.vector(amd.var),
	varc      = as.vector(amd.varc),
	amplitude = as.vector(amd.amp))

both.all.stat <- data.frame(
	variable  = colnames(both.quant),
	min       = as.vector(both.min),
	Q1        = as.vector(t(both.quant)[,1]),
	median    = as.vector(t(both.quant)[,2]),
	mean      = as.vector(both.mean),
	Q3        = as.vector(t(both.quant)[,3]),
	max       = as.vector(both.max),
	sd        = as.vector(both.sd),
	var       = as.vector(both.var),
	varc      = as.vector(both.varc),
	amplitude = as.vector(both.amp))

###########################
#
# Exporting data
#
write.table(intel.all.stat, "data/intel_stat.csv", 
		sep = ",", dec = ".", row.names = FALSE,
		col.names = cn)

write.table(amd.all.stat, "data/amd_stat.csv",   
		sep = ",", dec = ".", row.names = FALSE, 
		col.names = cn)

write.table(both.all.stat, "data/both_stat.csv",  
		sep = ",", dec = ".", row.names = FALSE, 
		col.names = cn)

###########################
#
# Plotting data 
# (independent variables)
#
#
#for (i in 1:ncol(intel)) {
#	
#	intel.data <- 
#		data.frame(
#			variable = colnames(intel)[i], 
#			data = intel[,i])
#
#	intel.stat.plot <- ggplot(intel.data, aes(x= 1, y = data)) + geom_boxplot(fill = I("gray50")) +
#		facet_grid(. ~ variable) + theme(legend.position = "none") +
#		xlab("") + ylab("Data") +
#		theme(
#			strip.text.x = element_text(size = 8),
#			axis.text.x  = element_text(size = 8),
#			axis.text.x  = element_blank(),
#			axis.ticks.x = element_blank(),
#			axis.text.y  = element_text(size = 8),
#			axis.title.y = element_text(size = 8))
#
#	ggsave(
#		paste("../images/", if (i < 10) paste( "0", i, sep = "") else i, "_INTEL_BOXPLOT.pdf", sep = ""), 
#		intel.stat.plot, width = 6, height = 4.5, units = "cm")
#}
#
#for (i in 1:ncol(amd)) {
#	
#	amd.data <- 
#		data.frame(
#			variable = colnames(amd)[i], 
#			data = amd[,i])
#	
#	amd.stat.plot <- ggplot(amd.data, aes(x= 1, y = data)) + geom_boxplot(fill = I("gray50")) +
#		facet_grid(. ~ variable) + theme(legend.position = "none") +
#		xlab("") + ylab("Data") +
#		theme(
#			strip.text.x = element_text(size = 8),
#			axis.text.x  = element_blank(),
#			axis.ticks.x = element_blank(),
#			axis.text.y  = element_text(size = 8),
#			axis.title.y = element_text(size = 8),
#			axis.title.y = element_text(size = 8))
#	
#	ggsave(
#		paste("../images/", if (i < 10) paste( "0", i, sep = "") else i, "_AMD_BOXPLOT.pdf", sep = ""), 
#		amd.stat.plot, width = 6, height = 4.5, units = "cm")
#}
#
#for (i in 1:ncol(both)) {
#	
#	both.data <- 
#			data.frame(
#					variable = colnames(both)[i], 
#					data = both[,i])
#	
#	both.stat.plot <- ggplot(both.data, aes(x= 1, y = data)) + geom_boxplot(fill = I("gray50")) +
#		facet_grid(. ~ variable) + theme(legend.position = "none") +
#		xlab("") + ylab("Data") +
#		theme(
#			strip.text.x = element_text(size = 8),
#			axis.text.x  = element_blank(),
#			axis.ticks.x = element_blank(),
#			axis.text.y  = element_text(size = 8),
#			axis.title.y = element_text(size = 8))
#	
#	ggsave(
#		paste("../images/", if (i < 10) paste( "0", i, sep = "") else i, "_BOTH_BOXPLOT.pdf", sep = ""), 
#		both.stat.plot, width = 6, height = 4.5, units = "cm")
#}
#
###########################
#
# Plotting data 
# (dependent variable)
#

power.mean.sd <- data.frame(
	mean = intel.mean[length(intel.mean)],
	sd   = intel.sd[length(intel.sd)],
	arch = "Intel")

power.mean.sd <- rbind(power.mean.sd, data.frame(
	mean = amd.mean[length(amd.mean)],
	sd   = amd.sd[length(amd.sd)],
	arch = "AMD"))

power.mean.sd <- rbind(power.mean.sd, data.frame(
	mean = both.mean[length(both.mean)],
	sd   = both.sd[length(both.sd)],
	arch = "Both"))

graph <- ggplot(power.mean.sd, aes(x = 1, y = mean, fill = arch)) + 
	geom_bar(stat = "identity", alpha = .6, colour = "black", position = "dodge") + 
	theme(legend.position = "none") +
	geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = "dodge", width = .4) +
	xlab("Architecture") + ylab("Mean Power Consumption (W)") + 
	theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      text         = element_text(size = 10),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x  = element_blank(),
		axis.text.y  = element_blank()) +
   facet_grid(. ~ arch)

ggsave("../images/POWER_MEAN_SD.pdf", graph, width = 8.6, height = 6, units = "cm")

power.all <- data.frame(
		data = intel[,ncol(intel)],
		arch = "Intel")

power.all <- rbind(power.all, data.frame(
		data = amd[,ncol(amd)],
		arch = "AMD"))

power.all <- rbind(power.all, data.frame(
		data = both[,ncol(both)],
		arch = "Both"))

graph <- ggplot(power.all, aes(x = arch, y = data, fill = arch)) + 
	geom_boxplot(alpha = .6) +
	xlab("Architecture") +
	ylab("Power Consumption (W)") +
	theme(
		legend.position = "none",
		strip.text.x = element_text(size = 10),
		axis.text.x  = element_text(size = 10),
		axis.text.y  = element_text(size = 10),
		axis.title.x = element_text(size = 10),
		axis.title.y = element_text(size = 10))

ggsave("../images/POWER_BOXPLOT.pdf", graph, width = 8.6, height = 6, units = "cm")

###########################
#
# Histogram 
# (dependent variable)
#

graph <- ggplot(power.all, aes(x = data / 100, y = ..count../1000, fill = arch)) + 
	 geom_histogram(alpha = .6) + 
	 xlab(expression("Power Consumption (W / "* 10^2*")")) +
	 ylab(expression("Frequency / "*10^3)) +
	 facet_grid(. ~ arch) +
	 theme(
		legend.position = "none",
		strip.text.x = element_text(size = 10),
		axis.text.x  = element_text(size = 10),
		axis.text.y  = element_text(size = 10),
		axis.title.x = element_text(size = 10),
		axis.title.y = element_text(size = 10))

ggsave("../images/POWER_HISTOGRAM.pdf", graph, width = 8.6, height = 6, units = "cm")

###########################
#
# Accuracy 
#
accur <- read.table("data/models.accuracy.csv", sep = ",", dec = ".", header = TRUE)
accur$arch <- factor(accur$arch, levels = c("AMD", "Intel", "Both"))

graph <- ggplot(accur, aes(x = methods, y = r2, fill = methods, color = methods)) +
	geom_bar(stat = "identity", alpha = .6) +
	xlab("Method") +
	scale_y_continuous(
		expression(R^2),
		labels = percent_format(),
		limits = c(.91, 1),
		oob = rescale_none) +
	theme(
		legend.position = "none",
		axis.title.y    = element_text(angle = 0,  size = 10),
		axis.title.x    = element_text(vjust = -.3, size = 10),
		text            = element_text(size = 10),
		title           = element_text(size = 10)) +
	facet_grid(. ~ arch)

ggsave("../images/R2.pdf", width = 15, units = "cm")