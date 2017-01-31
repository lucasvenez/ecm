setwd("/home/anacarla/Dropbox/Papers/Writing/FGCS/analysis/")

##############################
#
# Required Packages 
#
require("ggplot2")
require("RSNNS")
require("scales")

##############################
#
# Loading data 
#
load("data/intel.glm.rda")
load("data/amd.glm.rda")
load("data/both.glm.rda")

load("data/intel.mlp.rda")
load("data/amd.mlp.rda")
load("data/both.mlp.rda")

load("data/intel.mrt.rda")
load("data/amd.mrt.rda")
load("data/both.mrt.rda")

intel <- read.table("data/intel.csv", sep = ",", dec = ".", header = TRUE)
amd   <- read.table("data/amd.csv",   sep = ",", dec = ".", header = TRUE)
both  <- read.table("data/both.csv",  sep = ",", dec = ".", header = TRUE)

##############################
#
# Residual Analysis
#
all <- rbind(
	data.frame(
		arch         = "Intel",
		method       = "GLM", 
		residual     = intel$POWER_CONSUMPTION - predict(intel.glm),
		fitted.value = predict(intel.glm)),

	data.frame(
		arch         = "AMD",
		method       = "GLM", 
		residual     = amd$POWER_CONSUMPTION - predict(amd.glm),
		fitted.value = predict(amd.glm)),

	data.frame(
		arch         = "Both",
		method       = "GLM", 
		residual     = both$POWER_CONSUMPTION - predict(both.glm),
		fitted.value = predict(both.glm)),

	data.frame(
		arch         = "Intel",
		method       = "MLP", 
		residual     = intel$POWER_CONSUMPTION - denormalizeData(as.numeric(predict(intel.mlp)), getNormParameters(normalizeData(intel$POWER_CONSUMPTION, type = "0_1"))),
		fitted.value = predict(intel.glm)),

	data.frame(
		arch         = "AMD",
		method       = "MLP", 
		residual     = amd$POWER_CONSUMPTION - denormalizeData(as.numeric(predict(amd.mlp)), getNormParameters(normalizeData(amd$POWER_CONSUMPTION, type = "0_1"))),
		fitted.value = predict(amd.glm)),

	data.frame(
		arch         = "Both",
		method       = "MLP", 
		residual     = both$POWER_CONSUMPTION - denormalizeData(as.numeric(predict(both.mlp)), getNormParameters(normalizeData(both$POWER_CONSUMPTION, type = "0_1"))),
		fitted.value = predict(both.glm)),

	data.frame(
		arch         = "Intel",
		method       = "MRT", 
		residual     = intel$POWER_CONSUMPTION - predict(intel.mrt),
		fitted.value = predict(intel.glm)),

	data.frame(
		arch         = "AMD",
		method       = "MRT", 
		residual     = amd$POWER_CONSUMPTION - predict(amd.mrt),
		fitted.value = predict(amd.glm)),

	data.frame(
		arch         = "Both",
		method       = "MRT", 
		residual     = both$POWER_CONSUMPTION - predict(both.mrt),
		fitted.value = predict(both.glm))
)

graph <- ggplot(all[all$method=="GLM",], aes(x = fitted.value, y = residual)) + geom_point(alpha = .6) +
  geom_smooth() +
  theme(text = element_text(size = 10)) +
	facet_grid(. ~ arch, scales = "free_x") +
	ylab("Residuals") + xlab("Fitted values")

ggsave("../images/RESIDUAL_ANALYSIS_GLM.png", graph, width = 9, height = 6.5, units = "cm")

graph <- ggplot(all[all$method=="MLP",], aes(x = fitted.value, y = residual)) + 
  geom_point(alpha = .6) + geom_smooth() +
  facet_grid(. ~ arch, scales = "free_x") +
  theme(text = element_text(size = 10)) +
  ylab("Residuals") + xlab("Fitted values")

ggsave("../images/RESIDUAL_ANALYSIS_MLP.png", graph, width = 9, height = 6.5, units = "cm")

graph <- ggplot(all[all$method=="MRT",], aes(x = fitted.value, y = residual)) + geom_point(alpha = .6) +
  geom_smooth() + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45)) +
  facet_grid(. ~ arch, scales = "free_x") +
  ylab("Residuals") + xlab("Fitted values")

ggsave("../images/RESIDUAL_ANALYSIS_MRT.png", graph, width = 9, height = 6.5, units = "cm")

##############################
#
# Model Precision Comparison 
#

legend <- c("Observed", "Forecast")

all <- rbind(
	cbind(
		data.frame(
			arch   = "Intel", 
			method = "GLR"),
		rbind(
			data.frame(
				index = 1:length(intel$POWER_CONSUMPTION), 
				value = intel$POWER_CONSUMPTION, 
				type  = legend[1]),
			data.frame(
				index = 1:length(intel$POWER_CONSUMPTION), 
				value = as.numeric(predict(intel.glm)), 
				type  = legend[2]))),
	cbind(
		data.frame(
				arch   = "AMD", 
				method = "GLR"),
		rbind(
				data.frame(
						index = 1:length(amd$POWER_CONSUMPTION), 
						value = amd$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(amd$POWER_CONSUMPTION), 
						value = as.numeric(predict(amd.glm)), 
						type  = legend[2]))),
	cbind(
		data.frame(
				arch   = "Both", 
				method = "GLR"),
		rbind(
				data.frame(
						index = 1:length(both$POWER_CONSUMPTION), 
						value = both$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(both$POWER_CONSUMPTION), 
						value = as.numeric(predict(both.glm)), 
						type  = legend[2]))),
	
	cbind(
		data.frame(
				arch   = "Intel", 
				method = "MLP"),
		rbind(
				data.frame(
						index = 1:length(intel$POWER_CONSUMPTION), 
						value = intel$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(intel$POWER_CONSUMPTION), 
						value =
							denormalizeData(as.numeric(predict(intel.mlp)),
							getNormParameters(normalizeData(intel$POWER_CONSUMPTION, type = "0_1"))), 
						type  = legend[2]))),
	cbind(
		data.frame(
				arch   = "AMD", 
				method = "MLP"),
		rbind(
				data.frame(
						index = 1:length(amd$POWER_CONSUMPTION), 
						value = amd$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(amd$POWER_CONSUMPTION), 
						value = 
							denormalizeData(as.numeric(predict(amd.mlp)),
							getNormParameters(normalizeData(amd$POWER_CONSUMPTION, type = "0_1"))), 
						type  = legend[2]))),
	cbind(
		data.frame(
				arch   = "Both", 
				method = "MLP"),
		rbind(
				data.frame(
						index = 1:length(both$POWER_CONSUMPTION), 
						value = both$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(both$POWER_CONSUMPTION), 
						value = 
							denormalizeData(as.numeric(predict(both.mlp)), 
							getNormParameters(normalizeData(both$POWER_CONSUMPTION, type = "0_1"))), 
						type  = legend[2]))),

	cbind(
		data.frame(
				arch   = "Intel", 
				method = "MRT"),
		rbind(
				data.frame(
						index = 1:length(intel$POWER_CONSUMPTION), 
						value = intel$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(intel$POWER_CONSUMPTION), 
						value = as.numeric(predict(intel.mrt)), 
						type  = legend[2]))),
	cbind(
		data.frame(
				arch   = "AMD", 
				method = "MRT"),
		rbind(
				data.frame(
						index = 1:length(amd$POWER_CONSUMPTION), 
						value = amd$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(amd$POWER_CONSUMPTION), 
						value = as.numeric(predict(amd.mrt)), 
						type  = legend[2]))),
	cbind(
		data.frame(
				arch   = "Both", 
				method = "MRT"),
		rbind(
				data.frame(
						index = 1:length(both$POWER_CONSUMPTION), 
						value = both$POWER_CONSUMPTION, 
						type  = legend[1]),
				data.frame(
						index = 1:length(both$POWER_CONSUMPTION), 
						value = as.numeric(predict(both.mrt)), 
						type  = legend[2])))
)

graph <- ggplot(all, aes(x = index, y = value, colour = type)) + 
	geom_line(alpha = .6) + 
	xlab("Sample") +
	ylab("Energy Consumption (W)") +
   scale_color_hue("Legend") +
   facet_grid(arch ~ method, scales = "free_y") +
	theme(
      legend.position   = c(.92, .5),
      legend.background = element_rect(fill=alpha('white', 0.6)),
      text              = element_text(size=10),
      axis.title        = element_text(size=8),
		axis.text         = element_blank(),
		axis.ticks        = element_blank()
	) 

ggsave("../images/MODELS_ACCURACY.png", graph, units = "cm", width = 16, height = 10, limitsize = T)

##############################
#
# Power Consumption Histogram 
#
all <- rbind(
		cbind(intel[ncol(intel)], data.frame(arch = "Intel")),
		cbind(amd[ncol(amd)], data.frame(arch = "AMD")))

graph <- ggplot(all, aes(x = POWER_CONSUMPTION, fill = arch, color = arch)) +
	geom_histogram(aes(y = ..count../1000), binwidth = 2.5, alpha = .6, size = 0) +
	#scale_y_continuous(labels = percent_format()) +
	scale_fill_hue("Architecture") +
	scale_colour_hue("Architecture") +
	xlab("Power Consumption (W)") +
	ylab(expression("Frequency ("*10^3*")")) +
	facet_grid(. ~ arch) +
	theme(
		legend.position = "none",
		axis.title.x = element_text(vjust = -.5))

ggsave("../images/POWER_HISTOGRAM.pdf", graph,
		width = 8.6, height = 6, units = "cm")

all <- rbind(all,
		cbind(both[ncol(both)], data.frame(arch = "Both")))

graph <- ggplot(all) + 
	geom_boxplot(
		aes(x = 1, y = POWER_CONSUMPTION, fill = arch),
		size = .4) +
	facet_grid(. ~ arch) +
	theme(
		legend.position = "none",
		axis.text.x  = element_blank(),
		axis.ticks.x = element_blank()) +
	xlab("Architecture") + ylab("Power Consumption")
	
ggsave("../images/POWER_BOXPLOT.pdf", graph,
			width = 20, height = 10, units = "cm")

######################################################################################

d1 <- data.frame(
      arch = c("AMD", "Intel", "Both"), 
      mse = c(
            0.0011 * (max(amd$POWER_CONSUMPTION) - min(amd$POWER_CONSUMPTION)), 
            0.0010 * (max(intel$POWER_CONSUMPTION) - min(intel$POWER_CONSUMPTION)), 
            0.0001 * (max(both$POWER_CONSUMPTION) - min(both$POWER_CONSUMPTION))))

graph <- ggplot(d1, aes(x = 1, y = mse, fill = arch)) + 
            geom_bar(stat = "identity", colour = "black", alpha = .6, position = "dodge") +
            xlab("Architecture") +
            ylab("Mean Squared Error (W)") +
            theme(
               legend.position    = "none",
               text               = element_text(size = 10),
               axis.text.x        = element_blank(),
               axis.ticks.x       = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank()) +
            facet_grid(. ~ arch)

ggsave("../images/MSE.pdf", graph, units = "cm", width = 8, height = 7)