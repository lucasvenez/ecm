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
load("data/intel.mlr.rda")
load("data/amd.mlr.rda")
load("data/both.mlr.rda")

load("data/intel.mlp.rda")
load("data/amd.mlp.rda")
load("data/both.mlp.rda")

load("data/intel.rt.rda")
load("data/amd.rt.rda")
load("data/both.rt.rda")

intel <- read.table("data/intel.csv", sep = ",", dec = ".", header = TRUE)
amd   <- read.table("data/amd.csv",   sep = ",", dec = ".", header = TRUE)
both  <- read.table("data/both.csv",  sep = ",", dec = ".", header = TRUE)

###################################
#
# Comparing Observed vs. Predicted
#
###################################

legend <- c("Actual", "Estimate")

all <- rbind(
  cbind(
    data.frame(
      arch   = "A1", 
      method = "MLR"),
    rbind(
      data.frame(
        index = 1:length(intel$POWER_CONSUMPTION), 
        value = intel$POWER_CONSUMPTION, 
        type  = legend[1]),
      data.frame(
        index = 1:length(intel$POWER_CONSUMPTION), 
        value = as.numeric(predict(intel.mlr)), 
        type  = legend[2]))),
  cbind(
    data.frame(
      arch   = "A2", 
      method = "MLR"),
    rbind(
      data.frame(
        index = 1:length(amd$POWER_CONSUMPTION), 
        value = amd$POWER_CONSUMPTION, 
        type  = legend[1]),
      data.frame(
        index = 1:length(amd$POWER_CONSUMPTION), 
        value = as.numeric(predict(amd.mlr)), 
        type  = legend[2]))),
  cbind(
    data.frame(
      arch   = "Mix", 
      method = "MLR"),
    rbind(
      data.frame(
        index = 1:length(both$POWER_CONSUMPTION), 
        value = both$POWER_CONSUMPTION, 
        type  = legend[1]),
      data.frame(
        index = 1:length(both$POWER_CONSUMPTION), 
        value = as.numeric(predict(both.mlr)), 
        type  = legend[2]))),
  cbind(
    data.frame(
      arch   = "A1", 
      method = "RET"),
    rbind(
      data.frame(
        index = 1:length(intel$POWER_CONSUMPTION), 
        value = intel$POWER_CONSUMPTION, 
        type  = legend[1]),
      data.frame(
        index = 1:length(intel$POWER_CONSUMPTION), 
        value = as.numeric(predict(intel.rt)), 
        type  = legend[2]))),
  cbind(
    data.frame(
      arch   = "A2", 
      method = "RET"),
    rbind(
      data.frame(
        index = 1:length(amd$POWER_CONSUMPTION), 
        value = amd$POWER_CONSUMPTION, 
        type  = legend[1]),
      data.frame(
        index = 1:length(amd$POWER_CONSUMPTION), 
        value = as.numeric(predict(amd.rt)), 
        type  = legend[2]))),
  cbind(
    data.frame(
      arch   = "Mix", 
      method = "RET"),
    rbind(
      data.frame(
        index = 1:length(both$POWER_CONSUMPTION), 
        value = both$POWER_CONSUMPTION, 
        type  = legend[1]),
      data.frame(
        index = 1:length(both$POWER_CONSUMPTION), 
        value = as.numeric(predict(both.rt)), 
        type  = legend[2]))),
  cbind(
    data.frame(
      arch   = "A1", 
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
      arch   = "A2", 
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
      arch   = "Mix", 
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
        type  = legend[2])))
)

rm(amd, both, intel, 
   amd.mlp, amd.mlr, amd.rt, 
   intel.mlp, intel.mlr, intel.rt, 
   both.mlp, both.mlr, both.rt)

graph <- 
ggplot(all, aes(x = index, y = value, colour = type)) + 
  geom_line(alpha = .6) + 
  xlab("Sample") +
  ylab("Energy Consumption (W)") +
  scale_color_hue("Legend") +
  facet_grid(arch ~ method, scales = "free_y") +
  theme(
    legend.position   = c(.92, .5),
    legend.background = element_rect(fill=alpha('white', 0.6)),
    text              = element_text(size=10),
    axis.title        = element_text(size=10),
    axis.text         = element_blank(),
    axis.ticks        = element_blank()
  ) 

rm(legend, all)

ggsave("../images/MODELS_ACCURACY.png", graph, units = "cm", width = 16, height = 10, limitsize = T)

rm(graph)