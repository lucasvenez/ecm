##############################
#
# Required Packages 
#
require("ggplot2")
require("RSNNS")
require("scales")
require(grid)
require(graphics)

##############################
# Loading data 
##############################

intel <- read.table("data/intel.csv", sep = ",", dec = ".", header = TRUE)
amd   <- read.table("data/amd.csv",   sep = ",", dec = ".", header = TRUE)

intel <- cbind(intel[ncol(intel)], data.frame(arch = "A1"))
amd   <- cbind(amd[ncol(amd)], data.frame(arch = "A2"))

both  <- rbind(intel, amd)
both$arch <- "Mix"

##############################
#
# Power Consumption Histogram 
#
all <- rbind(intel, amd, both)

graph <- ggplot(all, aes(x = POWER_CONSUMPTION, fill = arch, color = arch)) +
  geom_histogram(aes(y = ..count../1000), binwidth = 2.5, alpha = .6, size = 0) +
  scale_fill_hue("Architecture") +
  scale_colour_hue("Architecture") +
  xlab("Power Consumption (W)") +
  ylab(expression("Frequency ("*E10^{-3}*")")) +
  facet_grid(. ~ arch) +
  theme(
    legend.position = "none",
    axis.text       = element_text(size = 6),
    text            = element_text(size = 8),
    axis.title.x    = element_text(vjust = -.5))

ggsave("../images/POWER_HISTOGRAM.pdf", graph, width = 8.25, height = 6, units = "cm")