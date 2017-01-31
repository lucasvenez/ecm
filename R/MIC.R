require("ggplot2")
require("scales")
require("grid")

###########################
#
# Reading MIC data
#

#########
#
# AMD
#
amd.mic <- read.table("data/amd_mine.csv", sep=",", header = T)[2:3]

amd.y.var.list <- lapply(as.character(amd.mic[,1]), FUN = strsplit, split = "_")

amd.y.var <- c()

amd.category <- c()

for(i in 1:length(amd.y.var.list)) {
	
	tmp.list <- amd.y.var.list[[i]][[1]]
	
	amd.y.var <- c(amd.y.var, 
			paste(tmp.list[-1], collapse = "_"))
	
	amd.category <- c(amd.category, tmp.list[1])
}

amd.mic[1] <- amd.y.var

amd.mic <- cbind(amd.mic, 
		data.frame(category = amd.category))

amd.mic <-  # adding ARCH var with MIC = 0.00 for adjusting plot
		rbind(
				data.frame(Y.var = "ARCH", 
						MIC..strength. = NA, 
						category = "Architecture"), amd.mic)

amd.mic <- amd.mic[order(-amd.mic[,2]),]      

#########
#
# Intel
#
intel.mic <- read.table("data/intel_mine.csv",   sep=",", header = T)[2:3]

intel.y.var.list <- lapply(as.character(intel.mic[,1]), FUN = strsplit, split = "_")

intel.y.var <- c()

intel.category <- c()

for(i in 1:length(intel.y.var.list)) {
	
	tmp.list <- intel.y.var.list[[i]][[1]]
	
	intel.y.var <- c(intel.y.var, 
			paste(tmp.list[-1], collapse = "_"))
	
	intel.category <- c(intel.category, tmp.list[1])
}

intel.mic[1] <- intel.y.var

intel.mic <- cbind(intel.mic, 
		data.frame(category = intel.category))

intel.mic <-  # adding ARCH var with MIC = 0.00 for adjusting plot
		rbind(
				data.frame(Y.var = "ARCH", 
						MIC..strength. = NA, 
						category = "Architecture"), intel.mic)

intel.mic <- intel.mic[order(-intel.mic[,2]),]   

#########
#
# Both
#
both.mic <- 
		read.table("data/both_mine.csv",  
				sep=",", header = T)[2:3]

both.y.var.list <- 
		lapply(as.character(both.mic[,1]), 
				FUN = strsplit, split = "_")

both.y.var <- c()

both.category <- c()

for(i in 1:length(both.y.var.list)) {
	
	tmp.list <- both.y.var.list[[i]][[1]]
	
	if (length(tmp.list) == 1) {
		both.y.var <- c(both.y.var, "ARCH")
	} else {
		both.y.var <- c(both.y.var, 
				paste(tmp.list[-1], collapse = "_"))
	}
	
	if (tmp.list[1] != "ARCH") {
		both.category <- c(both.category, tmp.list[1])
	} else {
		both.category <- c(both.category, "Architecture")
	}
}

both.mic[1] <- both.y.var

both.mic <- cbind(
	both.mic, 
	data.frame(category = both.category))

both.mic <- both.mic[order(-both.mic[,2]),] 

#########
#
# Merge
#
all.mic <- rbind(
		cbind(intel.mic, data.frame(arch = "A1")),
		cbind(amd.mic,   data.frame(arch = "A2")),
		cbind(both.mic,  data.frame(arch = "Mix")))

all.mic$MIC..strength.[is.na(all.mic$MIC..strength.)] <- 0

all.mic$category <- factor(all.mic$category, levels = c("NETWORK", "DISK", "MEMORY", "CPU", "Architecture"))

rm(intel.mic, amd.mic, both.mic, 
   amd.category,  amd.y.var,  amd.y.var.list, 
   both.category, both.y.var, both.y.var.list,
   intel.category, intel.y.var, intel.y.var.list,
   i, tmp.list)

###########################
#
# Plotting MIC
#
graph <- ggplot(all.mic, 
                aes(x = Y.var, 
                    y = MIC..strength., 
                    colour = category,
                    fill = category)) + 
  geom_bar(stat = "identity", alpha = .6, size = 0.3) +
  ylim(0,1) + 
  scale_fill_hue(name    = "Category Legend", 
                 labels  = c("Network", "Disk", "Memory", "Processor", "Architecture")) +
  scale_colour_hue(guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) + ylab("Maximal Information Coefficient") +
  theme(legend.background  = element_rect(colour = "black", size = .2),
        panel.grid.major.y = element_line(size = .15),
        text               = element_text(size = 7),
        legend.key.size    = unit(8, "pt"),
        legend.position    = c(-0.25, .3),
        axis.text.x        = element_text(size = 6, angle = 45),
        axis.title.x       = element_text(vjust = 1)) + 
  coord_flip() + 
  facet_grid(. ~ arch) +
  geom_hline(
    data = data.frame(
      x = rep(.5, length(all.mic[,2])), 
      y = all.mic[,2]), 
    aes(yintercept=.125, xintercept=12), 
    alpha = .6)

ggsave("../images/MIC.png", graph, width = 16, height = 10, units = "cm")

rm(graph, all.mic)
