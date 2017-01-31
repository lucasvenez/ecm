# modifying workspace
setwd("/home/anacarla/Dropbox/Papers/Writing/FGCS/analysis/")

# loading required packages
require(RSNNS)
require(ggplot2)
require(grid)
require(graphics)
require(scales)

######################################
# MLP
######################################

# loading multilayer perceptrons
load("data/intel.mlp.rda")
load("data/amd.mlp.rda")
load("data/both.mlp.rda")

##
# getting MLP weights and insert into a list
weights <- list(
   weightMatrix(intel.mlp),
   weightMatrix(amd.mlp),
   weightMatrix(both.mlp))

architecture <- list(
  extractNetInfo(intel.mlp),
  extractNetInfo(amd.mlp),
  extractNetInfo(both.mlp))

names(weights) <- c("A1", "A2", "Mix")

# converting to data.frame containing the column source, target, weight, and arch

r <- data.frame(source = c(), target = c(), id = c(), category = c(), weight = c(), arch = c())

for (l in 1:length(weights)) {
   o <- length((architecture[[l]])$unitDefinitions$unitName)
   
   r <- rbind(r, 
              data.frame(
                source = "Bias",
                target = (architecture[[l]])$unitDefinitions$unitName,
                id = 1:o,
                category = "Bias",
                weight = (architecture[[l]])$unitDefinitions$unitBias,
                arch = names(weights)[l]))
   
   for (i in 1:nrow(weights[[l]])) {
     
      for (j in 1:ncol(weights[[l]])) {
         
        if ((weights[[l]])[i,j] != 0) {
          
          s <- NA
          t <- NA
          
          if (grepl("Input", row.names(weights[[l]])[i])) 
            s <- "I"
          else if (grepl("Hidden", row.names(weights[[l]])[i])) 
            s <- paste("H", substr(row.names(weights[[l]])[i], 8, 8), sep = "")
          
          if (grepl("Hidden", colnames(weights[[l]])[j])) 
            t <- paste("H", substr(colnames(weights[[l]])[j], 8, 8), sep = "")
          else if (grepl("Output", colnames(weights[[l]])[j])) 
            t <- "O"
          
          cat <- paste(s, t, sep = "-")
          o = o + 1;
          r <- rbind(r, 
                  data.frame(
                    source   = row.names(weights[[l]])[i], 
                    target   = colnames(weights[[l]])[j],
                    id       = o,
                    category = cat,
                    weight   = (weights[[l]])[i,j], 
                    arch     = names(weights)[l]))
        }
      }
   }
}

rm(l, i, j, weights)

graph <- ggplot(r, aes(x = id, y = weight, fill = category, colour = category)) +
  geom_bar(stat = "identity", alpha = .6, position = "dodge") +
  xlab("Connection") +
  ylab("Weight") +
  scale_fill_hue(name = "Connections",
      labels = c(
          "Bias", 
          "IL-HL#1", 
          "HL#1-HL#2", 
          "HL#2-HL#3", 
          "HL#3-OL")) +
  scale_colour_hue(guide = "none") +
  theme(
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.background  = element_rect(colour = "black", size = .2),
    legend.key.size    = unit(8, "pt"),
    legend.position    = c(.85, .20),
    text               = element_text(size = 9)) +
  facet_grid( ~ arch, scales = "free_x")

ggsave("../images/MLP-WEIGHTS.png", graph, width = 18, height = 7, units = "cm")

rm(intel.mlp, amd.mlp, architecture, both.mlp, graph)

######################################
# MLR
######################################

# loading MLR
load("data/intel.mlr.rda")
load("data/amd.mlr.rda")
load("data/both.mlr.rda")

##
# Functions for hue colors
colours <- function(n=3, h=c(0, 360) +15) {
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

##
# Function scientific notation
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scientific_format(digits = 1)(x)))
}

l <- list(intel.mlr, amd.mlr, both.mlr)

for (i in 1:3) {

  l[[i]]$coefficients <- l[[i]]$coefficients[!is.na(l[[i]]$coefficients)]
  
  coeff <- data.frame(var         = names(l[[i]]$coefficients[-1]), 
                      index       = 1:length(l[[i]]$coefficients[-1]),
                      coefficient = as.numeric(l[[i]]$coefficients[-1]),
                      arch        = c("A1", "A2", "Mix")[i])
  
  intersect <- as.numeric(l[[i]]$coefficients[1])
  
  xlab <- if(i == 3) xlab("Coefficients") else xlab(NULL)
  
  graph <- 
    ggplot(coeff, aes(x = index, y = factor(coefficient), colour = arch, fill = arch)) +
    geom_bar(stat = "identity", alpha = .6) +
    #annotate("text", x = c(4, 3, 4.5)[i], y = c(19.5, 15.5, 22.5)[i], parse = T, size = 3,
             #label = paste("alpha ==", scientific_10(intersect))) +
    xlab + 
    ylab(NULL) +
    scale_y_discrete(labels = scientific_10(sort(coeff$coefficient))) +
    scale_colour_manual(values = c(colours()[i])) +
    scale_fill_manual(values = c(colours()[i])) +
    theme(
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.text.y        = element_text(size = 6),
      legend.position    = "none",
      text               = element_text(size = 8),
      plot.margin = unit(c(0,0,0,0), "cm")) +
    facet_grid( ~ arch, scales = "free")
  
  ggsave(paste("../images/MLR-COEFF", i, ".png", sep = ""), graph, width = 8.25, height = ifelse(i == 5, 4, 5.2), units = "cm")
}