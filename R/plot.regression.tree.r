# setwd("/home/anacarla/Dropbox/Papers/Writing/FGCS/analysis/")

##############################
#
# Required Packages 
#
require(rpart)
require(rpart.plot)

##############################
#
# Loading data 
#
load("data/intel.mrt.rda")
load("data/amd.mrt.rda")
load("data/both.mrt.rda")

inch2cm <- function(x) x / 2.54

pdf("../images/RT_INTEL.pdf", width = inch2cm(5), height = inch2cm(5))

rpart.plot(intel.mrt, cex = .5)

dev.off()

pdf("../images/RT_AMD.pdf", width = inch2cm(8), height = inch2cm(5))

rpart.plot(amd.mrt, cex = .5)

dev.off()

pdf("../images/RT_BOTH.pdf", width = inch2cm(5), height = inch2cm(3))

rpart.plot(both.mrt, cex = .5)

dev.off()
