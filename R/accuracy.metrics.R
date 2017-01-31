#
# SE  - Squared Error
#

se <- function(y, yhat) (y - yhat)^2

se.mean  <- function(y, yhat) mean(se(y, yhat), na.rm = T)

se.var   <- function(y, yhat) var(se(y, yhat), na.rm = T)

se.sd    <- function(y, yhat) sd(se(y, yhat), na.rm = T)

#
# AE  - Absolute Error
#

ae <- function(y, yhat) abs(y - yhat)

ae.mean <- function(y, yhat) mean(ae(y, yhat), na.rm = T)

ae.var  <- function(y, yhat) var(ae(y, yhat), na.rm = T)

ae.sd   <- function(y, yhat) sd(ae(y, yhat), na.rm = T)

#
# PE  - Percentage Error
#

pe <- function(y, yhat) (y - yhat)/y

pe.mean <- function(y, yhat) mean(pe(y, yhat), na.rm = T)

pe.var <- function(y, yhat) var(pe(y, yhat), na.rm = T)

pe.sd <- function(y, yhat) sd(pe(y, yhat), na.rm = T)

#
# APE - Absolute Percentage Error
#

ape      <- function(y, yhat) abs((y-yhat)/y)

ape.mean <- function(y, yhat) mean(ape(y, yhat), na.rm = T) 

ape.var  <- function(y, yhat) var(ape(y, yhat), na.rm = T) 

ape.sd   <- function(y, yhat) sd(ape(y, yhat), na.rm = T) 

#
# ASE - Absolute Scaled Error
#

ase <- function(y, yhat) abs(y - yhat) / mean(abs(y[2:length(y)] - y[1:(length(y) - 1)]))
  
ase.mean <- function(y, yhat) mean(ase(y, yhat), na.rm = T)

ase.var <- function(y, yhat) var(ase(y, yhat), na.rm = T)

ase.sd <- function(y, yhat) sd(ase(y, yhat), na.rm = T)